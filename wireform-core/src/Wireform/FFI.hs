{- | FFI bindings to the SWAR\/SIMD-optimized C decoding primitives
in @cbits\/fast_decode.c@.  Shared across all wireform format packages.

These supplement the pure-Haskell decoders for hot paths where C
can leverage SWAR (SIMD Within A Register) for batch operations on
packed fields.

C primitives and what they accelerate:

* 'countPackedVarints' \/ 'packedAllSingleByte' — packed repeated field pre-scan
* 'validateUtf8SWAR' — UTF-8 validation (8 bytes at a time)
* 'decodeVarintSWAR' — branchless varint decode (8-byte load + CTZ)
* 'relocatePageBoundary' — safe 8-byte overread padding
* 'encodeLengthDelimitedC' \/ 'encodeVarintFieldC' \/ 'encodeBoolFieldC' — field encode
* 'findNul' \/ 'findNulBS' — NUL byte scanning for BSON cstrings
* 'isAscii' \/ 'isAsciiBS' — SIMD ASCII check (general purpose)
* 'decodeTextFast' — text decode via @text@'s simdutf
* 'findJsonEscape' \/ 'escapeJSONStringBS' — JSON string escaping
* 'skipWhitespace' \/ 'skipWhitespaceBS' — EDN whitespace skipping
* 'compareBounds' \/ 'compareBoundsBS' — Iceberg partition bounds comparison
* 'validateArrowBuffers' — Arrow IPC buffer offset validation
* @read\/writeBE\/LE@ — endianness conversion helpers

Widths: every length and offset crosses the FFI at its full width, so a
'ByteString' or encode buffer over 2 GiB is not silently truncated. Byte
counts, capacities and reported counts are 'CSize' (\'size_t\'); positions
into a buffer, write cursors and scanner results are 'CPtrdiff'
(\'ptrdiff_t\'), which also carries the @-1@ / @len@ miss conventions.
Only genuinely small values (booleans, byte counts like bytes consumed,
the 32-bit bounds bitmask) stay 'CInt'.
-}
module Wireform.FFI (
  -- * Packed varint helpers
  countPackedVarints,
  packedAllSingleByte,

  -- * SWAR UTF-8 validation
  validateUtf8SWAR,

  -- * SWAR varint decode
  decodeVarintSWAR,

  -- * Page boundary relocation
  relocatePageBoundary,

  -- * C-native encode primitives
  encodeLengthDelimitedC,
  encodeVarintFieldC,
  encodeBoolFieldC,

  -- * SIMD NUL scanner (for BSON cstrings)
  findNul,
  findNulBS,

  -- * SIMD generic byte scanner (CSV delimiters, NDJSON

  -- newlines, XML structural chars, …)
  findByte,
  findByteBS,

  -- * SIMD 4-byte repeating-key XOR (WebSocket masking,

  --   RFC 6455 sec 5.3)
  xorRepeatingKey,
  xorRepeatingKeyBS,

  -- * Thread-local xoshiro256++ PRNG
  fastRandomWord64,

  -- * SIMD ASCII check (general purpose)
  isAscii,
  isAsciiBS,

  -- * Text decoding (via text's simdutf)
  decodeTextFast,

  -- * SIMD JSON escape scanner (for all JSON output paths)
  findJsonEscape,
  findJsonEscapeBS,
  escapeJSONStringBS,
  escapeJSONText,

  -- * SIMD EDN whitespace skipper
  skipWhitespace,
  skipWhitespaceBS,

  -- * SIMD Iceberg bounds comparison
  compareBounds,
  compareBoundsBS,

  -- * SIMD Arrow IPC buffer validation
  validateArrowBuffers,

  -- * Endianness helpers (Haskell-side, single MOV + BSWAP)
  readBE16H,
  readBE32H,
  readBE64H,
  writeBE16H,
  writeBE32H,
  writeBE64H,
  readLE16H,
  readLE32H,
  readLE64H,
  writeLE16H,
  writeLE32H,
  writeLE64H,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32, Word64, Word8, byteSwap16, byteSwap32, byteSwap64)
import Foreign.C.Types (CInt (..), CPtrdiff (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Alloc qualified
import Foreign.Marshal.Array qualified
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.Storable qualified
import System.IO.Unsafe (unsafePerformIO)
import Wireform.Builder qualified as B


-- (byte count, varint count): both can exceed INT_MAX.
foreign import ccall unsafe "hs_proto_count_packed_varints"
  c_count_packed_varints :: Ptr () -> CSize -> CSize


-- (byte count), boolean result.
foreign import ccall unsafe "hs_proto_packed_all_single_byte"
  c_packed_all_single_byte :: Ptr () -> CSize -> CInt


-- (byte count), boolean result.
foreign import ccall unsafe "hs_proto_validate_utf8_fast"
  c_validate_utf8_fast :: Ptr () -> CSize -> CInt


-- (position), bytes consumed (1..8, or 0 on overflow).
foreign import ccall unsafe "hs_proto_decode_varint_swar"
  c_decode_varint_swar :: Ptr () -> CPtrdiff -> Ptr Word64 -> CInt


-- (byte count, destination capacity), 1/0/-1 decision.
foreign import ccall unsafe "hs_proto_relocate_page_boundary"
  c_relocate_page_boundary :: Ptr () -> CSize -> Ptr () -> CSize -> CInt


-- (write cursor, byte count), bytes written.
foreign import ccall unsafe "hs_proto_encode_length_delimited"
  c_encode_length_delimited :: Ptr () -> CSize -> Word8 -> Ptr () -> CSize -> CSize


-- (write cursor), bytes written.
foreign import ccall unsafe "hs_proto_encode_varint_field"
  c_encode_varint_field :: Ptr () -> CSize -> Word8 -> Word64 -> CSize


-- (write cursor), bytes written.
foreign import ccall unsafe "hs_proto_encode_bool_field"
  c_encode_bool_field :: Ptr () -> CSize -> Word8 -> CInt -> CSize


{- | Count the number of varints in a packed buffer using SWAR.
Each byte with its high bit clear terminates one varint.

Both the length and the count cross the FFI at full width ('CSize'), so a
packed run over 2 GiB is counted whole, even when its count passes
'Prelude.maxBound' @::@ 'CInt'.
-}
countPackedVarints :: ByteString -> Int
countPackedVarints bs = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! fromIntegral (c_count_packed_varints (castPtr ptr) (fromIntegral len))
{-# INLINE countPackedVarints #-}


{- | Check if every varint in a packed buffer is a single byte (0x00-0x7F).
When true, the buffer can be zero-copy decoded by reading bytes directly.
-}
packedAllSingleByte :: ByteString -> Bool
packedAllSingleByte bs = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! c_packed_all_single_byte (castPtr ptr) (fromIntegral len) /= 0
{-# INLINE packedAllSingleByte #-}


{- | Encode a length-delimited field (tag + length varint + data) in one C call.

@off@ is a non-negative write cursor and @dataLen@ a byte count; both are
passed as 'CSize' (the C @off@/return are \'size_t\'), so a direct-encode
buffer or a single field over 2 GiB is written whole.
-}
encodeLengthDelimitedC :: Ptr Word8 -> Int -> Word8 -> Ptr Word8 -> Int -> IO Int
encodeLengthDelimitedC buf off tag dataPtr dataLen =
  pure $!
    fromIntegral $
      c_encode_length_delimited (castPtr buf) (fromIntegral off) tag (castPtr dataPtr) (fromIntegral dataLen)
{-# INLINE encodeLengthDelimitedC #-}


-- | Encode a varint field (tag + varint) in one C call.
encodeVarintFieldC :: Ptr Word8 -> Int -> Word8 -> Word64 -> IO Int
encodeVarintFieldC buf off tag val =
  pure $!
    fromIntegral $
      c_encode_varint_field (castPtr buf) (fromIntegral off) tag val
{-# INLINE encodeVarintFieldC #-}


-- | Encode a bool field (tag + 0/1) in one C call. Always 2 bytes.
encodeBoolFieldC :: Ptr Word8 -> Int -> Word8 -> Bool -> IO Int
encodeBoolFieldC buf off tag val =
  pure $!
    fromIntegral $
      c_encode_bool_field (castPtr buf) (fromIntegral off) tag (if val then 1 else 0)
{-# INLINE encodeBoolFieldC #-}


{- | Validate UTF-8 using SWAR ASCII fast path.
Processes 8 bytes at a time for ASCII (the common case), only entering
the full multibyte validator when non-ASCII bytes are found.
-}
validateUtf8SWAR :: ByteString -> Bool
validateUtf8SWAR bs = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! c_validate_utf8_fast (castPtr ptr) (fromIntegral len) /= 0
{-# INLINE validateUtf8SWAR #-}


{- | SWAR branchless varint decode.

Ported from hyperpb's number: block in vm/run.go.
Loads 8 bytes, XORs sign bits, uses CTZ to find the terminator,
masks and compacts in one shot. Zero per-byte branches.

REQUIRES: at least 8 readable bytes from the given offset.
Returns (value, bytesConsumed) or Nothing on overflow.
-}
decodeVarintSWAR :: ByteString -> Int -> Maybe (Word64, Int)
decodeVarintSWAR bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, _len) ->
    alloca $ \outPtr -> do
      let consumed = c_decode_varint_swar (castPtr ptr) (fromIntegral off) outPtr
      if consumed == 0
        then pure Nothing
        else do
          val <- Foreign.Storable.peek outPtr
          pure (Just (val, fromIntegral consumed))
{-# INLINE decodeVarintSWAR #-}


{- | Pad a buffer for safe 8-byte overreads at any position.

hyperpb's RelocatePageBoundary: if the end of the buffer is within
7 bytes of a page boundary, returns a copy with 7 bytes of zero
padding. Otherwise returns the original buffer unchanged.

The padding zeros act as varint terminators (byte < 0x80), making
SWAR 8-byte loads safe at every position.
-}
relocatePageBoundary :: ByteString -> ByteString
relocatePageBoundary bs
  | BS.null bs = bs
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
        let outLen = len + 7
        outBuf <- Foreign.Marshal.Array.mallocArray outLen
        let result =
              c_relocate_page_boundary
                (castPtr ptr)
                (fromIntegral len)
                (castPtr outBuf)
                (fromIntegral outLen)
        if result == 1
          then do
            -- packCStringLen copies; the scratch buffer is ours to free.
            copied <- BS.packCStringLen (outBuf, len)
            Foreign.Marshal.Alloc.free outBuf
            pure copied
          else do
            Foreign.Marshal.Alloc.free outBuf
            pure bs
{-# INLINE relocatePageBoundary #-}


------------------------------------------------------------------------
-- SIMD NUL scanner
------------------------------------------------------------------------

-- (start position, buffer length): both indices, so CPtrdiff; -1 on a miss.
foreign import ccall unsafe "hs_proto_find_nul"
  c_find_nul :: Ptr () -> CPtrdiff -> CPtrdiff -> CPtrdiff


{- | Find the first NUL byte starting at the given offset within a raw pointer.
Returns @Just idx@ (absolute offset from buf start) or @Nothing@.
-}
findNul :: Ptr Word8 -> Int -> Int -> Maybe Int
findNul !ptr !offset !len =
  let !r = c_find_nul (castPtr ptr) (fromIntegral offset) (fromIntegral len)
  in if r < 0 then Nothing else Just (fromIntegral r)
{-# INLINE findNul #-}


{- | Find the first NUL byte in a 'ByteString' starting at the given offset.
Returns @Just idx@ (offset into the ByteString) or @Nothing@.
-}
findNulBS :: ByteString -> Int -> Maybe Int
findNulBS bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! findNul (castPtr ptr) off len
{-# INLINE findNulBS #-}


{- | SIMD @memchr@-analog: 16-byte-at-a-time scan for the first
occurrence of @target@ in @buf[offset..len)@. Returns the
absolute offset (0-based from the start of @buf@) of the
first match, or @len@ if no match was found.

Both @offset@ and @len@ are indices into @buf@ and cross the FFI as
'CPtrdiff', so a 'ByteString' or scan range over 2 GiB is scanned whole
and a miss reports the real @len@ instead of a wrapped value.

Callers that prefer @Maybe Int@ semantics should wrap the
result; the @len@-on-miss convention matches the existing
CSV / NDJSON / XML call sites that reused the primitive
before it was lifted here.
-}
foreign import ccall unsafe "hs_find_byte"
  c_find_byte :: Ptr () -> CPtrdiff -> CPtrdiff -> Word8 -> CPtrdiff


findByte :: Ptr Word8 -> Int -> Int -> Word8 -> Int
findByte !ptr !offset !len !target =
  fromIntegral
    ( c_find_byte
        (castPtr ptr)
        (fromIntegral offset)
        (fromIntegral len)
        target
    )
{-# INLINE findByte #-}


-- | 'findByte' variant that takes a 'ByteString' directly.
findByteBS :: ByteString -> Int -> Word8 -> Int
findByteBS bs off target = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! findByte (castPtr ptr) off len target
{-# INLINE findByteBS #-}


------------------------------------------------------------------------
-- 4-byte repeating-key XOR (WebSocket masking)
------------------------------------------------------------------------

-- (byte count), in place.
foreign import ccall unsafe "hs_ws_mask"
  c_ws_mask :: Ptr Word8 -> CSize -> Word32 -> IO ()


{- | In-place XOR of the 4-byte repeating @key@ over @buf[0..len)@.

The key is interpreted in network byte order: byte 0 of the
key sits in the high byte of @key@.  This matches RFC 6455 sec
5.3's masking-key layout.

Used by 'wireform-websocket' for frame masking; exposed
generically because the same primitive shows up in any protocol
that masks with a periodic 4-byte key.  Implemented in
@cbits\/fast_scan.c@ as an SSE2 (via simde) loop processing
16 bytes per iteration with a tiled mask vector.
-}
xorRepeatingKey :: Ptr Word8 -> Int -> Word32 -> IO ()
xorRepeatingKey ptr len key = c_ws_mask ptr (fromIntegral len) key
{-# INLINE xorRepeatingKey #-}


{- | 'ByteString' variant: XOR the 4-byte repeating key in-place
over the bytes the 'ByteString' references.  The 'ByteString'
itself is shared with the caller, so the mutation is visible to
everyone else holding the same 'ByteString'.  Callers must own
the backing memory exclusively at this point.
-}
xorRepeatingKeyBS :: ByteString -> Word32 -> IO ()
xorRepeatingKeyBS bs key = BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
  c_ws_mask (castPtr ptr) (fromIntegral len) key
{-# INLINE xorRepeatingKeyBS #-}


------------------------------------------------------------------------
-- Thread-local xoshiro256++ PRNG
------------------------------------------------------------------------

foreign import ccall unsafe "hs_xoshiro256pp_next"
  c_xoshiro256pp_next :: IO Word64


{- | Pull a non-cryptographically-random 'Word64' from the calling
OS thread's xoshiro256++ generator.

State is per-OS-thread (@__thread@), seeded from @getrandom(2)@
(@arc4random_buf@ on macOS and the BSDs, @BCryptGenRandom@ on
Windows, @\/dev\/urandom@ elsewhere) on first use, and reseeded in
the child after @fork@ so parent and child never share a stream.
Once seeded, each call is a handful of register-only
arithmetic ops — typically ~1 ns including the FFI boundary,
versus ~50 ns for the global @splitmix@ generator that takes an
@MVar@ on every call.

Caveat: because Haskell threads are multiplexed across OS
threads, the per-Haskell-thread sequence is not reproducible
(an HS thread may resume on a different capability and draw
from a different RNG stream).  This is the right trade-off for
the non-deterministic-randomness needs that motivated the helper
(WebSocket frame masking, retry jitter, …); for reproducible
streams use a 'System.Random.Stateful' generator the caller
owns.
-}
fastRandomWord64 :: IO Word64
fastRandomWord64 = c_xoshiro256pp_next
{-# INLINE fastRandomWord64 #-}


------------------------------------------------------------------------
-- SIMD ASCII check
------------------------------------------------------------------------

-- (position, byte count from there), boolean result.
foreign import ccall unsafe "hs_proto_is_ascii"
  c_is_ascii :: Ptr () -> CPtrdiff -> CPtrdiff -> CInt


-- | Check if @len@ bytes starting at @ptr+offset@ are all ASCII.
isAscii :: Ptr Word8 -> Int -> Int -> Bool
isAscii !ptr !offset !len =
  c_is_ascii (castPtr ptr) (fromIntegral offset) (fromIntegral len) /= 0
{-# INLINE isAscii #-}


-- | Check if a 'ByteString' is entirely ASCII.
isAsciiBS :: ByteString -> Bool
isAsciiBS bs = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! isAscii (castPtr ptr) 0 len
{-# INLINE isAsciiBS #-}


{- | Fast text decoding via @text >= 2.0@'s simdutf-powered 'TE.decodeUtf8''.

We rely on text's internal simdutf which uses AVX2/NEON for UTF-8
validation and decoding in a single pass — faster than a separate
ASCII pre-check followed by decode.
-}
decodeTextFast :: ByteString -> Either String Text
decodeTextFast bs = case TE.decodeUtf8' bs of
  Right t -> Right t
  Left _ -> Left "invalid UTF-8"
{-# INLINE decodeTextFast #-}


------------------------------------------------------------------------
-- SIMD JSON escape scanner
------------------------------------------------------------------------

-- (position, byte count from there), position or offset+len: CPtrdiff.
foreign import ccall unsafe "hs_proto_find_json_escape"
  c_find_json_escape :: Ptr () -> CPtrdiff -> CPtrdiff -> CPtrdiff


{- | Find the first byte that needs JSON escaping (control char < 0x20,
@\"@, or @\\@) starting at the given offset within a raw pointer.
Returns the absolute offset of that byte, or @offset + len@ if none found.
-}
findJsonEscape :: Ptr Word8 -> Int -> Int -> Int
findJsonEscape !ptr !offset !len =
  fromIntegral (c_find_json_escape (castPtr ptr) (fromIntegral offset) (fromIntegral len))
{-# INLINE findJsonEscape #-}


-- | Find the first byte needing JSON escaping in a 'ByteString'.
findJsonEscapeBS :: ByteString -> Int -> Int
findJsonEscapeBS bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! findJsonEscape (castPtr ptr) off (len - off)
{-# INLINE findJsonEscapeBS #-}


{- | Escape a 'ByteString' (assumed UTF-8) for inclusion in a JSON string.
Uses the SIMD scanner to skip safe regions in bulk, only character-escaping
the bytes that need it. Does NOT add surrounding quotes.
-}
escapeJSONStringBS :: ByteString -> B.Builder
escapeJSONStringBS bs = go 0
  where
    !len = BS.length bs
    go !pos
      | pos >= len = mempty
      | otherwise =
          let !escPos = findJsonEscapeBS bs pos
              !safeLen = escPos - pos
          in ( if safeLen > 0
                 then B.byteString (BSU.unsafeTake safeLen (BSU.unsafeDrop pos bs))
                 else mempty
             )
               <> if escPos >= len
                 then mempty
                 else
                   let !b = BSU.unsafeIndex bs escPos
                   in escByte b <> go (escPos + 1)

    escByte :: Word8 -> B.Builder
    escByte 0x22 = B.byteString "\\\"" -- "
    escByte 0x5C = B.byteString "\\\\" -- backslash
    escByte 0x08 = B.byteString "\\b" -- BS
    escByte 0x0C = B.byteString "\\f" -- FF
    escByte 0x0A = B.byteString "\\n" -- LF
    escByte 0x0D = B.byteString "\\r" -- CR
    escByte 0x09 = B.byteString "\\t" -- TAB
    escByte b =
      B.byteString "\\u00"
        <> B.word8 (hexNibble (b `div` 16))
        <> B.word8 (hexNibble (b `mod` 16))

    hexNibble :: Word8 -> Word8
    hexNibble n
      | n < 10 = 0x30 + n -- '0'..'9'
      | otherwise = 0x61 + n - 10 -- 'a'..'f'
{-# INLINE escapeJSONStringBS #-}


{- | Escape a 'Text' value for JSON output. Encodes to UTF-8 and uses the
SIMD scanner for fast bulk skipping of safe characters.
-}
escapeJSONText :: Text -> B.Builder
escapeJSONText = escapeJSONStringBS . TE.encodeUtf8
{-# INLINE escapeJSONText #-}


------------------------------------------------------------------------
-- SIMD EDN whitespace skipper
------------------------------------------------------------------------

-- (start position, buffer length), position: CPtrdiff throughout.
foreign import ccall unsafe "hs_proto_skip_ws"
  c_skip_ws :: Ptr () -> CPtrdiff -> CPtrdiff -> CPtrdiff


{- | Skip EDN whitespace (space, tab, newline, CR, comma) and comments
(; to end of line) starting at the given offset.
Returns the offset of the first non-whitespace character.
-}
skipWhitespace :: Ptr Word8 -> Int -> Int -> Int
skipWhitespace !ptr !offset !len =
  fromIntegral (c_skip_ws (castPtr ptr) (fromIntegral offset) (fromIntegral len))
{-# INLINE skipWhitespace #-}


-- | Skip EDN whitespace in a 'ByteString' starting at the given offset.
skipWhitespaceBS :: ByteString -> Int -> Int
skipWhitespaceBS bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! skipWhitespace (castPtr ptr) off len
{-# INLINE skipWhitespaceBS #-}


------------------------------------------------------------------------
-- SIMD Iceberg bounds comparison
------------------------------------------------------------------------

-- (count, width in bytes), 32-bit bitmask result.
foreign import ccall unsafe "hs_proto_compare_bounds"
  c_compare_bounds :: Ptr () -> CSize -> CSize -> Ptr () -> CInt


{- | Compare a search value against N serialized bounds (all same width).
Returns a bitmask where bit i is set if @bounds[i] <= search@.
For 4-byte LE int32 bounds, uses SSE2 to compare 4 at a time.

@count@ and @width@ are counts, not offsets, and must be non-negative:
they cross the FFI as 'CSize', so a negative value is a caller bug (it
becomes a huge count) rather than an empty mask. @width == 0@ means empty
bounds, which compare equal to the search value.
-}
compareBounds :: Ptr Word8 -> Int -> Int -> Ptr Word8 -> Int
compareBounds !boundsPtr !count !width !searchPtr =
  fromIntegral (c_compare_bounds (castPtr boundsPtr) (fromIntegral count) (fromIntegral width) (castPtr searchPtr))
{-# INLINE compareBounds #-}


-- | Compare bounds stored in a 'ByteString' against a search value
-- 'ByteString'.
--
-- @count@ and @width@ are clamped to what the buffers actually hold, so a
-- caller that passes an inconsistent count cannot make the kernel read
-- past either buffer (the kernel trusts them: it compares @count*width@
-- bytes and reads @width@ from the search value).
compareBoundsBS :: ByteString -> Int -> Int -> ByteString -> Int
compareBoundsBS boundsBs count width searchBs =
  let !w = max 0 (min width (BS.length searchBs))
      !fits = if w == 0 then 0 else BS.length boundsBs `quot` w
      !n = max 0 (min count fits)
  in unsafePerformIO $
       BSU.unsafeUseAsCStringLen boundsBs $ \(bPtr, _) ->
         BSU.unsafeUseAsCStringLen searchBs $ \(sPtr, _) ->
           pure $! compareBounds (castPtr bPtr) n w (castPtr sPtr)
{-# INLINE compareBoundsBS #-}


------------------------------------------------------------------------
-- SIMD Arrow IPC buffer validation
------------------------------------------------------------------------

foreign import ccall unsafe "hs_proto_validate_arrow_buffers"
  c_validate_arrow_buffers :: Ptr () -> CSize -> Int64 -> CInt

foreign import ccall safe "hs_proto_validate_arrow_buffers"
  c_validate_arrow_buffers_safe :: Ptr () -> CSize -> Int64 -> CInt


{- | Validate Arrow IPC buffer offset/length pairs.
Buffers are pairs of @(offset :: Int64, length :: Int64)@ packed contiguously.
Returns 'True' if all offsets/lengths are non-negative, within body_length,
and non-overlapping.

The kernel is one linear pass over the pairs (16 bytes each). Like the
"Columnar.SIMD" kernels it uses an @unsafe@ call for at most 64 KiB of
input (4096 buffers, far more than a record batch normally has) and a
@safe@ call above that, so a hostile buffer table cannot hold up the RTS.

@count@ is the number of pairs (it crosses the FFI as 'CSize'), so it must
be non-negative; the callers pass @VS.length@ of the buffer vector.
-}
validateArrowBuffers :: Ptr Int64 -> Int -> Int64 -> Bool
validateArrowBuffers !ptr !count !bodyLength
  | count <= 4096 = c_validate_arrow_buffers (castPtr ptr) (fromIntegral count) bodyLength /= 0
  | otherwise = c_validate_arrow_buffers_safe (castPtr ptr) (fromIntegral count) bodyLength /= 0
{-# INLINE validateArrowBuffers #-}


------------------------------------------------------------------------
-- Haskell-side endianness helpers (single MOV + BSWAP on x86)
------------------------------------------------------------------------

-- | Read a big-endian 16-bit word at offset. Single MOV + BSWAP on x86.
readBE16H :: Ptr Word8 -> Int -> IO Word16
readBE16H p off = byteSwap16 <$> peekByteOff p off
{-# INLINE readBE16H #-}


-- | Read a big-endian 32-bit word at offset. Single MOV + BSWAP on x86.
readBE32H :: Ptr Word8 -> Int -> IO Word32
readBE32H p off = byteSwap32 <$> peekByteOff p off
{-# INLINE readBE32H #-}


-- | Read a big-endian 64-bit word at offset. Single MOV + BSWAP on x86.
readBE64H :: Ptr Word8 -> Int -> IO Word64
readBE64H p off = byteSwap64 <$> peekByteOff p off
{-# INLINE readBE64H #-}


-- | Write a big-endian 16-bit word at offset.
writeBE16H :: Ptr Word8 -> Int -> Word16 -> IO ()
writeBE16H p off v = pokeByteOff p off (byteSwap16 v)
{-# INLINE writeBE16H #-}


-- | Write a big-endian 32-bit word at offset.
writeBE32H :: Ptr Word8 -> Int -> Word32 -> IO ()
writeBE32H p off v = pokeByteOff p off (byteSwap32 v)
{-# INLINE writeBE32H #-}


-- | Write a big-endian 64-bit word at offset.
writeBE64H :: Ptr Word8 -> Int -> Word64 -> IO ()
writeBE64H p off v = pokeByteOff p off (byteSwap64 v)
{-# INLINE writeBE64H #-}


-- | Read a little-endian 16-bit word at offset. Identity on x86.
readLE16H :: Ptr Word8 -> Int -> IO Word16
readLE16H = peekByteOff
{-# INLINE readLE16H #-}


-- | Read a little-endian 32-bit word at offset. Identity on x86.
readLE32H :: Ptr Word8 -> Int -> IO Word32
readLE32H = peekByteOff
{-# INLINE readLE32H #-}


-- | Read a little-endian 64-bit word at offset. Identity on x86.
readLE64H :: Ptr Word8 -> Int -> IO Word64
readLE64H = peekByteOff
{-# INLINE readLE64H #-}


-- | Write a little-endian 16-bit word at offset. Identity on x86.
writeLE16H :: Ptr Word8 -> Int -> Word16 -> IO ()
writeLE16H = pokeByteOff
{-# INLINE writeLE16H #-}


-- | Write a little-endian 32-bit word at offset. Identity on x86.
writeLE32H :: Ptr Word8 -> Int -> Word32 -> IO ()
writeLE32H = pokeByteOff
{-# INLINE writeLE32H #-}


-- | Write a little-endian 64-bit word at offset. Identity on x86.
writeLE64H :: Ptr Word8 -> Int -> Word64 -> IO ()
writeLE64H = pokeByteOff
{-# INLINE writeLE64H #-}
