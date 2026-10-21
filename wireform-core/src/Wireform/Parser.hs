{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Fast streaming parser with flatparse-grade inner-loop performance.
module Wireform.Parser (
  -- * Parser type
  Parser,

  -- * Byte primitives
  anyWord8,
  anyWord8_,
  anyWord16,
  anyWord32,
  anyWord64,
  anyWord16le,
  anyWord32le,
  anyWord64le,
  anyWord16be,
  anyWord32be,
  anyWord64be,
  anyWord16_,
  anyWord32_,
  anyWord64_,
  anyFloatle,
  anyFloatbe,
  anyDoublele,
  anyDoublebe,

  -- * CPS byte primitives (avoid intermediate boxing)
  withAnyWord8,
  withAnyWord16,
  withAnyWord32,
  withAnyWord64,
  withSatisfyAscii,

  -- * Byte matching
  word8,
  byteString,

  -- * ByteString operations
  takeBs,
  takeBsCopy,
  skip,
  takeRest,
  skipBack,

  -- * Signed integers
  anyInt8,
  anyInt16,
  anyInt32,
  anyInt64,
  anyInt16le,
  anyInt32le,
  anyInt64le,
  anyInt16be,
  anyInt32be,
  anyInt64be,

  -- * UTF-8 characters
  anyChar,
  anyChar_,
  satisfy,
  satisfy_,
  satisfyAscii,
  satisfyAscii_,
  skipSatisfyAscii,
  fusedSatisfy,

  -- * UTF-8 characters (additional)
  anyAsciiChar,
  skipAnyAsciiChar,

  -- * Character classes
  isDigit,
  isLatinLetter,
  isGreekLetter,

  -- * ASCII numeric
  anyAsciiDecimalWord,
  anyAsciiDecimalInt,
  anyAsciiDecimalInteger,
  anyAsciiHexWord,
  anyAsciiHexInt,

  -- * Control flow
  (<|>),
  empty,
  branch,
  lookahead,
  fails,
  try,
  optional,
  optional_,
  many_,
  some_,
  many,
  some,
  skipMany,
  skipSome,
  withOption,
  chainl,
  chainr,
  notFollowedBy,
  isolate,

  -- * Error handling
  err,
  cut,
  cutting,
  ensureNOrEof,
  withError,

  -- * Position and span
  Pos (..),
  Span (..),
  getPos,
  withSpan,
  byteStringOf,
  spanToByteString,
  withByteString,
  inSpan,
  subPos,

  -- * Marks
  Mark,
  mark,
  restore,
  release,

  -- * Low-level
  ensureN#,
  checkpoint,
  eof,
  atEnd,
  remaining,

  -- * Re-exports
  module Wireform.Parser.Error,
) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Int
import Data.Word
import Foreign.C.Types (CSize (..))
import Foreign.Marshal.Utils (copyBytes)
import GHC.Exts
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.ForeignPtr (ForeignPtr (..), mallocPlainForeignPtrBytes, unsafeWithForeignPtr)
import GHC.IO (IO (..))
import GHC.Word (Word16 (..), Word32 (..), Word64 (..), Word8 (..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Wireform.Parser.Error
import Wireform.Parser.Internal
import Wireform.Parser.Mark
import Wireform.Parser.Position


------------------------------------------------------------------------
-- Internal: withEnsure# (bounds check then run continuation)
------------------------------------------------------------------------

{- | Ensure @n#@ bytes available, then run the body.

Fast path (1 register comparison, no memory access).
Semi-fast path: re-read 'peEndPtr' to catch stale @eob@ after a
prior streaming resume; avoids the NOINLINE 'ensureNSlow' call.
Slow path: genuine suspension via 'onEnsureFail'.
-}
withEnsure# :: forall m e a. ParserMode m => Int# -> Parser m e a -> Parser m e a
withEnsure# n# (Parser p) = Parser \env eob s st ->
  case n# <=# minusAddr# eob s of
    1# -> p env eob s st
    _ -> case readEnd# env st of
      (# st', eob' #) -> case n# <=# minusAddr# eob' s of
        1# -> p env eob' s st'
        _ -> case onEnsureFail @m env eob' s n# st' of
          (# st'', OK# _ s'' #) -> case readEnd# env st'' of
            (# st''', eob'' #) -> p env eob'' s'' st'''
          (# st'', x #) -> (# st'', unsafeCoerce# x #)
{-# INLINE withEnsure# #-}


------------------------------------------------------------------------
-- CPS byte primitives (match flatparse's withAnyWord8 etc)
------------------------------------------------------------------------

{- | Read one byte and pass it to the continuation.  Most efficient
form — avoids allocating an intermediate @Word8@ on the heap.
-}
withAnyWord8 :: ParserMode m => (Word8 -> Parser m e r) -> Parser m e r
withAnyWord8 p = withEnsure# 1# $ Parser \env eob s st ->
  case indexWord8OffAddr# s 0# of
    w# -> runParser# (p (W8# w#)) env eob (plusAddr# s 1#) st
{-# INLINE withAnyWord8 #-}


withAnyWord16 :: ParserMode m => (Word16 -> Parser m e r) -> Parser m e r
withAnyWord16 p = withEnsure# 2# $ Parser \env eob s st ->
  case indexWord16OffAddr# s 0# of
    w# -> runParser# (p (W16# w#)) env eob (plusAddr# s 2#) st
{-# INLINE withAnyWord16 #-}


withAnyWord32 :: ParserMode m => (Word32 -> Parser m e r) -> Parser m e r
withAnyWord32 p = withEnsure# 4# $ Parser \env eob s st ->
  case indexWord32OffAddr# s 0# of
    w# -> runParser# (p (W32# w#)) env eob (plusAddr# s 4#) st
{-# INLINE withAnyWord32 #-}


withAnyWord64 :: ParserMode m => (Word64 -> Parser m e r) -> Parser m e r
withAnyWord64 p = withEnsure# 8# $ Parser \env eob s st ->
  case indexWord64OffAddr# s 0# of
    w# -> runParser# (p (W64# w#)) env eob (plusAddr# s 8#) st
{-# INLINE withAnyWord64 #-}


------------------------------------------------------------------------
-- Non-CPS byte primitives
------------------------------------------------------------------------

anyWord8 :: ParserMode m => Parser m e Word8
anyWord8 = withAnyWord8 pure
{-# INLINE anyWord8 #-}


anyWord8_ :: ParserMode m => Parser m e ()
anyWord8_ = withEnsure# 1# $ Parser \_ _ s st ->
  (# st, OK# () (plusAddr# s 1#) #)
{-# INLINE anyWord8_ #-}


anyWord16 :: ParserMode m => Parser m e Word16
anyWord16 = withAnyWord16 pure
{-# INLINE anyWord16 #-}


anyWord32 :: ParserMode m => Parser m e Word32
anyWord32 = withAnyWord32 pure
{-# INLINE anyWord32 #-}


anyWord64 :: ParserMode m => Parser m e Word64
anyWord64 = withAnyWord64 pure
{-# INLINE anyWord64 #-}


-- Skip variants
anyWord16_ :: ParserMode m => Parser m e ()
anyWord16_ = withEnsure# 2# $ Parser \_ _ s st -> (# st, OK# () (plusAddr# s 2#) #)
{-# INLINE anyWord16_ #-}


anyWord32_ :: ParserMode m => Parser m e ()
anyWord32_ = withEnsure# 4# $ Parser \_ _ s st -> (# st, OK# () (plusAddr# s 4#) #)
{-# INLINE anyWord32_ #-}


anyWord64_ :: ParserMode m => Parser m e ()
anyWord64_ = withEnsure# 8# $ Parser \_ _ s st -> (# st, OK# () (plusAddr# s 8#) #)
{-# INLINE anyWord64_ #-}

------------------------------------------------------------------------
-- Endianness variants
------------------------------------------------------------------------

#if defined(WORDS_BIGENDIAN)
anyWord16le = withAnyWord16 (pure . byteSwap16)
anyWord32le = withAnyWord32 (pure . byteSwap32)
anyWord64le = withAnyWord64 (pure . byteSwap64)
anyWord16be = anyWord16
anyWord32be = anyWord32
anyWord64be = anyWord64
#else
anyWord16le :: ParserMode m => Parser m e Word16
anyWord16le = anyWord16
{-# INLINE anyWord16le #-}
anyWord32le :: ParserMode m => Parser m e Word32
anyWord32le = anyWord32
{-# INLINE anyWord32le #-}
anyWord64le :: ParserMode m => Parser m e Word64
anyWord64le = anyWord64
{-# INLINE anyWord64le #-}
anyWord16be :: ParserMode m => Parser m e Word16
anyWord16be = withAnyWord16 (pure . byteSwap16)
{-# INLINE anyWord16be #-}
anyWord32be :: ParserMode m => Parser m e Word32
anyWord32be = withAnyWord32 (pure . byteSwap32)
{-# INLINE anyWord32be #-}
anyWord64be :: ParserMode m => Parser m e Word64
anyWord64be = withAnyWord64 (pure . byteSwap64)
{-# INLINE anyWord64be #-}
#endif


------------------------------------------------------------------------
-- Floating-point
------------------------------------------------------------------------

anyFloatle :: ParserMode m => Parser m e Float
anyFloatle = castWord32ToFloat <$> anyWord32le
{-# INLINE anyFloatle #-}


anyFloatbe :: ParserMode m => Parser m e Float
anyFloatbe = castWord32ToFloat <$> anyWord32be
{-# INLINE anyFloatbe #-}


anyDoublele :: ParserMode m => Parser m e Double
anyDoublele = castWord64ToDouble <$> anyWord64le
{-# INLINE anyDoublele #-}


anyDoublebe :: ParserMode m => Parser m e Double
anyDoublebe = castWord64ToDouble <$> anyWord64be
{-# INLINE anyDoublebe #-}


------------------------------------------------------------------------
-- Byte matching
------------------------------------------------------------------------

word8 :: ParserMode m => Word8 -> Parser m e ()
word8 (W8# expected) = withEnsure# 1# $ Parser \_ _ s st ->
  case eqWord8# (indexWord8OffAddr# s 0#) expected of
    1# -> (# st, OK# () (plusAddr# s 1#) #)
    _ -> (# st, Fail# #)
{-# INLINE word8 #-}


-- | Match a literal 'ByteString'.  Uses word-at-a-time comparison.
byteString :: ByteString -> Parser m e ()
byteString bs = Parser \env eob s st ->
  let !(BSI.BS _ (I# len#)) = bs
  in case len# <=# minusAddr# eob s of
       1# -> case memcmpAddr# s (bsAddr# bs) len# of
         0# -> (# st, OK# () (plusAddr# s len#) #)
         _ -> (# st, Fail# #)
       _ -> case ensureNSlow env eob s len# st of
         (# st', OK# _ s' #) -> case readEnd# env st' of
           (# st'', _ #) ->
             case memcmpAddr# s' (bsAddr# bs) len# of
               0# -> (# st'', OK# () (plusAddr# s' len#) #)
               _ -> (# st'', Fail# #)
         (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE byteString #-}


bsAddr# :: ByteString -> Addr#
bsAddr# (BSI.BS (ForeignPtr addr _) _) = addr
{-# INLINE bsAddr# #-}


memcmpAddr# :: Addr# -> Addr# -> Int# -> Int#
memcmpAddr# a b n = case unsafeDupablePerformIO (c_memcmp (Ptr a) (Ptr b) (fromIntegral (I# n))) of
  r -> if r == 0 then 0# else 1#
{-# INLINE memcmpAddr# #-}


-- memcmp's third parameter is size_t. n is the length of a literal
-- ByteString, which byteString has already compared against the bytes
-- remaining (`len# <=# ...`), so it is non-negative here and Int -> CSize
-- cannot narrow.
foreign import ccall unsafe "memcmp"
  c_memcmp :: Ptr Word8 -> Ptr Word8 -> CSize -> IO Int


------------------------------------------------------------------------
-- ByteString operations
------------------------------------------------------------------------

{- | Take @n@ bytes.

Returns a /zero-copy slice/ into the ring (in streaming mode) or
the input 'ByteString' (in 'parseByteString' mode) whenever @n@
fits in the current backing buffer in one go.  When @n@ exceeds
the magic ring's capacity, falls back to draining the bytes into
a fresh heap allocation: 'takeBs' still hands the caller all @n@
bytes, the cost is one allocation + memcpy and the result no
longer aliases the ring.

== Lifetime

For the zero-copy slice case the returned 'ByteString' holds a
reference to the ring's backing memory.  The slice is valid as
long as the ring exists (managed by 'withMagicRing' / 'withRecvTransport').
If the ring is freed while the 'ByteString' is still alive, the
slice becomes a dangling pointer.  In practice this is safe because
'runParser' / 'runParserLoop' run within the transport's scope.

For the drained-into-a-fresh-allocation case (@n@ > ring size) the
result is an ordinary heap 'ByteString' with no ring dependency
and may outlive 'withMagicRing' freely.

A type-system-enforced alternative for callers that prefer to
/prove/ slices cannot outlive a refill lives in "Wireform.Ring":
use 'Wireform.Ring.RingSlice' values and 'Wireform.Ring.copyRingSlice'
as the explicit escape hatch.
-}
takeBs :: forall m e. ParserMode m => Int -> Parser m e ByteString
takeBs total
  | total <= 0 = pure BS.empty
  | otherwise = Parser \env eob s st ->
      let !(I# mask#) = peMask env
      in case (case total of I# n# -> (n# -# 1#) <=# mask#) of
           1# -> runParser# (takeBsSlice @m total) env eob s st
           -- Drain path is Stream-monomorphic so that 'modeCheckpoint'
           -- in the loop body resolves to 'checkpoint' at compile time
           -- (no per-iteration dictionary indirection).  'coerce' is
           -- safe because the 'm' phantom parameter of 'Parser' has
           -- role phantom — 'Parser Stream e a' and 'Parser m e a'
           -- have identical runtime representation.  Pure mode never
           -- reaches this branch at runtime (peMask is maxBound for
           -- Pure, so the bounds check always picks the fast slice).
           _ ->
             runParser#
               (coerce (takeBsDrainStream @e total) :: Parser m e ByteString)
               env
               eob
               s
               st
{-# INLINE takeBs #-}


{- | Take @n@ bytes, always copying.  Use when you need the result
to outlive the transport's scope.

Like 'takeBs', accepts any @n@ — for @n@ larger than the ring
size, the bytes are drained chunk-by-chunk through the ring into
the destination, advancing the consumer tail between chunks so
the producer has room to keep filling.
-}
takeBsCopy :: forall m e. ParserMode m => Int -> Parser m e ByteString
takeBsCopy total
  | total <= 0 = pure BS.empty
  | otherwise = Parser \env eob s st ->
      let !(I# mask#) = peMask env
      in case (case total of I# n# -> (n# -# 1#) <=# mask#) of
           1# -> runParser# (takeBsCopySingle @m total) env eob s st
           -- See 'takeBs' for why the drain path is Stream-monomorphic
           -- + 'coerce'd back to 'Parser m'.
           _ ->
             runParser#
               (coerce (takeBsDrainStream @e total) :: Parser m e ByteString)
               env
               eob
               s
               st
{-# INLINE takeBsCopy #-}


-- | Single-shot zero-copy slice (fast path of 'takeBs').
takeBsSlice :: forall m e. ParserMode m => Int -> Parser m e ByteString
takeBsSlice (I# n#) = withEnsure# @m n# $ Parser \env _ s st ->
  let !bs = BSI.BS (ForeignPtr s (peBackingFp env)) (I# n#)
  in (# st, OK# bs (plusAddr# s n#) #)
{-# INLINE takeBsSlice #-}


-- | Single-shot memcpy into a fresh allocation (fast path of 'takeBsCopy').
takeBsCopySingle :: forall m e. ParserMode m => Int -> Parser m e ByteString
takeBsCopySingle (I# n#) = withEnsure# @m n# $ Parser \_ _ s st ->
  let !bs =
        unsafeDupablePerformIO
          (BSI.create (I# n#) \dst -> copyBytes dst (Ptr s) (I# n#))
  in (# st, OK# bs (plusAddr# s n#) #)
{-# INLINE takeBsCopySingle #-}


{- | Drain @total@ bytes through the ring into a fresh heap
allocation.  Used by 'takeBs' and 'takeBsCopy' when @total@ does
not fit in the ring at once.

Strategy:

1.  Allocate a pinned 'ForeignPtr' of size @total@.
2.  'checkpoint' — advance tail to the parser's current position
    so the producer has the whole ring to refill into.
3.  Loop: 'ensureN#' a ring-sized chunk, memcpy from the cursor
    into the destination, advance the cursor, 'checkpoint' if
    more chunks remain.

The loop performs roughly @ceil(total / ringSize)@
producer/consumer round-trips.

Deliberately monomorphic in @Parser Stream e@ rather than
polymorphic over @ParserMode m@: 'checkpoint' / 'ensureN#' need
to resolve at compile time so the inner loop carries no
dictionary indirection.  Pure-mode callers can't reach this
function anyway (their bounds check always picks the fast
slice), so the polymorphic dispatch lives in 'takeBs' /
'takeBsCopy' which 'coerce' the 'Parser Stream' result into
'Parser m' across the (phantom) @m@ parameter.
-}
takeBsDrainStream :: forall e. Int -> Parser Stream e ByteString
takeBsDrainStream total = do
  ringSz <- readRingSize
  fp <- allocPinnedFp total
  checkpoint
  drainLoopStream fp 0 total ringSz
  pure $! BSI.BS fp total


{- | Pull at most 'ringSize' bytes per iteration into @fp@; advance
cur and tail between iterations.

Bang patterns on every argument so the strictness analyzer
unboxes 'copied' / 'total' / 'ringSz' into 'Int#' across the
recursive call.  Without the bang on @ringSz@ in particular the
worker shipped it as a boxed 'Int' and paid one @case I#@ per
iteration to unbox it.
-}
drainLoopStream
  :: forall e
   . ForeignPtr Word8
  -> Int
  -> Int
  -> Int
  -> Parser Stream e ()
drainLoopStream !fp !copied !total !ringSz
  | copied >= total = pure ()
  | otherwise = do
      let !(I# c#) = min (total - copied) ringSz
      ensureN# @Stream c#
      memcpyFromCur fp copied (I# c#)
      let !copied' = copied + I# c#
      when (copied' < total) checkpoint
      drainLoopStream fp copied' total ringSz


{- | 'peMask + 1' — the streaming ring's capacity, or 'maxBound' /
arbitrary in whole-input mode (the dispatch in 'takeBs' / 'takeBsCopy'
never enters the drain path in whole-input mode, so the value is
not actually consumed there).
-}
readRingSize :: Parser m e Int
readRingSize = Parser \env _eob s st ->
  let !rsz = case peMask env of
        m -> if m == maxBound then m else m + 1
  in (# st, OK# rsz s #)
{-# INLINE readRingSize #-}


{- | Allocate a pinned 'ForeignPtr' of the given size, threading the
parser's 'State#' so allocation order is well-defined.
-}
allocPinnedFp :: Int -> Parser m e (ForeignPtr Word8)
allocPinnedFp n = Parser \_env _eob s st0 ->
  case unIO (mallocPlainForeignPtrBytes n) st0 of
    (# st1, fp #) -> (# st1, OK# fp s #)
  where
    unIO (IO f) = f
{-# INLINE allocPinnedFp #-}


{- | Copy @len@ bytes from the parser's cursor into @fp + dstOff@,
advancing the cursor by @len@ bytes.  Caller must have called
'ensureN#' (or equivalent) for at least @len@ bytes.

Uses 'unsafeWithForeignPtr' rather than 'withForeignPtr': the
former skips the per-call 'keepAlive#' barrier and the fresh
closure allocation that goes with it.  Safe here because @fp@
is bound by the outer @do@-block in 'takeBsDrain' and stays live
for the entire drain — the returned @BSI.BS fp _@ keeps it
referenced past the loop's last iteration, and the drain loop
itself only ever reads through the address.
-}
memcpyFromCur :: ForeignPtr Word8 -> Int -> Int -> Parser m e ()
memcpyFromCur fp (I# dstOff#) (I# len#) = Parser \_env _eob s st0 ->
  case unIO
    ( unsafeWithForeignPtr fp \(Ptr dst#) ->
        copyBytes
          (Ptr (plusAddr# dst# dstOff#))
          (Ptr s)
          (I# len#)
    )
    st0 of
    (# st1, () #) -> (# st1, OK# () (plusAddr# s len#) #)
  where
    unIO (IO f) = f
{-# INLINE memcpyFromCur #-}


skip :: ParserMode m => Int -> Parser m e ()
skip (I# n#) = withEnsure# n# $ Parser \_ _ s st ->
  (# st, OK# () (plusAddr# s n#) #)
{-# INLINE skip #-}


-- | Consume all remaining bytes in the current window.
takeRest :: Parser m e ByteString
takeRest = Parser \env eob s st ->
  let !len = I# (minusAddr# eob s)
      !bs =
        if len <= 0
          then BS.empty
          else BSI.BS (ForeignPtr s (peBackingFp env)) len
  in (# st, OK# bs eob #)
{-# INLINE takeRest #-}


{- | Skip backward @n@ bytes (takes a positive integer).

In the ring buffer context, you can only skip back to bytes that
haven't been overwritten — i.e., bytes between the consumer tail
and the current position.  For 'parseByteString' this is the
entire input.  For streaming, it's bounded by whatever the driver
hasn't advanced the tail past (at minimum, the start of the
current 'runParser' invocation).

Fails if @n@ would move before the most recent anchor (the start
of the current parse run for non-streaming, or the latest
'checkpoint' for streaming).  Does NOT check the ring tail
directly — the driver guarantees tail <= anchor.
-}
skipBack :: Int -> Parser m e ()
skipBack (I# n#) = Parser \env _ s st ->
  let !target = plusAddr# s (negateInt# n#)
      !(Ptr anchorPtr#) = peAnchorCur env
  in case readAddrOffAddr# anchorPtr# 0# st of
       (# st', anchorCur# #) ->
         case leAddr# anchorCur# target of
           1# -> (# st', OK# () target #)
           _ -> (# st', Fail# #)
{-# INLINE skipBack #-}


------------------------------------------------------------------------
-- Signed integers
------------------------------------------------------------------------

anyInt8 :: ParserMode m => Parser m e Int8
anyInt8 = fromIntegral <$> anyWord8
{-# INLINE anyInt8 #-}


anyInt16 :: ParserMode m => Parser m e Int16
anyInt16 = fromIntegral <$> anyWord16
{-# INLINE anyInt16 #-}


anyInt32 :: ParserMode m => Parser m e Int32
anyInt32 = fromIntegral <$> anyWord32
{-# INLINE anyInt32 #-}


anyInt64 :: ParserMode m => Parser m e Int64
anyInt64 = fromIntegral <$> anyWord64
{-# INLINE anyInt64 #-}


anyInt16le :: ParserMode m => Parser m e Int16
anyInt16le = fromIntegral <$> anyWord16le
{-# INLINE anyInt16le #-}


anyInt32le :: ParserMode m => Parser m e Int32
anyInt32le = fromIntegral <$> anyWord32le
{-# INLINE anyInt32le #-}


anyInt64le :: ParserMode m => Parser m e Int64
anyInt64le = fromIntegral <$> anyWord64le
{-# INLINE anyInt64le #-}


anyInt16be :: ParserMode m => Parser m e Int16
anyInt16be = fromIntegral <$> anyWord16be
{-# INLINE anyInt16be #-}


anyInt32be :: ParserMode m => Parser m e Int32
anyInt32be = fromIntegral <$> anyWord32be
{-# INLINE anyInt32be #-}


anyInt64be :: ParserMode m => Parser m e Int64
anyInt64be = fromIntegral <$> anyWord64be
{-# INLINE anyInt64be #-}


------------------------------------------------------------------------
-- UTF-8 / ASCII
------------------------------------------------------------------------

-- | Parse any UTF-8 character.
anyChar :: ParserMode m => Parser m e Char
anyChar = withEnsure# 1# $ Parser \_ eob s st ->
  case indexWord8OffAddr# s 0# of
    c1 -> case leWord8# c1 (wordToWord8# 0x7F##) of
      1# -> (# st, OK# (C# (chr# (word2Int# (word8ToWord# c1)))) (plusAddr# s 1#) #)
      _ -> case leWord8# c1 (wordToWord8# 0xBF##) of
        1# -> (# st, Fail# #)
        _ -> case leWord8# c1 (wordToWord8# 0xDF##) of
          1# -> case 2# <=# minusAddr# eob s of
            1# -> case indexWord8OffAddr# s 1# of
              c2 ->
                let !cp =
                      ((word2Int# (word8ToWord# c1) -# 0xC0#) `uncheckedIShiftL#` 6#)
                        +# (word2Int# (word8ToWord# c2) -# 0x80#)
                in (# st, OK# (C# (chr# cp)) (plusAddr# s 2#) #)
            _ -> (# st, Fail# #)
          _ -> case leWord8# c1 (wordToWord8# 0xEF##) of
            1# -> case 3# <=# minusAddr# eob s of
              1# -> case indexWord8OffAddr# s 1# of
                c2 ->
                  case indexWord8OffAddr# s 2# of
                    c3 ->
                      let !cp =
                            ((word2Int# (word8ToWord# c1) -# 0xE0#) `uncheckedIShiftL#` 12#)
                              +# ((word2Int# (word8ToWord# c2) -# 0x80#) `uncheckedIShiftL#` 6#)
                              +# (word2Int# (word8ToWord# c3) -# 0x80#)
                      in (# st, OK# (C# (chr# cp)) (plusAddr# s 3#) #)
              _ -> (# st, Fail# #)
            _ -> case 4# <=# minusAddr# eob s of
              1# -> case indexWord8OffAddr# s 1# of
                c2 ->
                  case indexWord8OffAddr# s 2# of
                    c3 ->
                      case indexWord8OffAddr# s 3# of
                        c4 ->
                          let !cp =
                                ((word2Int# (word8ToWord# c1) -# 0xF0#) `uncheckedIShiftL#` 18#)
                                  +# ((word2Int# (word8ToWord# c2) -# 0x80#) `uncheckedIShiftL#` 12#)
                                  +# ((word2Int# (word8ToWord# c3) -# 0x80#) `uncheckedIShiftL#` 6#)
                                  +# (word2Int# (word8ToWord# c4) -# 0x80#)
                          in (# st, OK# (C# (chr# cp)) (plusAddr# s 4#) #)
              _ -> (# st, Fail# #)
{-# INLINE anyChar #-}


anyChar_ :: ParserMode m => Parser m e ()
anyChar_ = () <$ anyChar
{-# INLINE anyChar_ #-}


-- | CPS variant: parse an ASCII char and pass to continuation.
withSatisfyAscii :: ParserMode m => (Char -> Bool) -> (Char -> Parser m e r) -> Parser m e r
withSatisfyAscii f p = withEnsure# 1# $ Parser \env eob s st ->
  case indexCharOffAddr# s 0# of
    c1 -> case leChar# c1 '\x7F'# of
      1# ->
        let !ch = C# c1
        in if f ch
             then runParser# (p ch) env eob (plusAddr# s 1#) st
             else (# st, Fail# #)
      _ -> (# st, Fail# #)
{-# INLINE withSatisfyAscii #-}


-- | Parse an ASCII character matching a predicate.
satisfyAscii :: ParserMode m => (Char -> Bool) -> Parser m e Char
satisfyAscii f = withSatisfyAscii f pure
{-# INLINE satisfyAscii #-}


satisfyAscii_ :: ParserMode m => (Char -> Bool) -> Parser m e ()
satisfyAscii_ f = withSatisfyAscii f (\_ -> pure ())
{-# INLINE satisfyAscii_ #-}


skipSatisfyAscii :: ParserMode m => (Char -> Bool) -> Parser m e ()
skipSatisfyAscii f = withEnsure# 1# $ Parser \_ _ s st ->
  case indexCharOffAddr# s 0# of
    c1 -> case leChar# c1 '\x7F'# of
      1# ->
        if f (C# c1)
          then (# st, OK# () (plusAddr# s 1#) #)
          else (# st, Fail# #)
      _ -> (# st, Fail# #)
{-# INLINE skipSatisfyAscii #-}


-- | Parse any ASCII character (< 0x80).
anyAsciiChar :: ParserMode m => Parser m e Char
anyAsciiChar = satisfyAscii (const True)
{-# INLINE anyAsciiChar #-}


-- | Skip one ASCII character.
skipAnyAsciiChar :: ParserMode m => Parser m e ()
skipAnyAsciiChar = skipSatisfyAscii (const True)
{-# INLINE skipAnyAsciiChar #-}


-- | Parse a UTF-8 character matching a predicate.
satisfy :: ParserMode m => (Char -> Bool) -> Parser m e Char
satisfy f = do
  c <- anyChar
  if f c then pure c else empty
{-# INLINE satisfy #-}


satisfy_ :: ParserMode m => (Char -> Bool) -> Parser m e ()
satisfy_ f = () <$ satisfy f
{-# INLINE satisfy_ #-}


{- | Fused satisfy: uses the ASCII fast path when the byte is < 0x80,
the full UTF-8 path otherwise.  Two separate predicates for each case.
-}
fusedSatisfy :: forall m e. ParserMode m => (Char -> Bool) -> (Word8 -> Bool) -> Parser m e Char
fusedSatisfy charPred _bytePred = withEnsure# 1# $ Parser \env eob s st ->
  case indexWord8OffAddr# s 0# of
    w -> case leWord8# w (wordToWord8# 0x7F##) of
      1# ->
        let !ch = C# (chr# (word2Int# (word8ToWord# w)))
        in if charPred ch
             then (# st, OK# ch (plusAddr# s 1#) #)
             else (# st, Fail# #)
      _ -> case runParser# (anyChar @m) env eob s st of
        (# st', OK# c s' #) -> if charPred c then (# st', OK# c s' #) else (# st', Fail# #)
        x -> x
{-# INLINE fusedSatisfy #-}


------------------------------------------------------------------------
-- Character classes
------------------------------------------------------------------------

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
{-# INLINE isDigit #-}


isLatinLetter :: Char -> Bool
isLatinLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
{-# INLINE isLatinLetter #-}


------------------------------------------------------------------------
-- ASCII numeric
------------------------------------------------------------------------

anyAsciiDecimalWord :: ParserMode m => Parser m e Word
anyAsciiDecimalWord = withEnsure# 1# $ Parser \_ eob s st ->
  case indexWord8OffAddr# s 0# of
    c -> case leWord8# (wordToWord8# 0x30##) c of
      0# -> (# st, Fail# #)
      _ -> case leWord8# c (wordToWord8# 0x39##) of
        0# -> (# st, Fail# #)
        _ ->
          let !d0 = W# (word8ToWord# c) - 0x30
          in goDecWord eob (plusAddr# s 1#) st d0
{-# INLINE anyAsciiDecimalWord #-}


goDecWord :: Addr# -> Addr# -> State# RealWorld -> Word -> StRes# e Word
goDecWord eob s st !acc =
  case eqAddr# eob s of
    1# -> (# st, OK# acc s #)
    _ -> case indexWord8OffAddr# s 0# of
      c -> case leWord8# (wordToWord8# 0x30##) c of
        0# -> (# st, OK# acc s #)
        _ -> case leWord8# c (wordToWord8# 0x39##) of
          0# -> (# st, OK# acc s #)
          _ ->
            goDecWord
              eob
              (plusAddr# s 1#)
              st
              (acc * 10 + (W# (word8ToWord# c) - 0x30))
{-# NOINLINE goDecWord #-}


anyAsciiDecimalInt :: ParserMode m => Parser m e Int
anyAsciiDecimalInt = fromIntegral <$> anyAsciiDecimalWord
{-# INLINE anyAsciiDecimalInt #-}


------------------------------------------------------------------------
-- Control flow
------------------------------------------------------------------------

empty :: Parser m e a
empty = Parser \_ _ _ st -> (# st, Fail# #)
{-# INLINE empty #-}


infixr 6 <|>


(<|>) :: Parser m e a -> Parser m e a -> Parser m e a
(<|>) (Parser f) (Parser g) = Parser \env eob s st ->
  case f env eob s st of
    (# st', Fail# #) -> g env eob s st'
    x -> x
{-# INLINE [1] (<|>) #-}


{-# RULES "wireform/reassoc-alt" forall l m r. (l <|> m) <|> r = l <|> (m <|> r) #-}


branch :: Parser m e a -> Parser m e b -> Parser m e b -> Parser m e b
branch (Parser p) (Parser t) (Parser f) = Parser \env eob s st ->
  case p env eob s st of
    (# st', OK# _ s' #) -> t env eob s' st'
    (# st', Fail# #) -> f env eob s st'
    (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE branch #-}


lookahead :: Parser m e a -> Parser m e a
lookahead (Parser f) = Parser \env eob s st ->
  case f env eob s st of
    (# st', OK# a _ #) -> (# st', OK# a s #)
    x -> x
{-# INLINE lookahead #-}


fails :: Parser m e a -> Parser m e ()
fails (Parser f) = Parser \env eob s st ->
  case f env eob s st of
    (# st', OK# _ _ #) -> (# st', Fail# #)
    (# st', Fail# #) -> (# st', OK# () s #)
    (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE fails #-}


try :: Parser m e a -> Parser m e a
try (Parser f) = Parser \env eob s st ->
  case f env eob s st of
    (# st', Err# _ #) -> (# st', Fail# #)
    x -> x
{-# INLINE try #-}


optional :: Parser m e a -> Parser m e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# INLINE optional #-}


optional_ :: Parser m e a -> Parser m e ()
optional_ p = (() <$ p) <|> pure ()
{-# INLINE optional_ #-}


withOption :: Parser m e a -> (a -> Parser m e b) -> Parser m e b -> Parser m e b
withOption (Parser p) just (Parser nothing) = Parser \env eob s st ->
  case p env eob s st of
    (# st', OK# a s' #) -> runParser# (just a) env eob s' st'
    (# st', Fail# #) -> nothing env eob s st'
    (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE withOption #-}


many_ :: Parser m e a -> Parser m e ()
many_ (Parser f) = Parser go
  where
    go env eob s st = case f env eob s st of
      (# st', OK# _ s' #) -> go env eob s' st'
      (# st', Fail# #) -> (# st', OK# () s #)
      (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE many_ #-}


some_ :: Parser m e a -> Parser m e ()
some_ p = p *> many_ p
{-# INLINE some_ #-}


skipMany :: Parser m e a -> Parser m e ()
skipMany = many_
{-# INLINE skipMany #-}


skipSome :: Parser m e a -> Parser m e ()
skipSome = some_
{-# INLINE skipSome #-}


-- | Run @p@ zero or more times, collecting results into a list.
many :: Parser m e a -> Parser m e [a]
many (Parser f) = Parser go
  where
    go env eob s st = case f env eob s st of
      (# st', OK# a s' #) -> case go env eob s' st' of
        (# st'', OK# as s'' #) -> (# st'', OK# (a : as) s'' #)
        x -> x
      (# st', Fail# #) -> (# st', OK# [] s #)
      (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE many #-}


-- | Run @p@ one or more times, collecting results.
some :: Parser m e a -> Parser m e [a]
some p = (:) <$> p <*> many p
{-# INLINE some #-}


-- | Succeed iff @p@ fails. Does not consume input.
notFollowedBy :: Parser m e a -> Parser m e ()
notFollowedBy = fails
{-# INLINE notFollowedBy #-}


{- | Run a parser on exactly @n@ bytes. The inner parser must consume
all @n@ bytes or the overall parse fails.
-}
isolate :: ParserMode m => Int -> Parser m e a -> Parser m e a
isolate (I# n#) (Parser p) = withEnsure# n# $ Parser \env _ s st ->
  let !isolEnd = plusAddr# s n#
  in case p env isolEnd s st of
       (# st', OK# a s' #) -> case eqAddr# s' isolEnd of
         1# -> (# st', OK# a s' #)
         _ -> (# st', Fail# #)
       (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE isolate #-}


------------------------------------------------------------------------
-- Error handling
------------------------------------------------------------------------

-- | Catch an @Err#@ and handle it.
withError :: (e -> Parser m e a) -> Parser m e a -> Parser m e a
withError handler (Parser f) = Parser \env eob s st ->
  case f env eob s st of
    (# st', Err# e #) -> runParser# (handler e) env eob s st'
    x -> x
{-# INLINE withError #-}


err :: e -> Parser m e a
err e = Parser \_ _ _ st -> (# st, Err# e #)
{-# INLINE err #-}


cut :: Parser m e a -> e -> Parser m e a
cut (Parser f) e = Parser \env eob s st ->
  case f env eob s st of
    (# st', Fail# #) -> (# st', Err# e #)
    x -> x
{-# INLINE cut #-}


cutting :: Parser m e a -> e -> (e -> e -> e) -> Parser m e a
cutting (Parser f) e merge = Parser \env eob s st ->
  case f env eob s st of
    (# st', Fail# #) -> (# st', Err# e #)
    (# st', Err# e' #) -> (# st', Err# (merge e' e) #)
    x -> x
{-# INLINE cutting #-}


------------------------------------------------------------------------
-- EOF
------------------------------------------------------------------------

{- | Succeed iff at end of input.
In streaming mode, this may suspend to check if more data is coming.
Uses ensureN# 1 internally: if ensure fails (no more data), we're at EOF.
-}
eof :: forall m e. ParserMode m => Parser m e ()
eof = Parser \env eob s st ->
  case eqAddr# eob s of
    1# -> case onEnsureFail @m env eob s 1# st of
      (# st', OK# _ _ #) -> (# st', Fail# #) -- data arrived
      (# st', Fail# #) -> (# st', OK# () s #) -- genuine EOF
      (# st', x #) -> (# st', unsafeCoerce# x #)
    _ -> (# st, Fail# #)
{-# INLINE eof #-}


{- | Non-consuming check: 'True' if at end of input.
In streaming mode, may suspend to determine.
-}
atEnd :: ParserMode m => Parser m e Bool
atEnd = (eof *> pure True) <|> pure False
{-# INLINE atEnd #-}


-- | Number of bytes remaining in the current window (cheap, no suspend).
remaining :: Parser m e Int
remaining = Parser \_ eob s st ->
  (# st, OK# (I# (minusAddr# eob s)) s #)
{-# INLINE remaining #-}


------------------------------------------------------------------------
-- Chain combinators
------------------------------------------------------------------------

{- | Left-associative chain.
@chainl f p q@ parses @p@ then zero or more @q@, folding left with @f@.
-}
chainl :: (b -> a -> b) -> Parser m e b -> Parser m e a -> Parser m e b
chainl f (Parser p) (Parser q) = Parser \env eob s st ->
  case p env eob s st of
    (# st', OK# b s' #) -> chainlGo f q env eob s' st' b
    (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE chainl #-}


chainlGo
  :: forall e b a
   . (b -> a -> b)
  -> (ParserEnv -> Addr# -> Addr# -> State# RealWorld -> StRes# e a)
  -> ParserEnv
  -> Addr#
  -> Addr#
  -> State# RealWorld
  -> b
  -> StRes# e b
chainlGo f q env eob s st !acc =
  case q env eob s st of
    (# st', OK# a s' #) -> chainlGo f q env eob s' st' (f acc a)
    (# st', Fail# #) -> (# st', OK# acc s #)
    (# st', x #) -> (# st', unsafeCoerce# x #)
{-# NOINLINE chainlGo #-}


{- | Right-associative chain.
@chainr f p q@ parses zero or more @p@, then @q@ at the end,
folding right with @f@.
-}
chainr :: (a -> b -> b) -> Parser m e a -> Parser m e b -> Parser m e b
chainr f (Parser p) (Parser q) = Parser go
  where
    go env eob s st =
      case p env eob s st of
        (# st', OK# a s' #) -> case go env eob s' st' of
          (# st'', OK# b s'' #) -> (# st'', OK# (f a b) s'' #)
          (# st'', x #) -> (# st'', unsafeCoerce# x #)
        (# st', Fail# #) -> q env eob s st'
        (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE chainr #-}


------------------------------------------------------------------------
-- Additional error handling
------------------------------------------------------------------------

{- | Like 'ensureN#' but on EOF, errors with a specific error
instead of failing recoverably.
-}
ensureNOrEof :: ParserMode m => Int -> e -> Parser m e ()
ensureNOrEof n e = ensureN# (case n of I# n# -> n#) <|> err e
{-# INLINE ensureNOrEof #-}


------------------------------------------------------------------------
-- Additional position combinators
------------------------------------------------------------------------

-- | Parse something and return both the result and the bytes consumed.
withByteString :: Parser m e a -> (a -> ByteString -> Parser m e b) -> Parser m e b
withByteString (Parser p) f = Parser \env eob s st ->
  case p env eob s st of
    (# st', OK# a s' #) ->
      let !len = I# (minusAddr# s' s)
          !bs = BSI.BS (ForeignPtr s (peBackingFp env)) len
      in runParser# (f a bs) env eob s' st'
    (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE withByteString #-}


------------------------------------------------------------------------
-- Additional character classes
------------------------------------------------------------------------

isGreekLetter :: Char -> Bool
isGreekLetter c = (c >= '\x0391' && c <= '\x03A9') || (c >= '\x03B1' && c <= '\x03C9')
{-# INLINE isGreekLetter #-}


------------------------------------------------------------------------
-- Additional ASCII numeric
------------------------------------------------------------------------

anyAsciiHexWord :: ParserMode m => Parser m e Word
anyAsciiHexWord = withEnsure# 1# $ Parser \_ eob s st ->
  case hexDigit (indexWord8OffAddr# s 0#) of
    (# | (# #) #) -> (# st, Fail# #)
    (# (# d #) | #) -> goHexWord eob (plusAddr# s 1#) st (W# (word8ToWord# d))
{-# INLINE anyAsciiHexWord #-}


hexDigit :: Word8# -> (# (# Word8# #) | (# #) #)
hexDigit w
  | isTrue# (leWord8# (wordToWord8# 0x30##) w)
  , isTrue# (leWord8# w (wordToWord8# 0x39##)) =
      (# (# wordToWord8# (word8ToWord# w `minusWord#` 0x30##) #) | #)
  | isTrue# (leWord8# (wordToWord8# 0x41##) w)
  , isTrue# (leWord8# w (wordToWord8# 0x46##)) =
      (# (# wordToWord8# (word8ToWord# w `minusWord#` 0x37##) #) | #)
  | isTrue# (leWord8# (wordToWord8# 0x61##) w)
  , isTrue# (leWord8# w (wordToWord8# 0x66##)) =
      (# (# wordToWord8# (word8ToWord# w `minusWord#` 0x57##) #) | #)
  | otherwise = (# | (# #) #)
{-# INLINE hexDigit #-}


goHexWord :: Addr# -> Addr# -> State# RealWorld -> Word -> StRes# e Word
goHexWord eob s st !acc =
  case eqAddr# eob s of
    1# -> (# st, OK# acc s #)
    _ -> case hexDigit (indexWord8OffAddr# s 0#) of
      (# | (# #) #) -> (# st, OK# acc s #)
      (# (# d #) | #) ->
        goHexWord
          eob
          (plusAddr# s 1#)
          st
          (acc * 16 + W# (word8ToWord# d))
{-# NOINLINE goHexWord #-}


anyAsciiHexInt :: ParserMode m => Parser m e Int
anyAsciiHexInt = fromIntegral <$> anyAsciiHexWord
{-# INLINE anyAsciiHexInt #-}


anyAsciiDecimalInteger :: ParserMode m => Parser m e Integer
anyAsciiDecimalInteger = fromIntegral <$> anyAsciiDecimalWord
{-# INLINE anyAsciiDecimalInteger #-}
