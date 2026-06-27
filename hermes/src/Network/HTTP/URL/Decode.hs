{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |

SIMD-accelerated URL percent-decoding.

The hot path delegates to a small C kernel
(@cbits/url_decode.c@) that scans with AVX2 / SSE2 on x86_64,
NEON on AArch64, and a scalar fallback elsewhere. For
unescaped inputs (the overwhelmingly common case for query
parameters) the entire buffer is rejected by a single SIMD
scan and we return the input bytestring unchanged with no
allocation.

When escapes /are/ present we allocate a fresh buffer sized to
@'BS.length' input@ (decoded output is always smaller than or
equal to the input) and let the C kernel write into it in one
pass; the trailing slack is trimmed via
'Data.ByteString.Internal.createUptoN'.

The module is split between:

  * 'urlDecode' / 'urlDecodeForm' — pure, total decoders for
    use outside a parser context.

  * 'urlDecodedSegment' / 'formUrlDecodedSegment' — flatparse
    combinators that grab a raw segment using the caller's
    sub-parser and then run the same C kernel over its bytes.

  * 'urlDecodedWhile' / 'formUrlDecodedWhile' — sugar around
    the segment combinators for the common
    \"consume bytes until a delimiter\" shape.
-}
module Network.HTTP.URL.Decode (
  -- * Errors
  URLDecodeError (..),

  -- * Pure decoding
  urlDecode,
  urlDecodeForm,
  urlDecodeMaybe,
  urlDecodeFormMaybe,

  -- * Flatparse combinators
  urlDecodedSegment,
  formUrlDecodedSegment,
  urlDecodedWhile,
  formUrlDecodedWhile,

  -- * Scanning primitives
  firstSpecialOffset,
) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Wireform.Parser (empty, err, skipMany, withAnyWord8, withByteString)
import Wireform.Parser.Internal (Parser, Pure)


-- ---------------------------------------------------------------------------
-- FFI
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "hermes_url_scan_special"
  c_url_scan_special
    :: Ptr Word8
    -- ^ src
    -> Int
    -- ^ len
    -> Int
    -- ^ plus_is_space (0/1)
    -> IO Int
    -- ^ first special offset, or len if none


foreign import ccall unsafe "hermes_url_decode"
  c_url_decode
    :: Ptr Word8
    -- ^ src
    -> Int
    -- ^ srclen
    -> Ptr Word8
    -- ^ dst (may equal src)
    -> Int
    -- ^ plus_is_space (0/1)
    -> IO Int
    -- ^ bytes written, or negative on error


-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | What went wrong while percent-decoding.
data URLDecodeError
  = -- | Input ends with @\'%\'@ or @\'%H\'@ without the second
    --     hex digit.
    TruncatedEscape
  | -- | An escape contained a byte that wasn't a hex digit.
    InvalidHexDigit
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Exception)


errorFromCode :: Int -> URLDecodeError
errorFromCode (-1) = TruncatedEscape
errorFromCode (-2) = InvalidHexDigit
errorFromCode n =
  -- The C kernel only emits -1 and -2 today; surface anything
  -- else loudly so a future kernel addition can't be silently
  -- swallowed.
  error ("Network.HTTP.URL.Decode: unknown C error code " <> show n)


-- ---------------------------------------------------------------------------
-- Pure decoders
-- ---------------------------------------------------------------------------

{- | Percent-decode a URL component (path segment, query value
without form semantics, fragment, etc.). Sharing-preserving:
returns the input unchanged when no @%@ escapes are present.
-}
urlDecode :: ByteString -> Either URLDecodeError ByteString
urlDecode = decodeWith 0


{- | Percent-decode using @application/x-www-form-urlencoded@
semantics — like 'urlDecode' but also translates @\'+\'@ to a
space.
-}
urlDecodeForm :: ByteString -> Either URLDecodeError ByteString
urlDecodeForm = decodeWith 1


urlDecodeMaybe :: ByteString -> Maybe ByteString
urlDecodeMaybe = either (const Nothing) Just . urlDecode


urlDecodeFormMaybe :: ByteString -> Maybe ByteString
urlDecodeFormMaybe = either (const Nothing) Just . urlDecodeForm


{- | Offset of the first byte the decoder would need to touch.
Returns @'BS.length' bs@ when no special bytes are present.
Backed by the same vectorised scan as the decoders, so it is
safe to call as a cheap pre-flight check before allocating.
-}
firstSpecialOffset :: Bool -> ByteString -> Int
firstSpecialOffset plusIsSpace bs = unsafeDupablePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(cs, len) ->
    c_url_scan_special (castPtr cs) len (boolFlag plusIsSpace)


decodeWith :: Int -> ByteString -> Either URLDecodeError ByteString
decodeWith mode bs
  | BS.null bs = Right bs
  | otherwise = unsafeDupablePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(csrc, srclen) -> do
        let !srcPtr = castPtr csrc :: Ptr Word8
        firstHit <- c_url_scan_special srcPtr srclen mode
        if firstHit == srclen
          then pure (Right bs)
          else do
            errRef <- newIORef (0 :: Int)
            out <- BSI.createUptoN srclen $ \dst -> do
              -- Bulk-copy the unescaped prefix; the kernel handles
              -- everything from the first hit onward in one shot.
              copyBytes dst srcPtr firstHit
              n <-
                c_url_decode
                  (srcPtr `plusPtr` firstHit)
                  (srclen - firstHit)
                  (dst `plusPtr` firstHit)
                  mode
              if n < 0
                then do
                  -- Bail out via a zero-length result; we'll check
                  -- the IORef below to surface the real error.
                  writeIORef errRef n
                  pure 0
                else pure (firstHit + n)
            code <- readIORef errRef
            if code == 0
              then pure (Right out)
              else pure (Left (errorFromCode code))


-- ---------------------------------------------------------------------------
-- Flatparse combinators
-- ---------------------------------------------------------------------------

{- | @urlDecodedSegment p@ runs @p@ for its byte coverage and
then decodes the matched bytes as percent-encoded data.

The wrapped parser controls /where the segment ends/; the
bytes it consumed are then handed to the SIMD decoder in one
pass. A decode failure raises an unrecoverable
'URLDecodeError' via 'FP.err'.
-}
urlDecodedSegment
  :: Parser Pure URLDecodeError a
  -> Parser Pure URLDecodeError ByteString
urlDecodedSegment = decodeSegmentWith 0


-- | Like 'urlDecodedSegment' but also maps @\'+\'@ to a space.
formUrlDecodedSegment
  :: Parser Pure URLDecodeError a
  -> Parser Pure URLDecodeError ByteString
formUrlDecodedSegment = decodeSegmentWith 1


decodeSegmentWith
  :: Int
  -> Parser Pure URLDecodeError a
  -> Parser Pure URLDecodeError ByteString
decodeSegmentWith mode p =
  withByteString p $ \_ bs ->
    case decodeWith mode bs of
      Right out -> pure out
      Left e -> err e


{- | Consume bytes while the predicate holds, then decode them.
The predicate sees raw input bytes (before decoding) so it
should treat @\'%\'@, @\'+\'@, and any byte that would
terminate the segment as outside the run.
-}
urlDecodedWhile
  :: (Word8 -> Bool)
  -> Parser Pure URLDecodeError ByteString
urlDecodedWhile = urlDecodedSegment . skipBytesWhile


formUrlDecodedWhile
  :: (Word8 -> Bool)
  -> Parser Pure URLDecodeError ByteString
formUrlDecodedWhile = formUrlDecodedSegment . skipBytesWhile


{- | Skip bytes while @p@ holds. Always succeeds (may consume
zero bytes). Backtracks past the offending byte on failure
via the default @\<|\>@.
-}
skipBytesWhile :: (Word8 -> Bool) -> Parser Pure e ()
skipBytesWhile p = skipMany consumeIfMatch
  where
    consumeIfMatch =
      withAnyWord8 $ \w ->
        unless (p w) empty


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

boolFlag :: Bool -> Int
boolFlag True = 1
boolFlag False = 0
