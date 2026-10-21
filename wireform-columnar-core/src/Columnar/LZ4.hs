{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- | Direct FFI bindings to @liblz4@ for the raw LZ4 block
format. No framing, no size headers — exactly the byte
layout that Apache Parquet's @LZ4_RAW@ codec (codec id 7)
and Apache ORC's @LZ4@ stream codec specify.

The Haskell @lz4@ package on Hackage wraps its compress /
decompress in its own 8-byte size header (matching the C
reference's demo @LZ4_compress_default@ -> @LZ4_uncompress@
contract); that's wire-incompatible with both Parquet and
ORC. We bind the C functions directly so we own the I/O
shape.

Build requires @liblz4-dev@ (or your distribution's
equivalent) at link time. The @-flz4@ Cabal flag (default
on) controls whether this module is exposed.

== Functions used

@
int LZ4_compressBound(int inputSize);
int LZ4_compress_default(const char* src, char* dst,
                         int srcSize, int dstCapacity);
int LZ4_decompress_safe(const char* src, char* dst,
                        int compressedSize, int dstCapacity);
@

The C entry points take @int@ sizes and cannot be widened, so the wrappers
enforce the documented bound themselves: 'compressBound' (and hence
'compress') refuses inputs above 'lz4MaxInputSize', 'decompress' refuses an
input or output above @maxBound :: CInt@ with 'LZ4LengthTooLarge', and no
length is ever narrowed by 'fromIntegral'.

== Test coverage

Round-trip property tests over random inputs in
@wireform-columnar-test/Main.hs@. End-to-end interop with
pyarrow / duckdb / polars Parquet and pyarrow ORC files
exercised by the probes under
@wireform-{parquet,orc}/scripts/@.
-}
module Columnar.LZ4 (
  -- * Block codec
  decompress,
  compress,

  -- * Errors
  LZ4Error (..),
  prettyLZ4Error,

  -- * Internals (exposed for tests)
  compressBound,
  lz4MaxInputSize,
) where

import Control.Exception (mask_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.Types (CChar, CInt (..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)


-- ============================================================
-- FFI
-- ============================================================

foreign import ccall unsafe "LZ4_compressBound"
  c_LZ4_compressBound :: CInt -> CInt


foreign import ccall unsafe "LZ4_compress_default"
  c_LZ4_compress_default
    :: Ptr CChar -- src
    -> Ptr CChar -- dst
    -> CInt -- srcSize
    -> CInt -- dstCapacity
    -> IO CInt -- compressedSize, or 0 on failure


foreign import ccall unsafe "LZ4_decompress_safe"
  c_LZ4_decompress_safe
    :: Ptr CChar -- src
    -> Ptr CChar -- dst
    -> CInt -- compressedSize
    -> CInt -- dstCapacity
    -> IO CInt -- decompressedSize, or negative on failure


{- | @LZ4_MAX_INPUT_SIZE@ from @lz4.h@ (0x7E000000, 2113929216 bytes).

The raw block API is @int@-based by definition, so it cannot be widened
here: the largest block liblz4 will compress is this, and every size the
wrappers below hand to C is checked against it (or against @INT_MAX@, which
is looser, for the decode side) before the call, so no length is ever
narrowed by 'fromIntegral'.
-}
lz4MaxInputSize :: Int
lz4MaxInputSize = 0x7E000000


-- | Largest size the int-based block API accepts for a decode parameter.
cIntMax :: Int
cIntMax = fromIntegral (maxBound :: CInt)


-- ============================================================
-- Errors
-- ============================================================

data LZ4Error
  = -- | liblz4 returned a negative size; the value is the magnitude.
    LZ4DecodeFailed !Int
  | -- | Caller-supplied @maxOutput@ was negative.
    LZ4OutputOverrun
  | -- | liblz4 returned 0 from compress (bound too small).
    LZ4CompressFailed
  | -- | An input or output length does not fit the @int@ sizes of the
    -- raw block API (over @maxBound :: CInt@). See 'lz4MaxInputSize'.
    LZ4LengthTooLarge
  deriving (Show, Eq)


prettyLZ4Error :: LZ4Error -> String
prettyLZ4Error = \case
  LZ4DecodeFailed n ->
    "LZ4: decompression failed at byte "
      ++ show n
      ++ " of the compressed input (malformed block or truncated)"
  LZ4OutputOverrun ->
    "LZ4: caller-supplied maxOutput was negative"
  LZ4CompressFailed ->
    "LZ4: compression failed (output bound was too small for the input)"
  LZ4LengthTooLarge ->
    "LZ4: input or output length exceeds "
      ++ show cIntMax
      ++ " bytes (the raw block API takes int sizes)"


-- ============================================================
-- Public API
-- ============================================================

{- | Worst-case compressed size for an input of @n@ bytes.
Mirrors @LZ4_compressBound@. Returns @-1@ if @n@ is
non-positive or larger than 'lz4MaxInputSize'.

The @n > 'lz4MaxInputSize'@ case is rejected here rather than delegated to
liblz4: @LZ4_compressBound@ takes an @int@, so passing @n@ through
'fromIntegral' would wrap it (a 2^32 + 100 byte input would be sized as 100
bytes) instead of reporting the input as unsupported.
-}
compressBound :: Int -> Int
compressBound !n
  | n <= 0 || n > lz4MaxInputSize = -1
  | otherwise = fromIntegral (c_LZ4_compressBound (fromIntegral n))


{- | Decode one raw LZ4 block.

@decompress maxOutput input@ asks liblz4 to expand @input@
into a buffer of size @maxOutput@. On success returns the
decoded bytes (a slice of that buffer); on failure returns
a diagnostic string.

@maxOutput@ should be the value the caller already knows
from upstream metadata — Parquet's page header has
@uncompressed_page_size@; ORC's PostScript has
@compressionBlockSize@. Passing a value that's too small
causes liblz4 to fail (it won't truncate); passing too
large just allocates extra memory.

Both sizes reach @LZ4_decompress_safe@ as @int@s (the API is
@int@-based by definition), so each is checked against
@maxBound :: CInt@ first: a larger @maxOutput@ or compressed
input returns 'LZ4LengthTooLarge' instead of being narrowed.
-}
decompress :: Int -> ByteString -> Either String ByteString
decompress !maxOutput !input
  | maxOutput < 0 = Left (prettyLZ4Error LZ4OutputOverrun)
  | maxOutput > cIntMax = Left (prettyLZ4Error LZ4LengthTooLarge)
  | BS.null input && maxOutput == 0 = Right BS.empty
  | BS.null input = Right BS.empty
  | inputLen > cIntMax = Left (prettyLZ4Error LZ4LengthTooLarge)
  | otherwise = unsafePerformIO $ mask_ $ do
      -- mask_ + mallocForeignPtrBytes: we want the buffer to
      -- exist for the duration of the C call without GC
      -- moving it. ByteString.fromForeignPtr keeps it alive
      -- for the slice's lifetime.
      fptr <- mallocForeignPtrBytes maxOutput
      withForeignPtr fptr $ \dstPtr ->
        BSU.unsafeUseAsCStringLen input $ \(srcPtr, srcLen) -> do
          rc <-
            c_LZ4_decompress_safe
              srcPtr
              (castPtr dstPtr)
              (fromIntegral srcLen)
              (fromIntegral maxOutput)
          if rc < 0
            then
              pure
                ( Left
                    ( prettyLZ4Error
                        ( LZ4DecodeFailed
                            (fromIntegral (negate rc))
                        )
                    )
                )
            else
              pure
                ( Right
                    ( BSI.fromForeignPtr
                        fptr
                        0
                        (fromIntegral rc)
                    )
                )
  where
    inputLen = BS.length input


{- | Encode one raw LZ4 block.

Output is byte-identical to what @LZ4_compress_default@
writes, so any reference decoder (the C reference,
arrow-rs, parquet-mr, parquet-cpp) accepts it without
needing a frame parser.

@LZ4_compress_default@ takes @int@ sizes, so an input larger than
'lz4MaxInputSize' is refused by 'compressBound' before the call and comes
back as 'BS.empty' (the same signal as a liblz4 failure) rather than being
narrowed to a short @int@.
-}
compress :: ByteString -> ByteString
compress !input
  | BS.null input = BS.empty
  | otherwise = unsafePerformIO $ mask_ $ do
      let !srcLen = BS.length input
          !dstCap = compressBound srcLen
      if dstCap <= 0
        then pure BS.empty -- over lz4MaxInputSize, or liblz4 refused it.
        else do
          fptr <- mallocForeignPtrBytes dstCap
          withForeignPtr fptr $ \dstPtr ->
            BSU.unsafeUseAsCStringLen input $ \(srcPtr, _) -> do
              written <-
                c_LZ4_compress_default
                  srcPtr
                  (castPtr dstPtr)
                  (fromIntegral srcLen)
                  (fromIntegral dstCap)
              if written <= 0
                then pure BS.empty
                else pure (BSI.fromForeignPtr fptr 0 (fromIntegral written))
