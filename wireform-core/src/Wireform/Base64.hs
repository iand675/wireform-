{-# LANGUAGE BangPatterns #-}

{- | RFC 4648 \u00a74 base64 encode / decode.

The implementation lives in @cbits\/base64.c@: an SSSE3 (via
simde) inner loop encoding 12 input bytes per iteration with a
scalar prologue \/ epilogue for the tail.  The decoder takes the
same shape and additionally validates the input alphabet
character by character; non-alphabet bytes outside the trailing
@=@ padding produce 'Nothing'.

These primitives are kept here (rather than in a downstream
package) so every wireform format that needs base64 \u2014
WebSocket handshakes, BSON binary subtype 0x00, the proto3 JSON
mapping for @bytes@, the Avro JSON encoding, the Iceberg
@avro-data-files@ glue \u2014 shares one SIMD implementation
rather than each pulling its own @base64-bytestring@.

The encoder produces /padded/ standard base64; there is no
URL-safe variant.  Add one in a separate module if needed.
-}
module Wireform.Base64 (
  -- * Encode
  encodeBase64,
  encodeBase64Length,

  -- * Decode
  decodeBase64,
  decodeBase64MaxLength,
) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word8)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import System.IO.Unsafe (unsafePerformIO)


------------------------------------------------------------------------
-- FFI
------------------------------------------------------------------------

foreign import ccall unsafe "hs_base64_encoded_length"
  c_base64_encoded_length :: CInt -> CInt


foreign import ccall unsafe "hs_base64_decoded_max_length"
  c_base64_decoded_max_length :: CInt -> CInt


{- | Encoder \/ decoder run as 'IO' so the buffer-mutating side
effect is sequenced; declaring them as pure @CInt@-returning
functions confuses GHC's strictness analysis and the call can
be silently elided.
-}
foreign import ccall unsafe "hs_base64_encode"
  c_base64_encode :: Ptr Word8 -> CInt -> Ptr Word8 -> IO CInt


foreign import ccall unsafe "hs_base64_decode"
  c_base64_decode :: Ptr Word8 -> CInt -> Ptr Word8 -> IO CInt


------------------------------------------------------------------------
-- Length helpers
------------------------------------------------------------------------

{- | Number of base64 characters produced for an input of @n@ bytes
(including @=@ padding).

The C kernel takes a 'CInt' length, so an input longer than
'maxEncodeInput' raises the same error 'encodeBase64' does rather than
silently returning a wrong length; @n < 0@ asks the kernel, which
returns 0.
-}
encodeBase64Length :: Int -> Int
encodeBase64Length n
  | n > maxEncodeInput = errorWithoutStackTrace encodeTooLong
  | otherwise = fromIntegral (c_base64_encoded_length (fromIntegral n))
{-# INLINE encodeBase64Length #-}


{- | Upper bound on the number of bytes produced when decoding @n@
input characters.  The actual decoded length is shorter by the
number of trailing @=@ characters.

Inputs longer than @maxBound :: CInt@ raise an error, like
'encodeBase64Length' does above 'maxEncodeInput'; 'decodeBase64'
returns 'Nothing' for such inputs instead.
-}
decodeBase64MaxLength :: Int -> Int
decodeBase64MaxLength n
  | n > fromIntegral (maxBound :: CInt) = errorWithoutStackTrace decodeTooLong
  | otherwise = fromIntegral (c_base64_decoded_max_length (fromIntegral n))
{-# INLINE decodeBase64MaxLength #-}


------------------------------------------------------------------------
-- Encode
------------------------------------------------------------------------

{- | Encode a 'ByteString' to RFC 4648 \u00a74 base64 (with @=@
padding).  One allocation of the exact output length, one
SIMD pass.

The C kernel works in 'CInt' lengths, so the input may be at most
@3 * (maxBound :: CInt) \`quot\` 4@ bytes (about 1.5 GiB); larger
inputs raise an error instead of being silently truncated.
-}
encodeBase64 :: ByteString -> ByteString
encodeBase64 bs
  | BS.length bs > maxEncodeInput = errorWithoutStackTrace encodeTooLong
  | otherwise = unsafePerformIO $ do
      let !inLen = BS.length bs
          !outLen = encodeBase64Length inLen
      BSI.create outLen $ \outPtr ->
        BSU.unsafeUseAsCStringLen bs $ \(inPtr, _) -> do
          _ <- c_base64_encode (castPtr inPtr) (fromIntegral inLen) outPtr
          pure ()
{-# NOINLINE encodeBase64 #-}


-- | Largest input whose padded encoding length fits in a 'CInt'.
maxEncodeInput :: Int
maxEncodeInput = 3 * (fromIntegral (maxBound :: CInt) `quot` 4)


encodeTooLong :: String
encodeTooLong =
  "Wireform.Base64: input longer than 3 * (2^31 - 1) / 4 bytes"


decodeTooLong :: String
decodeTooLong =
  "Wireform.Base64: input longer than 2^31 - 1 bytes"


------------------------------------------------------------------------
-- Decode
------------------------------------------------------------------------

{- | Decode an RFC 4648 \u00a74 base64 'ByteString'.  Returns
'Nothing' if the input is malformed: not a multiple of four
characters, contains an out-of-alphabet byte, or uses padding
in an invalid position.

Whitespace is /not/ tolerated; strip it before calling if your
input is line-wrapped (PEM, MIME, etc.).

Inputs longer than @maxBound :: CInt@ characters (the C kernel's
length type) yield 'Nothing' rather than being silently truncated.
-}
decodeBase64 :: ByteString -> Maybe ByteString
decodeBase64 bs
  | BS.null bs = Just BS.empty
  | (BS.length bs .&. 3) /= 0 = Nothing
  | BS.length bs > fromIntegral (maxBound :: CInt) = Nothing
  | otherwise = unsafePerformIO $ do
      let !inLen = BS.length bs
          !maxOut = decodeBase64MaxLength inLen
          (inFp, inOff, _) = BSI.toForeignPtr bs
      outFp <- BSI.mallocByteString maxOut
      actual <- withForeignPtr inFp $ \inBase ->
        withForeignPtr outFp $ \outPtr -> do
          n <-
            c_base64_decode
              (inBase `plusPtr` inOff)
              (fromIntegral inLen)
              outPtr
          pure (fromIntegral n :: Int)
      if actual < 0
        then pure Nothing
        else pure (Just (BSI.fromForeignPtr outFp 0 actual))
{-# NOINLINE decodeBase64 #-}
