{- | Tiny ASCII helpers (case-fold, equality) backed by the SIMD
scanner in @cbits\/http1_scan.c@.

These are exposed as @other-modules@; format-internal use only.
-}
module Network.HTTP1.Internal.Ascii (
  asciiLower,
  asciiIeq,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)


{- | @len@ is a byte count, passed as 'CSize' (the C kernel's @size_t@) so
lengths above 2 GiB are not truncated. It is always @BS.length@ of a
ByteString, hence non-negative.
-}
foreign import ccall unsafe "hs_http1_to_lower_ascii"
  c_to_lower_ascii :: Ptr () -> Ptr () -> CSize -> IO ()


-- | As 'c_to_lower_ascii': @len@ is a non-negative byte count in 'CSize'.
foreign import ccall unsafe "hs_http1_ascii_ieq"
  c_ascii_ieq :: Ptr () -> Ptr () -> CSize -> CInt


-- | ASCII-lowercase a 'ByteString'. Non-ASCII bytes pass through.
{-# INLINE asciiLower #-}
asciiLower :: ByteString -> ByteString
asciiLower bs
  | BS.null bs = bs
  | otherwise = unsafePerformIO $ do
      let !len = BS.length bs
      fp <- BSI.mallocByteString len
      BSU.unsafeUseAsCStringLen bs $ \(src, _) ->
        withForeignPtr fp $ \dst ->
          c_to_lower_ascii (castPtr src) (castPtr dst) (fromIntegral len)
      pure $! BSI.fromForeignPtr fp 0 len


{- | Case-insensitive ASCII equality. Length-mismatched inputs return
'False' without invoking the SIMD path.
-}
{-# INLINE asciiIeq #-}
asciiIeq :: ByteString -> ByteString -> Bool
asciiIeq a b
  | BS.length a /= BS.length b = False
  | BS.null a = True
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen a $ \(pa, len) ->
        BSU.unsafeUseAsCStringLen b $ \(pb, _) ->
          pure (c_ascii_ieq (castPtr pa) (castPtr pb) (fromIntegral len) /= 0)
