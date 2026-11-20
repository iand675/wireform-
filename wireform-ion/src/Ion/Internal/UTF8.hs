{-# LANGUAGE MagicHash #-}

-- Private UTF-8 copy worker for the direct Ion writer. Short fields avoid
-- a foreign memcpy call; longer fields use text's bulk-copy implementation.
module Ion.Internal.UTF8 (copyText) where

import Data.Array.Byte (ByteArray (..))
import Data.Text (Text)
import Data.Text.Array qualified as A
import Data.Text.Foreign qualified as TF
import Data.Text.Internal qualified as T
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Exts (Int (I#), indexWord8ArrayAsWord16#, indexWord8ArrayAsWord32#, indexWord8ArrayAsWord64#)
import GHC.Word (Word16 (W16#), Word32 (W32#), Word64 (W64#))


-- No load/store extends beyond the Text slice or the bytes being copied.
-- In particular the tail load starts at size-width, never at a rounded-up
-- address. The byte-offset primops support unaligned UTF-8 slices.
copyText :: Text -> Ptr Word8 -> IO ()
copyText text@(T.Text array@(ByteArray array#) off@(I# off#) size) ptr
  | size > 32 = TF.unsafeCopyToPtr text ptr
  | size >= 16 = do
      pokeByteOff ptr 0 (load64 array off)
      pokeByteOff ptr 8 (load64 array (off + 8))
      if size > 24 then pokeByteOff ptr 16 (load64 array (off + 16)) else pure ()
      pokeByteOff ptr (size - 8) (load64 array (off + size - 8))
  | size >= 8 = do
      pokeByteOff ptr 0 (load64 array off)
      pokeByteOff ptr (size - 8) (load64 array (off + size - 8))
  | size >= 4 = case off + size - 4 of
      I# end# -> do
        pokeByteOff ptr 0 (W32# (indexWord8ArrayAsWord32# array# off#))
        pokeByteOff ptr (size - 4) (W32# (indexWord8ArrayAsWord32# array# end#))
  | size >= 2 = case off + size - 2 of
      I# end# -> do
        pokeByteOff ptr 0 (W16# (indexWord8ArrayAsWord16# array# off#))
        pokeByteOff ptr (size - 2) (W16# (indexWord8ArrayAsWord16# array# end#))
  | size == 1 = pokeByteOff ptr 0 (A.unsafeIndex array off)
  | otherwise = pure ()
{-# INLINE copyText #-}


load64 :: A.Array -> Int -> Word64
load64 (ByteArray array#) (I# off#) = W64# (indexWord8ArrayAsWord64# array# off#)
{-# INLINE load64 #-}
