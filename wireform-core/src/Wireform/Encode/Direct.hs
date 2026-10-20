{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- | Direct-write encoder: bypasses ByteString.Builder entirely.

Uses offset-based writes to a pre-allocated buffer. The offset is
an unboxed Int threaded through all operations, avoiding any heap
allocation for the write context.

For field writes (tag + value), uses single C FFI calls that
handle the entire field in one shot, minimizing Haskell\/C
call overhead.

@
import Proto.Encode.Direct (directEncode, dWord8, dVarint)

let bytes = directEncode 3 (\\p off -> do
      off1 <- dWord8 p off 0x42
      dVarint p off1 12345)
@
-}
module Wireform.Encode.Direct (
  -- * Core
  directEncode,

  -- * Offset-based write primitives (return new offset)
  dWord8,
  dVarint,
  dWord32LE,
  dWord64LE,
  dFloatLE,
  dDoubleLE,
  dBytes,
  dText,

  -- * Field-level writes (tag + value in one call)
  dVarintField,
  dBoolField,
  dStringField,
  dBytesField,
  dFixed32Field,
  dFixed64Field,
  dFloatField,
  dDoubleField,
) where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Wireform.FFI (encodeBoolFieldC, encodeLengthDelimitedC, encodeVarintFieldC)


{- | Allocate a buffer of exactly @sz@ bytes, run the writer, return ByteString.
The writer takes (Ptr Word8, offset) and returns the final offset.
-}
directEncode :: Int -> (Ptr Word8 -> Int -> IO Int) -> ByteString
directEncode !sz writer = BSI.unsafeCreate sz $ \ptr -> do
  _ <- writer ptr 0
  pure ()
{-# INLINE directEncode #-}


-- Offset-based primitives: take (Ptr, offset), return new offset.

dWord8 :: Ptr Word8 -> Int -> Word8 -> IO Int
dWord8 !p !off !b = do
  pokeByteOff p off b
  pure $! off + 1
{-# INLINE dWord8 #-}


dWord32LE :: Ptr Word8 -> Int -> Word32 -> IO Int
dWord32LE !p !off !v = do
  pokeByteOff p off v
  pure $! off + 4
{-# INLINE dWord32LE #-}


dWord64LE :: Ptr Word8 -> Int -> Word64 -> IO Int
dWord64LE !p !off !v = do
  pokeByteOff p off v
  pure $! off + 8
{-# INLINE dWord64LE #-}


dFloatLE :: Ptr Word8 -> Int -> Float -> IO Int
dFloatLE !p !off !v = dWord32LE p off (castFloatToWord32 v)
{-# INLINE dFloatLE #-}


dDoubleLE :: Ptr Word8 -> Int -> Double -> IO Int
dDoubleLE !p !off !v = dWord64LE p off (castDoubleToWord64 v)
{-# INLINE dDoubleLE #-}


-- | Write a varint at offset, return new offset.
dVarint :: Ptr Word8 -> Int -> Word64 -> IO Int
dVarint !p !off !n
  | n < 0x80 = do
      pokeByteOff p off (fromIntegral n :: Word8)
      pure $! off + 1
  | n < 0x4000 = do
      pokeByteOff p off (fromIntegral (n .&. 0x7F .|. 0x80) :: Word8)
      pokeByteOff p (off + 1) (fromIntegral (n `shiftR` 7) :: Word8)
      pure $! off + 2
  | n < 0x200000 = do
      pokeByteOff p off (fromIntegral (n .&. 0x7F .|. 0x80) :: Word8)
      pokeByteOff p (off + 1) (fromIntegral ((n `shiftR` 7) .&. 0x7F .|. 0x80) :: Word8)
      pokeByteOff p (off + 2) (fromIntegral (n `shiftR` 14) :: Word8)
      pure $! off + 3
  | otherwise = go off n
  where
    -- Values of 2^21 and up are rare (lengths of 2 MiB+), so a plain loop.
    -- Not encodeVarintFieldC: that writes a field tag byte first.
    go !o !v
      | v < 0x80 = do
          pokeByteOff p o (fromIntegral v :: Word8)
          pure $! o + 1
      | otherwise = do
          pokeByteOff p o (fromIntegral (v .&. 0x7F .|. 0x80) :: Word8)
          go (o + 1) (v `shiftR` 7)
{-# INLINE dVarint #-}


-- | Write raw bytes at offset (no length prefix). Returns new offset.
dBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
dBytes !p !off (BSI.BS fp len) = do
  withForeignPtr fp $ \src ->
    copyBytes (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE dBytes #-}


-- | Write text (length-prefixed UTF-8 bytes) at offset.
dText :: Ptr Word8 -> Int -> Text -> IO Int
dText !p !off !t = do
  let !bs = TE.encodeUtf8 t
      !len = BS.length bs
  off1 <- dVarint p off (fromIntegral len)
  dBytes p off1 bs
{-# INLINE dText #-}


-- ============================================================
-- Field-level writes: tag + value in minimal calls
-- ============================================================

-- | Varint field: tag_byte + varint(value).
dVarintField :: Ptr Word8 -> Int -> Word8 -> Word64 -> IO Int
dVarintField !p !off !tag !val = do
  n <- encodeVarintFieldC p off tag val
  pure $! off + n
{-# INLINE dVarintField #-}


-- | Bool field: tag_byte + 0/1. Always 2 bytes.
dBoolField :: Ptr Word8 -> Int -> Word8 -> Bool -> IO Int
dBoolField !p !off !tag !val = do
  _ <- encodeBoolFieldC p off tag val
  pure $! off + 2
{-# INLINE dBoolField #-}


-- | String field: tag + varint(len) + UTF-8 bytes. Single C call.
dStringField :: Ptr Word8 -> Int -> Word8 -> Text -> IO Int
dStringField !p !off !tag !t = do
  let !(BSI.BS fp len) = TE.encodeUtf8 t
  withForeignPtr fp $ \src -> do
    n <- encodeLengthDelimitedC p off tag src len
    pure $! off + n
{-# INLINE dStringField #-}


-- | Bytes field: tag + varint(len) + raw bytes. Single C call.
dBytesField :: Ptr Word8 -> Int -> Word8 -> ByteString -> IO Int
dBytesField !p !off !tag (BSI.BS fp len) = do
  withForeignPtr fp $ \src -> do
    n <- encodeLengthDelimitedC p off tag src len
    pure $! off + n
{-# INLINE dBytesField #-}


-- | Fixed32 field: tag + 4 LE bytes. Always 5 bytes.
dFixed32Field :: Ptr Word8 -> Int -> Word8 -> Word32 -> IO Int
dFixed32Field !p !off !tag !val = do
  pokeByteOff p off tag
  pokeByteOff p (off + 1) val
  pure $! off + 5
{-# INLINE dFixed32Field #-}


-- | Fixed64 field: tag + 8 LE bytes. Always 9 bytes.
dFixed64Field :: Ptr Word8 -> Int -> Word8 -> Word64 -> IO Int
dFixed64Field !p !off !tag !val = do
  pokeByteOff p off tag
  pokeByteOff p (off + 1) val
  pure $! off + 9
{-# INLINE dFixed64Field #-}


-- | Float field: tag + 4 LE bytes.
dFloatField :: Ptr Word8 -> Int -> Word8 -> Float -> IO Int
dFloatField !p !off !tag !val = dFixed32Field p off tag (castFloatToWord32 val)
{-# INLINE dFloatField #-}


-- | Double field: tag + 8 LE bytes.
dDoubleField :: Ptr Word8 -> Int -> Word8 -> Double -> IO Int
dDoubleField !p !off !tag !val = dFixed64Field p off tag (castDoubleToWord64 val)
{-# INLINE dDoubleField #-}
