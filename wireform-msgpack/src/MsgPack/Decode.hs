{-# LANGUAGE BangPatterns #-}
-- | MessagePack binary decoding.
--
-- Decodes a wire-format 'ByteString' into a 'MsgPack.Value.Value' tree
-- using pre-allocated mutable vectors instead of list accumulation.
-- Uses 'Ptr'-based unsafe indexing for performance on validated input.
--
-- @
-- import qualified MsgPack.Decode as MPD
--
-- case MPD.decode bytes of
--   Right val -> print val
--   Left err  -> putStrLn err
-- @
module MsgPack.Decode
  ( decode
  ) where

import Control.Monad.ST (stToIO)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word32, Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekByteOff)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified MsgPack.Value as MV'
import Wireform.FFI (decodeTextFast)

-- | Decode a MessagePack binary into a Value.
decode :: ByteString -> Either String MV'.Value
decode bs
  | BS.null bs = Left "MsgPack.Decode: empty input"
  | otherwise  = unsafeDupablePerformIO $ withBSPtr bs $ \ptr -> do
      let !len = BS.length bs
      result <- decodeValue ptr len 0 bs
      pure $! case result of
        Left e -> Left e
        Right (v, off)
          | off == len -> Right v
          | otherwise  -> Left $ "MsgPack.Decode: " ++ show (len - off) ++ " trailing bytes"

withBSPtr :: ByteString -> (Ptr Word8 -> IO a) -> IO a
withBSPtr (BSI.BS fp _) f = withForeignPtr fp (f . castPtr)

peekAt :: Ptr Word8 -> Int -> IO Word8
peekAt p off = peekByteOff p off
{-# INLINE peekAt #-}

readBE16 :: Ptr Word8 -> Int -> IO Word64
readBE16 p off = do
  b0 <- peekAt p off
  b1 <- peekAt p (off + 1)
  pure $! (fromIntegral b0 `shiftL` 8) .|. fromIntegral b1
{-# INLINE readBE16 #-}

readBE32 :: Ptr Word8 -> Int -> IO Word64
readBE32 p off = do
  b0 <- peekAt p off
  b1 <- peekAt p (off + 1)
  b2 <- peekAt p (off + 2)
  b3 <- peekAt p (off + 3)
  pure $! (fromIntegral b0 `shiftL` 24)
      .|. (fromIntegral b1 `shiftL` 16)
      .|. (fromIntegral b2 `shiftL` 8)
      .|. fromIntegral b3
{-# INLINE readBE32 #-}

readBE64 :: Ptr Word8 -> Int -> IO Word64
readBE64 p off = do
  b0 <- peekAt p off
  b1 <- peekAt p (off + 1)
  b2 <- peekAt p (off + 2)
  b3 <- peekAt p (off + 3)
  b4 <- peekAt p (off + 4)
  b5 <- peekAt p (off + 5)
  b6 <- peekAt p (off + 6)
  b7 <- peekAt p (off + 7)
  pure $! (fromIntegral b0 `shiftL` 56)
      .|. (fromIntegral b1 `shiftL` 48)
      .|. (fromIntegral b2 `shiftL` 40)
      .|. (fromIntegral b3 `shiftL` 32)
      .|. (fromIntegral b4 `shiftL` 24)
      .|. (fromIntegral b5 `shiftL` 16)
      .|. (fromIntegral b6 `shiftL` 8)
      .|. fromIntegral b7
{-# INLINE readBE64 #-}

type DecResult = IO (Either String (MV'.Value, Int))

decodeValue :: Ptr Word8 -> Int -> Int -> ByteString -> DecResult
decodeValue p len off origBs
  | off >= len = pure $ Left "MsgPack.Decode: unexpected end of input"
  | otherwise = do
      b <- peekAt p off
      let !off1 = off + 1
      dispatch p len off1 b origBs
{-# INLINE decodeValue #-}

dispatch :: Ptr Word8 -> Int -> Int -> Word8 -> ByteString -> DecResult
dispatch p len off b origBs

  -- positive fixint 0x00-0x7f
  | b <= 0x7f = pure $ Right (MV'.Word (fromIntegral b), off)

  -- fixmap 0x80-0x8f
  | b >= 0x80 && b <= 0x8f = do
      let !cnt = fromIntegral (b .&. 0x0f)
      decodeMapN p len off cnt origBs

  -- fixarray 0x90-0x9f
  | b >= 0x90 && b <= 0x9f = do
      let !cnt = fromIntegral (b .&. 0x0f)
      decodeArrayN p len off cnt origBs

  -- fixstr 0xa0-0xbf
  | b >= 0xa0 && b <= 0xbf = do
      let !slen = fromIntegral (b .&. 0x1f)
      decodeStr p len off slen origBs

  -- nil
  | b == 0xc0 = pure $ Right (MV'.Nil, off)

  -- (never used) 0xc1
  | b == 0xc1 = pure $ Left "MsgPack.Decode: reserved byte 0xc1"

  -- false
  | b == 0xc2 = pure $ Right (MV'.Bool False, off)

  -- true
  | b == 0xc3 = pure $ Right (MV'.Bool True, off)

  -- bin8
  | b == 0xc4 = requireBytes p len off 1 $ do
      blen <- fromIntegral <$> peekAt p off
      decodeBin p len (off + 1) blen origBs

  -- bin16
  | b == 0xc5 = requireBytes p len off 2 $ do
      blen <- fromIntegral <$> readBE16 p off
      decodeBin p len (off + 2) blen origBs

  -- bin32
  | b == 0xc6 = requireBytes p len off 4 $ do
      blen <- fromIntegral <$> readBE32 p off
      decodeBin p len (off + 4) blen origBs

  -- ext8
  | b == 0xc7 = requireBytes p len off 2 $ do
      elen <- fromIntegral <$> peekAt p off
      ty   <- peekAt p (off + 1)
      decodeExtData p len (off + 2) (fromIntegral ty :: Int8) elen origBs

  -- ext16
  | b == 0xc8 = requireBytes p len off 3 $ do
      elen <- fromIntegral <$> readBE16 p off
      ty   <- peekAt p (off + 2)
      decodeExtData p len (off + 3) (fromIntegral ty :: Int8) elen origBs

  -- ext32
  | b == 0xc9 = requireBytes p len off 5 $ do
      elen <- fromIntegral <$> readBE32 p off
      ty   <- peekAt p (off + 4)
      decodeExtData p len (off + 5) (fromIntegral ty :: Int8) elen origBs

  -- float32
  | b == 0xca = requireBytes p len off 4 $ do
      w <- readBE32 p off
      let !f = castWord32ToFloat (fromIntegral w)
      pure $ Right (MV'.Float f, off + 4)

  -- float64
  | b == 0xcb = requireBytes p len off 8 $ do
      w <- readBE64 p off
      let !d = castWord64ToDouble w
      pure $ Right (MV'.Double d, off + 8)

  -- uint8
  | b == 0xcc = requireBytes p len off 1 $ do
      v <- peekAt p off
      pure $ Right (MV'.Word (fromIntegral v), off + 1)

  -- uint16
  | b == 0xcd = requireBytes p len off 2 $ do
      v <- readBE16 p off
      pure $ Right (MV'.Word v, off + 2)

  -- uint32
  | b == 0xce = requireBytes p len off 4 $ do
      v <- readBE32 p off
      pure $ Right (MV'.Word v, off + 4)

  -- uint64
  | b == 0xcf = requireBytes p len off 8 $ do
      v <- readBE64 p off
      pure $ Right (MV'.Word v, off + 8)

  -- int8
  | b == 0xd0 = requireBytes p len off 1 $ do
      v <- peekAt p off
      pure $ Right (MV'.Int (fromIntegral (fromIntegral v :: Int8)), off + 1)

  -- int16
  | b == 0xd1 = requireBytes p len off 2 $ do
      v <- readBE16 p off
      let !i = fromIntegral (fromIntegral v :: Int16)
      pure $ Right (MV'.Int i, off + 2)

  -- int32
  | b == 0xd2 = requireBytes p len off 4 $ do
      v <- readBE32 p off
      let !i = fromIntegral (fromIntegral v :: Int32)
      pure $ Right (MV'.Int i, off + 4)

  -- int64
  | b == 0xd3 = requireBytes p len off 8 $ do
      v <- readBE64 p off
      let !i = fromIntegral v :: Int64
      pure $ Right (MV'.Int i, off + 8)

  -- fixext1
  | b == 0xd4 = decodeFixExt p len off 1 origBs
  -- fixext2
  | b == 0xd5 = decodeFixExt p len off 2 origBs
  -- fixext4
  | b == 0xd6 = decodeFixExt p len off 4 origBs
  -- fixext8
  | b == 0xd7 = decodeFixExt p len off 8 origBs
  -- fixext16
  | b == 0xd8 = decodeFixExt p len off 16 origBs

  -- str8
  | b == 0xd9 = requireBytes p len off 1 $ do
      slen <- fromIntegral <$> peekAt p off
      decodeStr p len (off + 1) slen origBs

  -- str16
  | b == 0xda = requireBytes p len off 2 $ do
      slen <- fromIntegral <$> readBE16 p off
      decodeStr p len (off + 2) slen origBs

  -- str32
  | b == 0xdb = requireBytes p len off 4 $ do
      slen <- fromIntegral <$> readBE32 p off
      decodeStr p len (off + 4) slen origBs

  -- array16
  | b == 0xdc = requireBytes p len off 2 $ do
      cnt <- fromIntegral <$> readBE16 p off
      decodeArrayN p len (off + 2) cnt origBs

  -- array32
  | b == 0xdd = requireBytes p len off 4 $ do
      cnt <- fromIntegral <$> readBE32 p off
      decodeArrayN p len (off + 4) cnt origBs

  -- map16
  | b == 0xde = requireBytes p len off 2 $ do
      cnt <- fromIntegral <$> readBE16 p off
      decodeMapN p len (off + 2) cnt origBs

  -- map32
  | b == 0xdf = requireBytes p len off 4 $ do
      cnt <- fromIntegral <$> readBE32 p off
      decodeMapN p len (off + 4) cnt origBs

  -- negative fixint 0xe0-0xff
  | otherwise = pure $ Right (MV'.Int (fromIntegral (fromIntegral b :: Int8)), off)

requireBytes :: Ptr Word8 -> Int -> Int -> Int -> DecResult -> DecResult
requireBytes _ len off need action
  | off + need > len = pure $ Left "MsgPack.Decode: unexpected end of input"
  | otherwise        = action
{-# INLINE requireBytes #-}

decodeStr :: Ptr Word8 -> Int -> Int -> Int -> ByteString -> DecResult
decodeStr _ len off slen origBs
  | slen < 0 = pure $ Left "MsgPack.Decode: string length overflows Int"
  | off + slen > len = pure $ Left "MsgPack.Decode: string truncated"
  | otherwise = do
      let !slice = BSU.unsafeTake slen (BSU.unsafeDrop off origBs)
      case decodeTextFast slice of
        Left _  -> pure $ Left "MsgPack.Decode: invalid UTF-8 in string"
        Right t -> pure $ Right (MV'.String t, off + slen)

decodeBin :: Ptr Word8 -> Int -> Int -> Int -> ByteString -> DecResult
decodeBin _ len off blen origBs
  | blen < 0 = pure $ Left "MsgPack.Decode: binary length overflows Int"
  | off + blen > len = pure $ Left "MsgPack.Decode: binary truncated"
  | otherwise = do
      let !slice = BSU.unsafeTake blen (BSU.unsafeDrop off origBs)
      pure $ Right (MV'.Binary slice, off + blen)

decodeArrayN :: Ptr Word8 -> Int -> Int -> Int -> ByteString -> DecResult
decodeArrayN p len off0 cnt origBs
  | cnt < 0 = pure $ Left "MsgPack.Decode: array length overflows Int"
  | cnt == 0 = pure $ Right (MV'.Array V.empty, off0)
  -- A msgpack value occupies at least one byte. If the declared array
  -- count exceeds the bytes left in the input, allocating an MV.new of
  -- that size is a denial-of-service vector. Cap by remaining-bytes
  -- before trusting the declared length.
  | cnt > len - off0 =
      pure $ Left "MsgPack.Decode: array length exceeds remaining input"
  | otherwise = do
      mv <- stToIO $ MV.new cnt
      go mv 0 off0
  where
    go !mv !i !off
      | i >= cnt = do
          vec <- stToIO $ V.unsafeFreeze mv
          pure $ Right (MV'.Array vec, off)
      | otherwise = do
          r <- decodeValue p len off origBs
          case r of
            Left e       -> pure $ Left e
            Right (v, o) -> do
              stToIO $ MV.unsafeWrite mv i v
              go mv (i + 1) o
{-# INLINE decodeArrayN #-}

decodeMapN :: Ptr Word8 -> Int -> Int -> Int -> ByteString -> DecResult
decodeMapN p len off0 cnt origBs
  | cnt < 0 = pure $ Left "MsgPack.Decode: map length overflows Int"
  | cnt == 0 = pure $ Right (MV'.Map V.empty, off0)
  -- Two bytes minimum per (key, value) pair — guard against
  -- allocation DoS when the declared count vastly exceeds the
  -- bytes left in the input.
  | cnt > (len - off0) `div` 2 =
      pure $ Left "MsgPack.Decode: map length exceeds remaining input"
  | otherwise = do
      mv <- stToIO $ MV.new cnt
      go mv 0 off0
  where
    go !mv !i !off
      | i >= cnt = do
          vec <- stToIO $ V.unsafeFreeze mv
          pure $ Right (MV'.Map vec, off)
      | otherwise = do
          r1 <- decodeValue p len off origBs
          case r1 of
            Left e -> pure $ Left e
            Right (k, o1) -> do
              r2 <- decodeValue p len o1 origBs
              case r2 of
                Left e -> pure $ Left e
                Right (v, o2) -> do
                  stToIO $ MV.unsafeWrite mv i (k, v)
                  go mv (i + 1) o2
{-# INLINE decodeMapN #-}

decodeFixExt :: Ptr Word8 -> Int -> Int -> Int -> ByteString -> DecResult
decodeFixExt p len off elen origBs
  | off + 1 + elen > len = pure $ Left "MsgPack.Decode: fixext truncated"
  | otherwise = do
      tyByte <- peekAt p off
      let !ty = fromIntegral tyByte :: Int8
      decodeExtData p len (off + 1) ty elen origBs

decodeExtData :: Ptr Word8 -> Int -> Int -> Int8 -> Int -> ByteString -> DecResult
decodeExtData _ len off ty elen origBs
  | off + elen > len = pure $ Left "MsgPack.Decode: ext truncated"
  | ty == -1  = decodeTimestamp off elen origBs
  | otherwise = do
      let !slice = BSU.unsafeTake elen (BSU.unsafeDrop off origBs)
      pure $ Right (MV'.Ext ty slice, off + elen)

decodeTimestamp :: Int -> Int -> ByteString -> DecResult
decodeTimestamp off elen origBs = case elen of
  4 -> do
    let !b0 = fromIntegral (BSU.unsafeIndex origBs off) :: Word64
        !b1 = fromIntegral (BSU.unsafeIndex origBs (off + 1)) :: Word64
        !b2 = fromIntegral (BSU.unsafeIndex origBs (off + 2)) :: Word64
        !b3 = fromIntegral (BSU.unsafeIndex origBs (off + 3)) :: Word64
        !secs = (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
    pure $ Right (MV'.Timestamp (fromIntegral secs) 0, off + 4)

  8 -> do
    let !b0 = fromIntegral (BSU.unsafeIndex origBs off) :: Word64
        !b1 = fromIntegral (BSU.unsafeIndex origBs (off + 1)) :: Word64
        !b2 = fromIntegral (BSU.unsafeIndex origBs (off + 2)) :: Word64
        !b3 = fromIntegral (BSU.unsafeIndex origBs (off + 3)) :: Word64
        !b4 = fromIntegral (BSU.unsafeIndex origBs (off + 4)) :: Word64
        !b5 = fromIntegral (BSU.unsafeIndex origBs (off + 5)) :: Word64
        !b6 = fromIntegral (BSU.unsafeIndex origBs (off + 6)) :: Word64
        !b7 = fromIntegral (BSU.unsafeIndex origBs (off + 7)) :: Word64
        !w64 = (b0 `shiftL` 56) .|. (b1 `shiftL` 48) .|. (b2 `shiftL` 40)
           .|. (b3 `shiftL` 32) .|. (b4 `shiftL` 24) .|. (b5 `shiftL` 16)
           .|. (b6 `shiftL` 8) .|. b7
        !nsAdj = w64 `shiftR` 34
        !secs  = w64 .&. 0x3FFFFFFFF
    pure $ Right (MV'.Timestamp (fromIntegral secs) (fromIntegral nsAdj), off + 8)

  12 -> do
    let !b0 = fromIntegral (BSU.unsafeIndex origBs off) :: Word64
        !b1 = fromIntegral (BSU.unsafeIndex origBs (off + 1)) :: Word64
        !b2 = fromIntegral (BSU.unsafeIndex origBs (off + 2)) :: Word64
        !b3 = fromIntegral (BSU.unsafeIndex origBs (off + 3)) :: Word64
        !ns32 = (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
    let !b4 = fromIntegral (BSU.unsafeIndex origBs (off + 4)) :: Word64
        !b5 = fromIntegral (BSU.unsafeIndex origBs (off + 5)) :: Word64
        !b6 = fromIntegral (BSU.unsafeIndex origBs (off + 6)) :: Word64
        !b7 = fromIntegral (BSU.unsafeIndex origBs (off + 7)) :: Word64
        !b8 = fromIntegral (BSU.unsafeIndex origBs (off + 8)) :: Word64
        !b9 = fromIntegral (BSU.unsafeIndex origBs (off + 9)) :: Word64
        !b10 = fromIntegral (BSU.unsafeIndex origBs (off + 10)) :: Word64
        !b11 = fromIntegral (BSU.unsafeIndex origBs (off + 11)) :: Word64
        !sec64 = (b4 `shiftL` 56) .|. (b5 `shiftL` 48) .|. (b6 `shiftL` 40)
             .|. (b7 `shiftL` 32) .|. (b8 `shiftL` 24) .|. (b9 `shiftL` 16)
             .|. (b10 `shiftL` 8) .|. b11
    pure $ Right (MV'.Timestamp (fromIntegral sec64 :: Int64) (fromIntegral ns32 :: Word32), off + 12)

  _ -> pure $ Left $ "MsgPack.Decode: invalid timestamp ext size: " ++ show elen
