{-# LANGUAGE BangPatterns #-}
-- | MessagePack binary encoding.
--
-- Encodes a 'MsgPack.Value.Value' tree into its wire-format 'ByteString'
-- using a two-pass strategy: compute the exact size, then direct-write into
-- a pre-allocated buffer via 'Wireform.Encode.Direct.directEncode'.
--
-- @
-- import qualified MsgPack.Encode as MPE
-- import qualified MsgPack.Value as MP
--
-- let bytes = MPE.encode (MP.String \"hello\")
-- @
module MsgPack.Encode
  ( encode
  ) where

import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int8, Int64)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64)
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

import Wireform.Encode.Direct (directEncode, dWord8)
import qualified MsgPack.Value as MV

-- | Encode a MessagePack value to its binary wire format.
encode :: MV.Value -> ByteString
encode !val = directEncode (valueSize val) (writeValue val)
{-# NOINLINE encode #-}

--------------------------------------------------------------------------------
-- Size computation
--------------------------------------------------------------------------------

valueSize :: MV.Value -> Int
valueSize MV.Nil           = 1
valueSize (MV.Bool _)      = 1
valueSize (MV.Int n)       = intSize n
valueSize (MV.Word n)      = wordSize n
valueSize (MV.Float _)     = 5
valueSize (MV.Double _)    = 9
valueSize (MV.String t)    = strSize (TE.encodeUtf8 t)
valueSize (MV.Binary bs)   = binSize bs
valueSize (MV.Array vs)    = arraySize vs
valueSize (MV.Map kvs)     = mapSize kvs
valueSize (MV.Ext _ bs)    = extSize bs
valueSize (MV.Timestamp s ns) = timestampSize s ns

intSize :: Int64 -> Int
intSize n
  | n >= 0              = wordSize (fromIntegral n)
  | n >= -32            = 1          -- negative fixint
  | n >= -128           = 2          -- int8
  | n >= -32768         = 3          -- int16
  | n >= -2147483648    = 5          -- int32
  | otherwise           = 9          -- int64

wordSize :: Word64 -> Int
wordSize n
  | n <= 0x7F    = 1   -- positive fixint
  | n <= 0xFF    = 2   -- uint8
  | n <= 0xFFFF  = 3   -- uint16
  | n <= 0xFFFFFFFF = 5 -- uint32
  | otherwise    = 9   -- uint64

strSize :: ByteString -> Int
strSize bs
  | len <= 31    = 1 + len         -- fixstr
  | len <= 0xFF  = 2 + len         -- str8
  | len <= 0xFFFF = 3 + len        -- str16
  | otherwise    = 5 + len         -- str32
  where !len = BS.length bs

binSize :: ByteString -> Int
binSize bs
  | len <= 0xFF   = 2 + len        -- bin8
  | len <= 0xFFFF = 3 + len        -- bin16
  | otherwise     = 5 + len        -- bin32
  where !len = BS.length bs

arraySize :: V.Vector MV.Value -> Int
arraySize vs
  | len <= 15    = 1 + elemsSize    -- fixarray
  | len <= 0xFFFF = 3 + elemsSize   -- array16
  | otherwise    = 5 + elemsSize    -- array32
  where
    !len = V.length vs
    !elemsSize = V.foldl' (\acc v -> acc + valueSize v) 0 vs

mapSize :: V.Vector (MV.Value, MV.Value) -> Int
mapSize kvs
  | len <= 15    = 1 + entriesSize  -- fixmap
  | len <= 0xFFFF = 3 + entriesSize -- map16
  | otherwise    = 5 + entriesSize  -- map32
  where
    !len = V.length kvs
    !entriesSize = V.foldl' (\acc (k, v) -> acc + valueSize k + valueSize v) 0 kvs

extSize :: ByteString -> Int
extSize bs = case len of
  1  -> 2 + len   -- fixext1:  0xd4 + type + 1
  2  -> 2 + len   -- fixext2:  0xd5 + type + 2
  4  -> 2 + len   -- fixext4:  0xd6 + type + 4
  8  -> 2 + len   -- fixext8:  0xd7 + type + 8
  16 -> 2 + len   -- fixext16: 0xd8 + type + 16
  _
    | len <= 0xFF   -> 3 + len   -- ext8
    | len <= 0xFFFF -> 4 + len   -- ext16
    | otherwise     -> 6 + len   -- ext32
  where !len = BS.length bs

timestampSize :: Int64 -> Word32 -> Int
timestampSize s ns
  | ns == 0 && s >= 0 && s <= 0xFFFFFFFF = 2 + 4    -- fixext4: 4 bytes data
  | s >= 0 && s <= 0x3FFFFFFFF           = 2 + 8    -- fixext8: 8 bytes data
  | otherwise                            = 3 + 12   -- ext8 with 12 bytes data

--------------------------------------------------------------------------------
-- Writing
--------------------------------------------------------------------------------

writeValue :: MV.Value -> Ptr Word8 -> Int -> IO Int
writeValue MV.Nil p off = dWord8 p off 0xc0
writeValue (MV.Bool False) p off = dWord8 p off 0xc2
writeValue (MV.Bool True) p off = dWord8 p off 0xc3
writeValue (MV.Int n) p off = writeInt p off n
writeValue (MV.Word n) p off = writeWord p off n
writeValue (MV.Float f) p off = writeFloat p off f
writeValue (MV.Double d) p off = writeDouble p off d
writeValue (MV.String t) p off = writeStr p off (TE.encodeUtf8 t)
writeValue (MV.Binary bs) p off = writeBin p off bs
writeValue (MV.Array vs) p off = writeArray p off vs
writeValue (MV.Map kvs) p off = writeMap p off kvs
writeValue (MV.Ext ty bs) p off = writeExt p off ty bs
writeValue (MV.Timestamp s ns) p off = writeTimestamp p off s ns

writeInt :: Ptr Word8 -> Int -> Int64 -> IO Int
writeInt p off n
  | n >= 0    = writeWord p off (fromIntegral n)
  | n >= -32  = dWord8 p off (fromIntegral n :: Word8)  -- negative fixint
  | n >= -128 = do
      off1 <- dWord8 p off 0xd0
      dWord8 p off1 (fromIntegral n :: Word8)
  | n >= -32768 = do
      off1 <- dWord8 p off 0xd1
      pokeBE16 p off1 (fromIntegral n :: Word64)
  | n >= -2147483648 = do
      off1 <- dWord8 p off 0xd2
      pokeBE32 p off1 (fromIntegral n :: Word64)
  | otherwise = do
      off1 <- dWord8 p off 0xd3
      pokeBE64 p off1 (fromIntegral n :: Word64)

writeWord :: Ptr Word8 -> Int -> Word64 -> IO Int
writeWord p off n
  | n <= 0x7F = dWord8 p off (fromIntegral n :: Word8)  -- positive fixint
  | n <= 0xFF = do
      off1 <- dWord8 p off 0xcc
      dWord8 p off1 (fromIntegral n :: Word8)
  | n <= 0xFFFF = do
      off1 <- dWord8 p off 0xcd
      pokeBE16 p off1 n
  | n <= 0xFFFFFFFF = do
      off1 <- dWord8 p off 0xce
      pokeBE32 p off1 n
  | otherwise = do
      off1 <- dWord8 p off 0xcf
      pokeBE64 p off1 n

writeFloat :: Ptr Word8 -> Int -> Float -> IO Int
writeFloat p off f = do
  off1 <- dWord8 p off 0xca
  let !w = castFloatToWord32 f
  pokeBE32 p off1 (fromIntegral w)

writeDouble :: Ptr Word8 -> Int -> Double -> IO Int
writeDouble p off d = do
  off1 <- dWord8 p off 0xcb
  let !w = castDoubleToWord64 d
  pokeBE64 p off1 w

writeStr :: Ptr Word8 -> Int -> ByteString -> IO Int
writeStr p off bs
  | len <= 31 = do
      off1 <- dWord8 p off (0xa0 + fromIntegral len :: Word8)
      dBytes p off1 bs
  | len <= 0xFF = do
      off1 <- dWord8 p off 0xd9
      off2 <- dWord8 p off1 (fromIntegral len :: Word8)
      dBytes p off2 bs
  | len <= 0xFFFF = do
      off1 <- dWord8 p off 0xda
      off2 <- pokeBE16 p off1 (fromIntegral len)
      dBytes p off2 bs
  | otherwise = do
      off1 <- dWord8 p off 0xdb
      off2 <- pokeBE32 p off1 (fromIntegral len)
      dBytes p off2 bs
  where !len = BS.length bs

writeBin :: Ptr Word8 -> Int -> ByteString -> IO Int
writeBin p off bs
  | len <= 0xFF = do
      off1 <- dWord8 p off 0xc4
      off2 <- dWord8 p off1 (fromIntegral len :: Word8)
      dBytes p off2 bs
  | len <= 0xFFFF = do
      off1 <- dWord8 p off 0xc5
      off2 <- pokeBE16 p off1 (fromIntegral len)
      dBytes p off2 bs
  | otherwise = do
      off1 <- dWord8 p off 0xc6
      off2 <- pokeBE32 p off1 (fromIntegral len)
      dBytes p off2 bs
  where !len = BS.length bs

writeArray :: Ptr Word8 -> Int -> V.Vector MV.Value -> IO Int
writeArray p off vs = do
  off1 <- writeArrayHeader p off len
  V.foldM' (\o v -> writeValue v p o) off1 vs
  where !len = V.length vs

writeArrayHeader :: Ptr Word8 -> Int -> Int -> IO Int
writeArrayHeader p off len
  | len <= 15 = dWord8 p off (0x90 + fromIntegral len :: Word8)
  | len <= 0xFFFF = do
      off1 <- dWord8 p off 0xdc
      pokeBE16 p off1 (fromIntegral len)
  | otherwise = do
      off1 <- dWord8 p off 0xdd
      pokeBE32 p off1 (fromIntegral len)

writeMap :: Ptr Word8 -> Int -> V.Vector (MV.Value, MV.Value) -> IO Int
writeMap p off kvs = do
  off1 <- writeMapHeader p off len
  V.foldM' (\o (k, v) -> do
      o1 <- writeValue k p o
      writeValue v p o1
    ) off1 kvs
  where !len = V.length kvs

writeMapHeader :: Ptr Word8 -> Int -> Int -> IO Int
writeMapHeader p off len
  | len <= 15 = dWord8 p off (0x80 + fromIntegral len :: Word8)
  | len <= 0xFFFF = do
      off1 <- dWord8 p off 0xde
      pokeBE16 p off1 (fromIntegral len)
  | otherwise = do
      off1 <- dWord8 p off 0xdf
      pokeBE32 p off1 (fromIntegral len)

writeExt :: Ptr Word8 -> Int -> Int8 -> ByteString -> IO Int
writeExt p off ty bs = case len of
  1 -> do
    off1 <- dWord8 p off 0xd4
    off2 <- dWord8 p off1 (fromIntegral ty :: Word8)
    dBytes p off2 bs
  2 -> do
    off1 <- dWord8 p off 0xd5
    off2 <- dWord8 p off1 (fromIntegral ty :: Word8)
    dBytes p off2 bs
  4 -> do
    off1 <- dWord8 p off 0xd6
    off2 <- dWord8 p off1 (fromIntegral ty :: Word8)
    dBytes p off2 bs
  8 -> do
    off1 <- dWord8 p off 0xd7
    off2 <- dWord8 p off1 (fromIntegral ty :: Word8)
    dBytes p off2 bs
  16 -> do
    off1 <- dWord8 p off 0xd8
    off2 <- dWord8 p off1 (fromIntegral ty :: Word8)
    dBytes p off2 bs
  _
    | len <= 0xFF -> do
        off1 <- dWord8 p off 0xc7
        off2 <- dWord8 p off1 (fromIntegral len :: Word8)
        off3 <- dWord8 p off2 (fromIntegral ty :: Word8)
        dBytes p off3 bs
    | len <= 0xFFFF -> do
        off1 <- dWord8 p off 0xc8
        off2 <- pokeBE16 p off1 (fromIntegral len)
        off3 <- dWord8 p off2 (fromIntegral ty :: Word8)
        dBytes p off3 bs
    | otherwise -> do
        off1 <- dWord8 p off 0xc9
        off2 <- pokeBE32 p off1 (fromIntegral len)
        off3 <- dWord8 p off2 (fromIntegral ty :: Word8)
        dBytes p off3 bs
  where !len = BS.length bs

writeTimestamp :: Ptr Word8 -> Int -> Int64 -> Word32 -> IO Int
writeTimestamp p off s ns
  -- Timestamp 32: 4 bytes, stores seconds in uint32
  | ns == 0 && s >= 0 && s <= 0xFFFFFFFF = do
      off1 <- dWord8 p off 0xd6      -- fixext4
      off2 <- dWord8 p off1 0xff     -- type = -1
      pokeBE32 p off2 (fromIntegral s)
  -- Timestamp 64: 8 bytes, nanosec-adjustment(30) | seconds(34)
  | s >= 0 && s <= 0x3FFFFFFFF = do
      off1 <- dWord8 p off 0xd7      -- fixext8
      off2 <- dWord8 p off1 0xff     -- type = -1
      let !secHi = (fromIntegral s `shiftR` 32) .&. 0x3 :: Word64
          !secLo = fromIntegral s .&. 0xFFFFFFFF :: Word64
          !w64upper = ((fromIntegral ns :: Word64) .&. 0x3FFFFFFF) * 4 + secHi
          !w64 = w64upper * 0x100000000 + secLo
      pokeBE64 p off2 w64
  -- Timestamp 96: 12 bytes
  | otherwise = do
      off1 <- dWord8 p off 0xc7       -- ext8
      off2 <- dWord8 p off1 12        -- length = 12
      off3 <- dWord8 p off2 0xff      -- type = -1
      pokeBE32 p off3 (fromIntegral ns)
      >>= \off4 -> pokeBE64 p off4 (fromIntegral s)

--------------------------------------------------------------------------------
-- Big-endian write helpers
--------------------------------------------------------------------------------

pokeBE16 :: Ptr Word8 -> Int -> Word64 -> IO Int
pokeBE16 p off w = do
  pokeByteOff p off (byteSwap16 (fromIntegral w) :: Word16)
  pure $! off + 2
{-# INLINE pokeBE16 #-}

pokeBE32 :: Ptr Word8 -> Int -> Word64 -> IO Int
pokeBE32 p off w = do
  pokeByteOff p off (byteSwap32 (fromIntegral w) :: Word32)
  pure $! off + 4
{-# INLINE pokeBE32 #-}

pokeBE64 :: Ptr Word8 -> Int -> Word64 -> IO Int
pokeBE64 p off w = do
  pokeByteOff p off (byteSwap64 w)
  pure $! off + 8
{-# INLINE pokeBE64 #-}

dBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
dBytes !p !off (BSI.BS fp len) = do
  withForeignPtr fp $ \src ->
    copyBytes (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE dBytes #-}
