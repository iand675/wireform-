{-# LANGUAGE BangPatterns #-}

{- | Direct-to-bytes MessagePack encoding, mirroring aeson's @toEncoding@
approach. An 'Encoding' is a 'Wireform.Builder.Builder' that,
when run, produces one MessagePack item.

The 'Encoding' constructors here are deliberately not a 'Monoid' on
the wire bytes: concatenating two encodings produces a sequence of
two MsgPack items, not a single composite item; use 'array' / 'map_'
to combine.
-}
module MsgPack.Encoding (
  Encoding (..),
  encodingToBuilder,
  encodingToLazyByteString,
  encodingToByteString,

  -- * Item constructors
  nil,
  bool,
  int,
  int8,
  int16,
  int32,
  int64,
  word,
  word8,
  word16,
  word32,
  word64,
  float,
  double,
  string,
  lazyString,
  binary,
  lazyBinary,
  ext,
  timestamp,

  -- * Containers
  array,
  arrayList,
  map_,
  mapList,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (foldl')
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import MsgPack.Timestamp (checkExtension, invalidNanoseconds, timestamp64Word, validNanoseconds)
import Wireform.Builder qualified as BB


-- | A MessagePack encoding represents a single, complete MsgPack item.
newtype Encoding = Encoding {runEncoding :: BB.Builder}


encodingToBuilder :: Encoding -> BB.Builder
encodingToBuilder = runEncoding
{-# INLINE encodingToBuilder #-}


encodingToLazyByteString :: Encoding -> BSL.ByteString
encodingToLazyByteString = BB.toLazyByteString . runEncoding
{-# INLINE encodingToLazyByteString #-}


encodingToByteString :: Encoding -> ByteString
encodingToByteString = BB.toStrictByteString . runEncoding
{-# INLINE encodingToByteString #-}


nil :: Encoding
nil = Encoding (BB.word8 0xc0)


bool :: Bool -> Encoding
bool False = Encoding (BB.word8 0xc2)
bool True = Encoding (BB.word8 0xc3)
{-# INLINE bool #-}


word64 :: Word64 -> Encoding
word64 !n
  | n <= 0x7F = Encoding (BB.word8 (fromIntegral n))
  | n <= 0xFF = Encoding (BB.word8 0xcc <> BB.word8 (fromIntegral n))
  | n <= 0xFFFF = Encoding (BB.word8 0xcd <> BB.word16BE (fromIntegral n))
  | n <= 0xFFFFFFFF = Encoding (BB.word8 0xce <> BB.word32BE (fromIntegral n))
  | otherwise = Encoding (BB.word8 0xcf <> BB.word64BE n)
{-# INLINEABLE word64 #-}


word :: Word -> Encoding
word = word64 . fromIntegral


word8 :: Word8 -> Encoding
word8 = word64 . fromIntegral


word16 :: Word16 -> Encoding
word16 = word64 . fromIntegral


word32 :: Word32 -> Encoding
word32 = word64 . fromIntegral


int64 :: Int64 -> Encoding
int64 !n
  | n >= 0 = word64 (fromIntegral n)
  | n >= -32 = Encoding (BB.word8 (fromIntegral (n :: Int64) :: Word8)) -- negative fixint
  | n >= -128 = Encoding (BB.word8 0xd0 <> BB.int8 (fromIntegral n))
  | n >= -32768 = Encoding (BB.word8 0xd1 <> BB.int16BE (fromIntegral n))
  | n >= -2147483648 = Encoding (BB.word8 0xd2 <> BB.int32BE (fromIntegral n))
  | otherwise = Encoding (BB.word8 0xd3 <> BB.int64BE n)
{-# INLINEABLE int64 #-}


int :: Int -> Encoding
int = int64 . fromIntegral


int8 :: Int8 -> Encoding
int8 = int64 . fromIntegral


int16 :: Int16 -> Encoding
int16 = int64 . fromIntegral


int32 :: Int32 -> Encoding
int32 = int64 . fromIntegral


float :: Float -> Encoding
float !f = Encoding (BB.word8 0xca <> BB.word32BE (castFloatToWord32 f))
{-# INLINE float #-}


double :: Double -> Encoding
double !d = Encoding (BB.word8 0xcb <> BB.word64BE (castDoubleToWord64 d))
{-# INLINE double #-}


string :: T.Text -> Encoding
string !t =
  let !bs = TE.encodeUtf8 t
      !len = BS.length bs
  in Encoding (strHeader len <> BB.byteString bs)
{-# INLINE string #-}


lazyString :: TL.Text -> Encoding
lazyString !t =
  let !bs = TLE.encodeUtf8 t
      !len = fromIntegral (BSL.length bs)
  in Encoding (strHeader len <> foldMap BB.byteString (BSL.toChunks bs))


strHeader :: Int -> BB.Builder
strHeader !len
  | len <= 31 = BB.word8 (0xa0 + fromIntegral len)
  | len <= 0xFF = BB.word8 0xd9 <> BB.word8 (fromIntegral len)
  | len <= 0xFFFF = BB.word8 0xda <> BB.word16BE (fromIntegral len)
  | otherwise = BB.word8 0xdb <> BB.word32BE (fromIntegral len)


binary :: ByteString -> Encoding
binary !bs =
  let !len = BS.length bs
  in Encoding (binHeader len <> BB.byteString bs)
{-# INLINE binary #-}


lazyBinary :: BSL.ByteString -> Encoding
lazyBinary !bs =
  let !len = fromIntegral (BSL.length bs)
  in Encoding (binHeader len <> foldMap BB.byteString (BSL.toChunks bs))


binHeader :: Int -> BB.Builder
binHeader !len
  | len <= 0xFF = BB.word8 0xc4 <> BB.word8 (fromIntegral len)
  | len <= 0xFFFF = BB.word8 0xc5 <> BB.word16BE (fromIntegral len)
  | otherwise = BB.word8 0xc6 <> BB.word32BE (fromIntegral len)


{- | Encode an extension. Type -1 is reserved for timestamps and requires
a valid timestamp payload.
-}
ext :: Int8 -> ByteString -> Encoding
ext ty !bs = checkExtension ty bs $ case len of
  1 -> Encoding (BB.word8 0xd4 <> BB.int8 ty <> BB.byteString bs)
  2 -> Encoding (BB.word8 0xd5 <> BB.int8 ty <> BB.byteString bs)
  4 -> Encoding (BB.word8 0xd6 <> BB.int8 ty <> BB.byteString bs)
  8 -> Encoding (BB.word8 0xd7 <> BB.int8 ty <> BB.byteString bs)
  16 -> Encoding (BB.word8 0xd8 <> BB.int8 ty <> BB.byteString bs)
  _
    | len <= 0xFF -> Encoding (BB.word8 0xc7 <> BB.word8 (fromIntegral len) <> BB.int8 ty <> BB.byteString bs)
    | len <= 0xFFFF -> Encoding (BB.word8 0xc8 <> BB.word16BE (fromIntegral len) <> BB.int8 ty <> BB.byteString bs)
    | otherwise -> Encoding (BB.word8 0xc9 <> BB.word32BE (fromIntegral len) <> BB.int8 ty <> BB.byteString bs)
  where
    !len = BS.length bs


-- | Encode a timestamp. Raises an error if nanoseconds exceed 999999999.
timestamp :: Int64 -> Word32 -> Encoding
timestamp s ns
  | not (validNanoseconds ns) = error invalidNanoseconds
  | ns == 0 && s >= 0 && s <= 0xFFFFFFFF =
      Encoding (BB.word8 0xd6 <> BB.word8 0xff <> BB.word32BE (fromIntegral s))
  | s >= 0 && s <= 0x3FFFFFFFF =
      Encoding (BB.word8 0xd7 <> BB.word8 0xff <> BB.word64BE (timestamp64Word s ns))
  | otherwise =
      Encoding
        ( BB.word8 0xc7
            <> BB.word8 12
            <> BB.word8 0xff
            <> BB.word32BE ns
            <> BB.int64BE s
        )


array :: Foldable f => f Encoding -> Encoding
array xs =
  let !n = length xs
      go b e = b <> runEncoding e
  in Encoding (arrayHeader n <> foldl' go mempty xs)
{-# INLINEABLE array #-}


arrayList :: [Encoding] -> Encoding
arrayList xs =
  let !n = length xs
  in Encoding (arrayHeader n <> mconcat (fmap runEncoding xs))


arrayHeader :: Int -> BB.Builder
arrayHeader !len
  | len <= 15 = BB.word8 (0x90 + fromIntegral len)
  | len <= 0xFFFF = BB.word8 0xdc <> BB.word16BE (fromIntegral len)
  | otherwise = BB.word8 0xdd <> BB.word32BE (fromIntegral len)


map_ :: Foldable f => f (Encoding, Encoding) -> Encoding
map_ kvs =
  let !n = length kvs
      go b (k, v) = b <> runEncoding k <> runEncoding v
  in Encoding (mapHeader n <> foldl' go mempty kvs)
{-# INLINEABLE map_ #-}


mapList :: [(Encoding, Encoding)] -> Encoding
mapList kvs =
  let !n = length kvs
      go (k, v) = runEncoding k <> runEncoding v
  in Encoding (mapHeader n <> mconcat (fmap go kvs))


mapHeader :: Int -> BB.Builder
mapHeader !len
  | len <= 15 = BB.word8 (0x80 + fromIntegral len)
  | len <= 0xFFFF = BB.word8 0xde <> BB.word16BE (fromIntegral len)
  | otherwise = BB.word8 0xdf <> BB.word32BE (fromIntegral len)
