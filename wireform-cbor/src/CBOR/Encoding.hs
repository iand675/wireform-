{-# LANGUAGE BangPatterns #-}

{- | Direct-to-bytes CBOR encoding, mirroring aeson's @toEncoding@ approach.

An 'Encoding' is a 'Wireform.Builder.Builder' that, when run,
produces a complete CBOR data item. Composing 'Encoding' values does
not construct an intermediate 'CBOR.Value.Value' tree, so the
@toEncoding@ path of 'CBOR.Class.ToCBOR' avoids the per-field heap
allocation that the @toCBOR@ path needs.

@
import qualified CBOR.Encoding as CE
import qualified Data.ByteString.Lazy as BSL

let bs = CE.encodingToLazyByteString (CE.text \"hello\")
@
-}
module CBOR.Encoding (
  Encoding (..),
  encodingToBuilder,
  encodingToLazyByteString,
  encodingToByteString,

  -- * Item constructors
  unsignedInteger,
  negativeInteger,
  integer,
  word,
  word8,
  word16,
  word32,
  word64,
  int,
  int8,
  int16,
  int32,
  int64,
  bool,
  null_,
  undefined_,
  float16,
  float32,
  float64,
  bytes,
  lazyBytes,
  text,
  lazyText,

  -- * Containers
  array,
  arrayList,
  map_,
  mapList,
  tag,

  -- * Pre-known length helpers
  arrayHeader,
  mapHeader,
) where

import CBOR.Internal (floatToHalf, withInteger)
import Data.Bits ((.|.))
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
import Wireform.Builder qualified as BB


{- | A CBOR encoding represents a single, complete CBOR data item.

Encodings compose through 'array', 'map_', and 'tag' rather than
through a 'Monoid' instance: concatenating two CBOR data items
produces a sequence (RFC 8742) rather than a single item, so
combining with @<>@ would silently drift from the AST semantics.
-}
newtype Encoding = Encoding {runEncoding :: BB.Builder}


-- | Render an 'Encoding' as a 'Wireform.Builder.Builder'.
encodingToBuilder :: Encoding -> BB.Builder
encodingToBuilder = runEncoding
{-# INLINE encodingToBuilder #-}


-- | Render an 'Encoding' as a lazy 'BSL.ByteString'.
encodingToLazyByteString :: Encoding -> BSL.ByteString
encodingToLazyByteString = BB.toLazyByteString . runEncoding
{-# INLINE encodingToLazyByteString #-}


-- | Render an 'Encoding' as a strict 'ByteString'.
encodingToByteString :: Encoding -> ByteString
encodingToByteString = BB.toStrictByteString . runEncoding
{-# INLINE encodingToByteString #-}


-- | Common header writer (RFC 8949 \xA73): major type | length, length, ....
header :: Word8 -> Word64 -> BB.Builder
header !major !n
  | n <= 23 = BB.word8 (major .|. fromIntegral n)
  | n <= 0xff = BB.word8 (major .|. 24) <> BB.word8 (fromIntegral n)
  | n <= 0xffff = BB.word8 (major .|. 25) <> BB.word16BE (fromIntegral n)
  | n <= 0xffffffff = BB.word8 (major .|. 26) <> BB.word32BE (fromIntegral n)
  | otherwise = BB.word8 (major .|. 27) <> BB.word64BE n
{-# INLINE header #-}


-- | Major type 0: unsigned integer.
unsignedInteger :: Word64 -> Encoding
unsignedInteger !n = Encoding (header 0x00 n)
{-# INLINE unsignedInteger #-}


{- | Major type 1: negative integer; @n@ is the @-1 - x@ encoding (i.e.
the same shape as 'CBOR.Value.NInt').
-}
negativeInteger :: Word64 -> Encoding
negativeInteger !n = Encoding (header 0x20 n)
{-# INLINE negativeInteger #-}


-- | Encode an integer, using a bignum tag beyond the basic 64-bit range.
integer :: Integer -> Encoding
integer n = withInteger n unsignedInteger negativeInteger (\t magnitude -> tag t (bytes magnitude))
{-# INLINEABLE integer #-}


word :: Word -> Encoding
word = unsignedInteger . fromIntegral


word8 :: Word8 -> Encoding
word8 = unsignedInteger . fromIntegral


word16 :: Word16 -> Encoding
word16 = unsignedInteger . fromIntegral


word32 :: Word32 -> Encoding
word32 = unsignedInteger . fromIntegral


word64 :: Word64 -> Encoding
word64 = unsignedInteger


int :: Int -> Encoding
int n
  | n >= 0 = unsignedInteger (fromIntegral n)
  | otherwise = negativeInteger (fromIntegral (negate n - 1))


int8 :: Int8 -> Encoding
int8 = int . fromIntegral


int16 :: Int16 -> Encoding
int16 = int . fromIntegral


int32 :: Int32 -> Encoding
int32 = int . fromIntegral


int64 :: Int64 -> Encoding
int64 n
  | n >= 0 = unsignedInteger (fromIntegral n)
  | otherwise = negativeInteger (fromIntegral (negate n - 1))


bool :: Bool -> Encoding
bool False = Encoding (BB.word8 0xf4)
bool True = Encoding (BB.word8 0xf5)
{-# INLINE bool #-}


null_ :: Encoding
null_ = Encoding (BB.word8 0xf6)


undefined_ :: Encoding
undefined_ = Encoding (BB.word8 0xf7)


float16 :: Float -> Encoding
float16 !f =
  let !w = castFloatToWord32 f
      !h = floatToHalf w
  in Encoding (BB.word8 0xf9 <> BB.word16BE h)


float32 :: Float -> Encoding
float32 !f = Encoding (BB.word8 0xfa <> BB.word32BE (castFloatToWord32 f))
{-# INLINE float32 #-}


float64 :: Double -> Encoding
float64 !d = Encoding (BB.word8 0xfb <> BB.word64BE (castDoubleToWord64 d))
{-# INLINE float64 #-}


bytes :: ByteString -> Encoding
bytes !bs = Encoding (header 0x40 (fromIntegral (BS.length bs)) <> BB.byteString bs)
{-# INLINE bytes #-}


lazyBytes :: BSL.ByteString -> Encoding
lazyBytes !bs = Encoding (header 0x40 (fromIntegral (BSL.length bs)) <> foldMap BB.byteString (BSL.toChunks bs))


text :: T.Text -> Encoding
text !t =
  let !bs = TE.encodeUtf8 t
  in Encoding (header 0x60 (fromIntegral (BS.length bs)) <> BB.byteString bs)
{-# INLINE text #-}


lazyText :: TL.Text -> Encoding
lazyText !t =
  let !bs = TLE.encodeUtf8 t
  in Encoding (header 0x60 (fromIntegral (BSL.length bs)) <> foldMap BB.byteString (BSL.toChunks bs))


-- | Definite-length array of pre-built encodings (vector-friendly).
array :: Foldable f => f Encoding -> Encoding
array xs =
  let !n = length xs
      go b e = b <> runEncoding e
  in Encoding (header 0x80 (fromIntegral n) <> foldl' go mempty xs)
{-# INLINEABLE array #-}


-- | Specialised list overload: avoids a re-traversal for length.
arrayList :: [Encoding] -> Encoding
arrayList xs =
  let !n = length xs
  in Encoding (header 0x80 (fromIntegral n) <> mconcat (fmap runEncoding xs))


-- | Definite-length map of pre-built (key, value) encodings.
map_ :: Foldable f => f (Encoding, Encoding) -> Encoding
map_ kvs =
  let !n = length kvs
      go b (k, v) = b <> runEncoding k <> runEncoding v
  in Encoding (header 0xa0 (fromIntegral n) <> foldl' go mempty kvs)
{-# INLINEABLE map_ #-}


mapList :: [(Encoding, Encoding)] -> Encoding
mapList kvs =
  let !n = length kvs
      go (k, v) = runEncoding k <> runEncoding v
  in Encoding (header 0xa0 (fromIntegral n) <> mconcat (fmap go kvs))


tag :: Word64 -> Encoding -> Encoding
tag !t !inner = Encoding (header 0xc0 t <> runEncoding inner)
{-# INLINE tag #-}


{- | Emit just the array header for an array of length @n@. The caller
is responsible for emitting exactly @n@ encodings afterwards. Useful
when the caller wants to stream elements without first computing
a list.
-}
arrayHeader :: Word64 -> BB.Builder
arrayHeader = header 0x80


-- | Emit just the map header for a map of @n@ pairs.
mapHeader :: Word64 -> BB.Builder
mapHeader = header 0xa0
