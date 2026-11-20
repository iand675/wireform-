{-# LANGUAGE BangPatterns #-}

{- | CBOR (RFC 8949) binary encoding.

Uses the smallest integer form, definite-length arrays and maps, and
big-endian multi-byte values. The ordinary encoder preserves the float
width and map order supplied by the caller.
Built on top of 'Wireform.Encode.Direct.directEncode' for direct buffer writes.

@
import qualified CBOR.Encode as CE
import qualified CBOR.Value as C

let bytes = CE.encode (C.TextString \"hello\")
@
-}
module CBOR.Encode (
  encode,
  encodeDeterministic,
  encodeSequence,
) where

import CBOR.Internal (floatToHalf, halfToFloat)
import CBOR.Value qualified as C
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.List (sortBy)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8, byteSwap16, byteSwap32, byteSwap64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, double2Float, float2Double)
import Wireform.Encode.Direct (directEncode)


-- | Encode a CBOR 'C.Value' to a strict 'ByteString'. Simple codes
-- 24 through 31 are reserved and raise an error.
encode :: C.Value -> ByteString
encode !val =
  let !sz = valueSize val
  in directEncode sz (\p off -> writeValue p off val)


{- | Encode with length-first map key ordering (RFC 8949 Section 4.2.3),
minimal integer widths and the shortest exact floating-point widths.
All NaNs use the half-precision encoding @0xf97e00@. Use 'encode' when
the original floating-point width or NaN payload matters.
-}
encodeDeterministic :: C.Value -> ByteString
encodeDeterministic !val =
  let !sorted = normalizeDeterministic val
      !sz = valueSize sorted
  in directEncode sz (\p off -> writeValue p off sorted)


-- | Encode a sequence of CBOR values (concatenated, no framing; RFC 8742).
encodeSequence :: V.Vector C.Value -> ByteString
encodeSequence !vals =
  let !sz = V.foldl' (\acc v -> acc + valueSize v) 0 vals
  in directEncode sz (\p off -> V.foldM' (\o v -> writeValue p o v) off vals)


normalizeDeterministic :: C.Value -> C.Value
normalizeDeterministic = \case
  C.Map kvs ->
    let sorted = V.fromList $ sortBy cmpEncodedKey $ V.toList $ V.map (\(k, v) -> (normalizeDeterministic k, normalizeDeterministic v)) kvs
    in C.Map sorted
  C.Array vs -> C.Array (V.map normalizeDeterministic vs)
  C.Tag n v -> C.Tag n (normalizeDeterministic v)
  C.Float16 f
    | isNaN f -> canonicalNaN
  C.Float32 f -> narrowFloat f
  C.Float64 d
    | isNaN d -> canonicalNaN
    | let f = double2Float d
    , castDoubleToWord64 (float2Double f) == castDoubleToWord64 d ->
        narrowFloat f
  other -> other


narrowFloat :: Float -> C.Value
narrowFloat f
  | isNaN f = canonicalNaN
  | castFloatToWord32 (halfToFloat (floatToHalf bits)) == bits = C.Float16 f
  | otherwise = C.Float32 f
  where
    bits = castFloatToWord32 f


canonicalNaN :: C.Value
canonicalNaN = C.Float16 (castWord32ToFloat 0x7fc00000)


cmpEncodedKey :: (C.Value, C.Value) -> (C.Value, C.Value) -> Ordering
cmpEncodedKey (k1, _) (k2, _) =
  let !e1 = encode k1
      !e2 = encode k2
      !l1 = BS.length e1
      !l2 = BS.length e2
  in case compare l1 l2 of
       EQ -> compare e1 e2
       other -> other


writeHeader :: Ptr Word8 -> Int -> Word8 -> Word64 -> IO Int
writeHeader !p !off !major !n
  | n <= 23 = do
      pokeByteOff p off (major .|. fromIntegral n :: Word8)
      pure $! off + 1
  | n <= 0xff = do
      pokeByteOff p off (major .|. 24 :: Word8)
      pokeByteOff p (off + 1) (fromIntegral n :: Word8)
      pure $! off + 2
  | n <= 0xffff = do
      pokeByteOff p off (major .|. 25 :: Word8)
      pokeByteOff p (off + 1) (byteSwap16 (fromIntegral n) :: Word16)
      pure $! off + 3
  | n <= 0xffffffff = do
      pokeByteOff p off (major .|. 26 :: Word8)
      pokeByteOff p (off + 1) (byteSwap32 (fromIntegral n) :: Word32)
      pure $! off + 5
  | otherwise = do
      pokeByteOff p off (major .|. 27 :: Word8)
      pokeByteOff p (off + 1) (byteSwap64 n)
      pure $! off + 9
{-# INLINE writeHeader #-}


writeRawBytes :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRawBytes !p !off (BSI.BS fp len) = do
  withForeignPtr fp $ \src ->
    copyBytes (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRawBytes #-}


writeValue :: Ptr Word8 -> Int -> C.Value -> IO Int
writeValue !p !off = \case
  C.UInt n -> writeHeader p off 0x00 n
  C.NInt n -> writeHeader p off 0x20 n
  C.Bool False -> do
    pokeByteOff p off (0xf4 :: Word8)
    pure $! off + 1
  C.Bool True -> do
    pokeByteOff p off (0xf5 :: Word8)
    pure $! off + 1
  C.Null -> do
    pokeByteOff p off (0xf6 :: Word8)
    pure $! off + 1
  C.Undefined -> do
    pokeByteOff p off (0xf7 :: Word8)
    pure $! off + 1
  C.Float16 f -> do
    let !w = castFloatToWord32 f
        !h = floatToHalf w
    pokeByteOff p off (0xf9 :: Word8)
    pokeByteOff p (off + 1) (byteSwap16 h :: Word16)
    pure $! off + 3
  C.Float32 f -> do
    let !w = castFloatToWord32 f
    pokeByteOff p off (0xfa :: Word8)
    pokeByteOff p (off + 1) (byteSwap32 w :: Word32)
    pure $! off + 5
  C.Float64 d -> do
    let !w = castDoubleToWord64 d
    pokeByteOff p off (0xfb :: Word8)
    pokeByteOff p (off + 1) (byteSwap64 w)
    pure $! off + 9
  C.ByteString bs -> do
    let !len = BS.length bs
    off1 <- writeHeader p off 0x40 (fromIntegral len)
    writeRawBytes p off1 bs
  C.TextString t -> do
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs
    off1 <- writeHeader p off 0x60 (fromIntegral len)
    writeRawBytes p off1 bs
  C.Array vec -> do
    let !len = V.length vec
    off1 <- writeHeader p off 0x80 (fromIntegral len)
    V.foldM' (\o v -> writeValue p o v) off1 vec
  C.Map vec -> do
    let !len = V.length vec
    off1 <- writeHeader p off 0xa0 (fromIntegral len)
    V.foldM'
      ( \o (k, v) -> do
          o1 <- writeValue p o k
          writeValue p o1 v
      )
      off1
      vec
  C.Tag tagNum content -> do
    off1 <- writeHeader p off 0xc0 tagNum
    writeValue p off1 content
  C.Simple n
    | n <= 23 -> do
        pokeByteOff p off (0xe0 .|. n :: Word8)
        pure $! off + 1
    | otherwise -> do
        pokeByteOff p off (0xf8 :: Word8)
        pokeByteOff p (off + 1) n
        pure $! off + 2


-- | Compute exact encoded size for pre-allocation.
valueSize :: C.Value -> Int
valueSize = \case
  C.UInt n -> headerSize n
  C.NInt n -> headerSize n
  C.Bool _ -> 1
  C.Null -> 1
  C.Undefined -> 1
  C.Float16 _ -> 3
  C.Float32 _ -> 5
  C.Float64 _ -> 9
  C.ByteString bs -> headerSize (fromIntegral (BS.length bs)) + BS.length bs
  C.TextString t ->
    let !bs = TE.encodeUtf8 t
        !len = BS.length bs
    in headerSize (fromIntegral len) + len
  C.Array vec ->
    headerSize (fromIntegral (V.length vec))
      + V.foldl' (\acc v -> acc + valueSize v) 0 vec
  C.Map vec ->
    headerSize (fromIntegral (V.length vec))
      + V.foldl' (\acc (k, v) -> acc + valueSize k + valueSize v) 0 vec
  C.Tag tagNum content ->
    headerSize tagNum + valueSize content
  C.Simple n
    | n <= 23 -> 1
    | n < 32 -> error "CBOR.Encode: reserved simple value"
    | otherwise -> 2


headerSize :: Word64 -> Int
headerSize !n
  | n <= 23 = 1
  | n <= 0xff = 2
  | n <= 0xffff = 3
  | n <= 0xffffffff = 5
  | otherwise = 9
{-# INLINE headerSize #-}
