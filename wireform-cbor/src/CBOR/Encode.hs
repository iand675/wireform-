{-# LANGUAGE BangPatterns #-}
-- | CBOR (RFC 8949) binary encoding.
--
-- Uses canonical encoding: smallest integer form, definite-length
-- arrays and maps, big-endian multi-byte integers.
-- Built on top of 'Proto.Encode.Direct.directEncode' for direct buffer writes.
--
-- @
-- import qualified CBOR.Encode as CE
-- import qualified CBOR.Value as C
--
-- let bytes = CE.encode (C.TextString \"hello\")
-- @
module CBOR.Encode
  ( encode
  , encodeDeterministic
  , encodeDeterministicLengthFirst
  , encodeSequence
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.List (sortBy)
import Data.Word (Word8, Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castFloatToWord32, castDoubleToWord64)

import Wireform.Encode.Direct (directEncode)

import qualified CBOR.Value as C

-- | Encode a CBOR 'C.Value' to a strict 'ByteString'.
encode :: C.Value -> ByteString
encode !val =
  let !sz = valueSize val
  in directEncode sz (\p off -> writeValue p off val)

-- | Encode in Deterministic CBOR (RFC 8949 Section 4.2.1):
-- map keys sorted bytewise lexicographically (i.e. unsigned byte
-- comparison) over the deterministic encoding of each key,
-- minimal integer encoding (already done), no indefinite-length (already done).
--
-- This matches the bytewise lexicographic ordering required by RFC 8949 §4.2.1
-- (Core Deterministic Encoding Requirements). Use
-- 'encodeDeterministicLengthFirst' for the older RFC 7049 §3.9
-- length-first ordering when interoperating with CTAP2-style consumers.
encodeDeterministic :: C.Value -> ByteString
encodeDeterministic !val =
  let !sorted = sortMapKeysWith cmpEncodedKeyLex val
      !sz = valueSize sorted
  in directEncode sz (\p off -> writeValue p off sorted)

-- | Encode using RFC 7049 \§3.9 length-first ordering: shorter encoded
-- keys first, then bytewise lexicographic. Retained for compatibility
-- with CTAP2 / older deterministic CBOR consumers.
encodeDeterministicLengthFirst :: C.Value -> ByteString
encodeDeterministicLengthFirst !val =
  let !sorted = sortMapKeysWith cmpEncodedKeyLengthFirst val
      !sz = valueSize sorted
  in directEncode sz (\p off -> writeValue p off sorted)

-- | Encode a sequence of CBOR values (concatenated, no framing; RFC 8742).
encodeSequence :: V.Vector C.Value -> ByteString
encodeSequence !vals =
  let !sz = V.foldl' (\acc v -> acc + valueSize v) 0 vals
  in directEncode sz (\p off -> V.foldM' (\o v -> writeValue p o v) off vals)

sortMapKeysWith
  :: ((C.Value, C.Value) -> (C.Value, C.Value) -> Ordering)
  -> C.Value
  -> C.Value
sortMapKeysWith cmp = go
  where
    go = \case
      C.Map kvs ->
        let !recursed = V.map (\(k, v) -> (go k, go v)) kvs
            !sorted   = V.fromList $ sortBy cmp $ V.toList recursed
        in C.Map sorted
      C.Array vs -> C.Array (V.map go vs)
      C.Tag n v  -> C.Tag n (go v)
      other      -> other

-- | RFC 8949 §4.2.1 bytewise lexicographic comparison of encoded keys.
-- Treats each encoded key as a sequence of unsigned bytes.
cmpEncodedKeyLex :: (C.Value, C.Value) -> (C.Value, C.Value) -> Ordering
cmpEncodedKeyLex (k1, _) (k2, _) = compare (encode k1) (encode k2)

-- | RFC 7049 §3.9 length-first comparison: shorter encoded key first,
-- then bytewise lexicographic.
cmpEncodedKeyLengthFirst :: (C.Value, C.Value) -> (C.Value, C.Value) -> Ordering
cmpEncodedKeyLengthFirst (k1, _) (k2, _) =
  let !e1 = encode k1
      !e2 = encode k2
  in case compare (BS.length e1) (BS.length e2) of
       EQ    -> compare e1 e2
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
    V.foldM' (\o (k, v) -> do
      o1 <- writeValue p o k
      writeValue p o1 v) off1 vec

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
  C.UInt n        -> headerSize n
  C.NInt n        -> headerSize n
  C.Bool _        -> 1
  C.Null          -> 1
  C.Undefined     -> 1
  C.Float16 _     -> 3
  C.Float32 _     -> 5
  C.Float64 _     -> 9
  C.ByteString bs -> headerSize (fromIntegral (BS.length bs)) + BS.length bs
  C.TextString t  ->
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
    | n <= 23   -> 1
    | otherwise -> 2

headerSize :: Word64 -> Int
headerSize !n
  | n <= 23         = 1
  | n <= 0xff       = 2
  | n <= 0xffff     = 3
  | n <= 0xffffffff = 5
  | otherwise       = 9
{-# INLINE headerSize #-}

-- | Convert IEEE 754 single-precision bits to half-precision bits.
floatToHalf :: Word32 -> Word16
floatToHalf !w =
  let !sign32 = w `shiftR` 31
      !expo   = (w `shiftR` 23) .&. 0xff
      !mant   = w .&. 0x7fffff
      !signBit = fromIntegral sign32 `shiftL` 15 :: Word16
  in if expo == 0xff
     then signBit .|. 0x7c00 .|. (if mant /= 0 then 0x0200 else 0)
     else if expo > 142
     then signBit .|. 0x7c00
     else if expo < 113
     then if expo < 103
          then signBit
          else let !m = mant .|. 0x800000
                   !shift = fromIntegral (125 - expo) :: Int
               in signBit .|. fromIntegral ((m `shiftR` shift) .&. 0x03ff)
     else let !hexp = fromIntegral (expo - 112) :: Word16
              !hmant = fromIntegral ((mant `shiftR` 13) .&. 0x03ff) :: Word16
          in signBit .|. (hexp `shiftL` 10) .|. hmant
