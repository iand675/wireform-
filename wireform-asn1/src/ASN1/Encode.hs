{-# LANGUAGE BangPatterns #-}
-- | ASN.1 DER (Distinguished Encoding Rules) encoder.
--
-- Encodes an 'ASN1.Value.Value' using ITU-T X.690 Distinguished Encoding
-- Rules. DER is the canonical subset of BER that produces a unique
-- encoding for each value. Tag-length-value (TLV) triplets are emitted
-- with definite-length encoding.
--
-- Uses direct buffer writes via 'Proto.Encode.Direct.directEncode'.
module ASN1.Encode
  ( encode
  ) where

import Data.Bits (shiftR, shiftL, (.&.), (.|.), testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.List (sortBy)
import Data.Word (Word8, Word64)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)

import ASN1.Value
import Wireform.Encode.Direct (directEncode)

encode :: Value -> ByteString
encode val = directEncode (asn1Size val) (writeASN1 val)
{-# INLINE encode #-}

-- Size computation

asn1Size :: Value -> Int
asn1Size = \case
  Boolean _ -> 1 + 1 + 1
  Integer n -> let !content = encodeInteger n in 1 + lengthOfLength (BS.length content) + BS.length content
  BitString _unused dat ->
    let !contentLen = 1 + BS.length dat
    in 1 + lengthOfLength contentLen + contentLen
  OctetString bs -> 1 + lengthOfLength (BS.length bs) + BS.length bs
  Null -> 2
  OID components -> let !content = encodeOID components in 1 + lengthOfLength (BS.length content) + BS.length content
  UTF8String t -> let !bs = TE.encodeUtf8 t in 1 + lengthOfLength (BS.length bs) + BS.length bs
  PrintableString t -> let !bs = TE.encodeUtf8 t in 1 + lengthOfLength (BS.length bs) + BS.length bs
  IA5String t -> let !bs = TE.encodeUtf8 t in 1 + lengthOfLength (BS.length bs) + BS.length bs
  UTCTime t -> let !bs = TE.encodeUtf8 t in 1 + lengthOfLength (BS.length bs) + BS.length bs
  GeneralizedTime t -> let !bs = TE.encodeUtf8 t in 1 + lengthOfLength (BS.length bs) + BS.length bs
  Sequence vs ->
    let !innerLen = V.foldl' (\s v -> s + asn1Size v) 0 vs
    in 1 + lengthOfLength innerLen + innerLen
  Set vs ->
    let !innerLen = V.foldl' (\s v -> s + asn1Size v) 0 vs
    in 1 + lengthOfLength innerLen + innerLen
  Tagged _tc tagNum v ->
    let !innerLen = asn1Size v
        !tagHdrSz = tagBytesSize tagNum
    in tagHdrSz + lengthOfLength innerLen + innerLen
  Other _tc _constructed tagNum raw ->
    let !tagHdrSz = tagBytesSize tagNum
    in tagHdrSz + lengthOfLength (BS.length raw) + BS.length raw

lengthOfLength :: Int -> Int
lengthOfLength n
  | n < 128   = 1
  | n < 256   = 2
  | n < 65536 = 3
  | otherwise = let !bs = encodeNonNegativeInteger (fromIntegral n) in 1 + BS.length bs
{-# INLINE lengthOfLength #-}

tagBytesSize :: Int -> Int
tagBytesSize tagNum
  | tagNum < 31 = 1
  | otherwise    = 1 + BS.length (encodeBase128 (fromIntegral tagNum))
{-# INLINE tagBytesSize #-}

-- Offset-based writer

writeASN1 :: Value -> Ptr Word8 -> Int -> IO Int
writeASN1 val p off = writeValueDER val p off

writeValueDER :: Value -> Ptr Word8 -> Int -> IO Int
writeValueDER val p off = case val of
  Boolean b -> do
    pokeByteOff p off (0x01 :: Word8)
    pokeByteOff p (off + 1) (0x01 :: Word8)
    pokeByteOff p (off + 2) (if b then 0xFF :: Word8 else 0x00)
    pure $! off + 3

  Integer n -> writeTLV p off 0x02 (encodeInteger n)

  BitString unused dat -> do
    -- DER §8.6.2.3: an empty bit string must use 'unused == 0'.
    -- Normalise rather than emitting an ill-formed TLV when callers
    -- pass a non-zero unused with empty data.
    let !normalisedUnused = if BS.null dat then 0 else unused
        !contentLen = 1 + BS.length dat
    pokeByteOff p off (0x03 :: Word8)
    off1 <- writeLength p (off + 1) contentLen
    pokeByteOff p off1 (fromIntegral normalisedUnused :: Word8)
    writeRaw p (off1 + 1) dat

  OctetString bs -> writeTLV p off 0x04 bs

  Null -> do
    pokeByteOff p off (0x05 :: Word8)
    pokeByteOff p (off + 1) (0x00 :: Word8)
    pure $! off + 2

  OID components -> writeTLV p off 0x06 (encodeOID components)
  UTF8String t -> writeTLV p off 0x0C (TE.encodeUtf8 t)
  PrintableString t -> writeTLV p off 0x13 (TE.encodeUtf8 t)
  IA5String t -> writeTLV p off 0x16 (TE.encodeUtf8 t)
  UTCTime t -> writeTLV p off 0x17 (TE.encodeUtf8 t)
  GeneralizedTime t -> writeTLV p off 0x18 (TE.encodeUtf8 t)

  Sequence vs -> writeConstructed p off 0x30 vs
  -- DER §11.5 / §11.6: SET (and SET OF) elements must be written in
  -- ascending byte order of their full DER encodings, with the
  -- shorter encoding padded at its trailing end with 0-octets when
  -- comparing. Two encodings of the same length sort by raw byte
  -- comparison, which is what 'compare' on 'ByteString' does. The
  -- shorter-padded-with-zeros rule turns out to be equivalent to
  -- normal lexicographic compare on the un-padded bytes (because
  -- appending zeros to a shorter prefix can never make it greater
  -- than the longer string at the diverging position).
  Set vs -> writeConstructedSorted p off 0x31 vs

  Tagged tc tagNum v -> do
    let !innerLen = asn1Size v
        !tagByte = encodeTagByte tc True tagNum
    off1 <- writeTagBytes p off tagByte tagNum
    off2 <- writeLength p off1 innerLen
    writeValueDER v p off2

  Other tc constructed tagNum raw -> do
    let !tagByte = encodeTagByte tc constructed tagNum
    off1 <- writeTagBytes p off tagByte tagNum
    off2 <- writeLength p off1 (BS.length raw)
    writeRaw p off2 raw

writeTLV :: Ptr Word8 -> Int -> Word8 -> ByteString -> IO Int
writeTLV p off tag content = do
  pokeByteOff p off tag
  off1 <- writeLength p (off + 1) (BS.length content)
  writeRaw p off1 content
{-# INLINE writeTLV #-}

writeConstructed :: Ptr Word8 -> Int -> Word8 -> V.Vector Value -> IO Int
writeConstructed p off tag vs = do
  let !innerLen = V.foldl' (\s v -> s + asn1Size v) 0 vs
  pokeByteOff p off tag
  off1 <- writeLength p (off + 1) innerLen
  V.foldM' (\o v -> writeValueDER v p o) off1 vs

-- | Like 'writeConstructed' but sorts the children by their full DER
-- encoding before emitting them. Required for SET / SET OF per
-- ITU-T X.690 §11.5–11.6.
writeConstructedSorted :: Ptr Word8 -> Int -> Word8 -> V.Vector Value -> IO Int
writeConstructedSorted p off tag vs = do
  -- Encode each child once into its standalone DER bytes; sort by the
  -- raw byte representation; emit the precomputed bytes in order. This
  -- avoids encoding twice (once for sort comparison, once for output).
  let !encoded = V.map encode vs
      !sorted  = V.fromList (sortBy compare (V.toList encoded))
      !innerLen = V.foldl' (\s b -> s + BS.length b) 0 sorted
  pokeByteOff p off tag
  off1 <- writeLength p (off + 1) innerLen
  V.foldM' (\o b -> writeRaw p o b) off1 sorted

writeLength :: Ptr Word8 -> Int -> Int -> IO Int
writeLength p off n
  | n < 128 = do
      pokeByteOff p off (fromIntegral n :: Word8)
      pure $! off + 1
  | otherwise = do
      let !bs = encodeNonNegativeInteger (fromIntegral n)
          !numBytes = BS.length bs
      pokeByteOff p off (0x80 .|. fromIntegral numBytes :: Word8)
      writeRaw p (off + 1) bs
{-# INLINE writeLength #-}

writeTagBytes :: Ptr Word8 -> Int -> Word8 -> Int -> IO Int
writeTagBytes p off tagByte tagNum
  | tagNum < 31 = do
      pokeByteOff p off tagByte
      pure $! off + 1
  | otherwise = do
      pokeByteOff p off tagByte
      writeRaw p (off + 1) (encodeBase128 (fromIntegral tagNum))
{-# INLINE writeTagBytes #-}

writeRaw :: Ptr Word8 -> Int -> ByteString -> IO Int
writeRaw p off (BSI.BS fp len) = do
  withForeignPtr fp $ \src -> BSI.memcpy (p `plusPtr` off) src len
  pure $! off + len
{-# INLINE writeRaw #-}

-- Pure helpers (kept from original)

encodeNonNegativeInteger :: Integer -> ByteString
encodeNonNegativeInteger 0 = BS.singleton 0
encodeNonNegativeInteger n = BS.pack (go n [])
  where
    go 0 acc = acc
    go v acc = go (v `shiftR` 8) (fromIntegral (v .&. 0xFF) : acc)

encodeInteger :: Integer -> ByteString
encodeInteger n = BS.pack (integerToTwosComplement n)

integerToTwosComplement :: Integer -> [Word8]
integerToTwosComplement 0 = [0]
integerToTwosComplement n
  | n > 0 =
      let bytes = toBytesBE n []
      in case bytes of
           (b:_) | testBit b 7 -> 0x00 : bytes
           _ -> bytes
  | otherwise =
      let !posVal = abs n
          !numBytes = byteWidth posVal
          !twosComp = (1 `shiftL` (8 * numBytes)) - posVal
          bytes = intToBE twosComp numBytes
      in trimFF bytes
  where
    toBytesBE 0 acc = acc
    toBytesBE v acc = toBytesBE (v `shiftR` 8) (fromIntegral (v .&. 0xFF) : acc)

    byteWidth v = go 1
      where go k = if v <= (1 `shiftL` (8 * k - 1)) then k else go (k + 1)

    intToBE val' k = [fromIntegral ((val' `shiftR` (8 * (k - 1 - i))) .&. 0xFF) | i <- [0 .. k - 1]]

    trimFF (0xFF : b : rest)
      | testBit b 7 = trimFF (b : rest)
    trimFF bs = bs

-- | Encode an OID per X.690 \xC2\xA78.19.
--
-- The first sub-identifier is @40 \xC3\x97 X1 + X2@, encoded in the same
-- base-128 form as every other component (NOT a raw byte: the
-- combined value can exceed 255 if @X2 >= 80@, which is common in
-- joint-iso-itu-t arcs).  Subsequent components encode independently.
encodeOID :: V.Vector Word64 -> ByteString
encodeOID components
  | V.length components < 2 = BS.empty
  | otherwise =
      let !first = V.unsafeIndex components 0
          !second = V.unsafeIndex components 1
          !combined = first * 40 + second
          !firstEncoded = encodeOIDComponent combined
          !rest = V.toList (V.drop 2 components)
      in firstEncoded <> mconcat (map encodeOIDComponent rest)

encodeOIDComponent :: Word64 -> ByteString
encodeOIDComponent n
  | n < 128 = BS.singleton (fromIntegral n)
  | otherwise = BS.pack (go n [])
  where
    go 0 acc = acc
    go v [] = go (v `shiftR` 7) [fromIntegral (v .&. 0x7F)]
    go v acc = go (v `shiftR` 7) (fromIntegral ((v .&. 0x7F) .|. 0x80) : acc)

encodeTagByte :: TagClass -> Bool -> Int -> Word8
encodeTagByte tc constructed tagNum =
  let !classBits = case tc of
        Universal       -> 0x00
        Application     -> 0x40
        ContextSpecific -> 0x80
        Private         -> 0xC0
      !consBit = if constructed then 0x20 else 0x00
      !tagBits = if tagNum < 31 then fromIntegral tagNum else 0x1F
  in classBits .|. consBit .|. tagBits

encodeBase128 :: Word64 -> ByteString
encodeBase128 n
  | n < 128 = BS.singleton (fromIntegral n)
  | otherwise = BS.pack (go n [])
  where
    go 0 acc = acc
    go v [] = go (v `shiftR` 7) [fromIntegral (v .&. 0x7F)]
    go v acc = go (v `shiftR` 7) (fromIntegral ((v .&. 0x7F) .|. 0x80) : acc)
