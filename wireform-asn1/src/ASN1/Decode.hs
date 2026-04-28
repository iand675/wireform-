{-# LANGUAGE BangPatterns #-}
-- | ASN.1 BER decoder (accepts DER-encoded data as well).
--
-- Decodes BER\/DER-encoded 'ByteString' data into an 'ASN1.Value.Value'.
-- Handles both primitive and constructed encodings, definite and
-- indefinite lengths, and all common universal tag types.
module ASN1.Decode
  ( decode
  ) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word64)
import qualified Data.Vector as V

import ASN1.Value

decode :: ByteString -> Either String Value
decode bs
  | BS.null bs = Left "ASN1.Decode: empty input"
  | otherwise = case decodeValue bs 0 of
      Left err     -> Left err
      Right (v, _) -> Right v

-- | Decode a single TLV starting at @off@. Honours BER's indefinite
-- length form ('Length' = 0x80) by collecting children until an
-- end-of-contents marker (a primitive @0x00 0x00@ TLV) is seen.
-- Indefinite length is only valid for /constructed/ encodings; an
-- indefinite length on a primitive type is rejected.
decodeValue :: ByteString -> Int -> Either String (Value, Int)
decodeValue bs off = do
  ensure bs off 1
  let !b0 = rdByte bs off
  (tc, constructed, tagNum, off1) <- decodeTag bs off b0
  (mLen, off2) <- decodeLengthMaybe bs off1
  case mLen of
    Just len -> do
      let !valueEnd = off2 + len
      ensure bs off2 len
      let !contentSlice = BSU.unsafeTake len (BSU.unsafeDrop off2 bs)
      if tc == Universal && not constructed
        then do
          v <- decodePrimitive tagNum contentSlice
          Right (v, valueEnd)
        else if tc == Universal && constructed
        then do
          v <- decodeConstructed tagNum bs off2 valueEnd
          Right (v, valueEnd)
        else if constructed
        then do
          (inner, _) <- decodeValue bs off2
          Right (Tagged tc tagNum inner, valueEnd)
        else
          Right (Other tc False tagNum contentSlice, valueEnd)
    Nothing
      -- Indefinite length form: only legal for constructed encodings.
      | not constructed ->
          Left "ASN1.Decode: indefinite length on a primitive encoding"
      | otherwise -> do
          (children, off3) <- decodeIndefiniteChildren bs off2
          if tc == Universal && tagNum == 16
            then Right (Sequence (V.fromList children), off3)
            else if tc == Universal && tagNum == 17
              then Right (Set (V.fromList children), off3)
              else if tc == Universal
                then
                  let !content = BSU.unsafeTake (off3 - 2 - off2) (BSU.unsafeDrop off2 bs)
                  in Right (Other Universal True tagNum content, off3)
                else case children of
                  [single] -> Right (Tagged tc tagNum single, off3)
                  _ ->
                    let !content = BSU.unsafeTake (off3 - 2 - off2) (BSU.unsafeDrop off2 bs)
                    in Right (Other tc True tagNum content, off3)

-- | Walk children of an indefinite-length constructed value until we
-- encounter the @0x00 0x00@ end-of-contents marker. Returns the
-- decoded children and the offset /after/ the EOC.
decodeIndefiniteChildren :: ByteString -> Int -> Either String ([Value], Int)
decodeIndefiniteChildren bs off0 = go off0 []
  where
    go !off !acc = do
      ensure bs off 2
      if rdByte bs off == 0x00 && rdByte bs (off + 1) == 0x00
        then Right (reverse acc, off + 2)
        else do
          (v, off') <- decodeValue bs off
          go off' (v : acc)

decodePrimitive :: Int -> ByteString -> Either String Value
decodePrimitive tagNum content = case tagNum of
  1 -> do -- BOOLEAN
    if BS.length content /= 1
      then Left "ASN1.Decode: BOOLEAN must be 1 byte"
      else Right (Boolean (BS.index content 0 /= 0))
  2 -> -- INTEGER (X.690 §8.3.1: at least one content octet)
    if BS.null content
      then Left "ASN1.Decode: INTEGER content must be at least one octet"
      else
        -- DER §10.2 also requires minimal encoding: the first nine
        -- bits must not all be the same. We enforce this in BER too
        -- because non-minimal forms only ever come from broken or
        -- adversarial encoders.
        if BS.length content >= 2
           && (let !b0 = BS.index content 0
                   !b1 = BS.index content 1
               in (b0 == 0x00 && not (testBit b1 7))
                  || (b0 == 0xFF && testBit b1 7))
          then Left "ASN1.Decode: non-minimal INTEGER encoding"
          else Right (Integer (decodeIntegerBytes content))
  3 -> do -- BIT STRING
    if BS.null content
      then Left "ASN1.Decode: empty BIT STRING"
      else
        let !unused = fromIntegral (BS.index content 0)
        in if unused > 7
             then Left "ASN1.Decode: BIT STRING unused-bits byte must be 0..7"
             else if unused > 0 && BS.length content == 1
             then Left "ASN1.Decode: BIT STRING declares unused bits with no content"
             else Right (BitString unused (BS.drop 1 content))
  4 -> Right (OctetString content) -- OCTET STRING
  5 -> Right Null -- NULL
  6 -> do -- OID
    components <- decodeOID content
    Right (OID (V.fromList components))
  12 -> decodeTextTag UTF8String content
  19 -> decodeTextTag PrintableString content
  22 -> decodeTextTag IA5String content
  23 -> decodeTextTag UTCTime content
  24 -> decodeTextTag GeneralizedTime content
  _ -> Right (Other Universal False tagNum content)

decodeTextTag :: (T.Text -> Value) -> ByteString -> Either String Value
decodeTextTag con content = case TE.decodeUtf8' content of
  Left _  -> Left "ASN1.Decode: invalid UTF-8"
  Right t -> Right (con t)

decodeConstructed :: Int -> ByteString -> Int -> Int -> Either String Value
decodeConstructed tagNum bs off end = case tagNum of
  16 -> do -- SEQUENCE
    elems <- decodeSequence bs off end
    Right (Sequence (V.fromList elems))
  17 -> do -- SET
    elems <- decodeSequence bs off end
    Right (Set (V.fromList elems))
  _ -> do
    let !content = BSU.unsafeTake (end - off) (BSU.unsafeDrop off bs)
    Right (Other Universal True tagNum content)

decodeSequence :: ByteString -> Int -> Int -> Either String [Value]
decodeSequence bs off end
  | off >= end = Right []
  | otherwise = do
      (v, off') <- decodeValue bs off
      rest <- decodeSequence bs off' end
      Right (v : rest)

decodeTag :: ByteString -> Int -> Word8 -> Either String (TagClass, Bool, Int, Int)
decodeTag bs off b0 = do
  let !tc = case (b0 `shiftR` 6) .&. 0x03 of
               0 -> Universal
               1 -> Application
               2 -> ContextSpecific
               _ -> Private
      !constructed = testBit b0 5
      !lowBits = fromIntegral (b0 .&. 0x1F) :: Int
  if lowBits < 31
    then Right (tc, constructed, lowBits, off + 1)
    else decodeLongTag bs (off + 1) tc constructed

decodeLongTag :: ByteString -> Int -> TagClass -> Bool -> Either String (TagClass, Bool, Int, Int)
decodeLongTag bs off tc constructed = do
  ensure bs off 1
  -- X.690 §8.1.2.4.2c: in the high-tag-number form the leading octet
  -- of the encoded tag-number must not be @0x80@ — that would
  -- represent a 7-bit zero prefix of an otherwise valid number.
  -- Likewise the encoded number itself must be ≥ 31; values 0..30
  -- belong in the low-tag-number form.
  let !b0 = rdByte bs off
  if b0 == 0x80
    then Left "ASN1.Decode: high-tag-number form must not start with 0x80"
    else go off 0
  where
    go !o !acc = do
      ensure bs o 1
      let !b = rdByte bs o
          !val = acc `shiftL` 7 .|. fromIntegral (b .&. 0x7F)
      if testBit b 7
        then go (o + 1) val
        else if val < 31
          then Left "ASN1.Decode: high-tag-number form used for low number"
          else Right (tc, constructed, val, o + 1)

-- | Decode a BER length octet sequence into 'Just' a definite length or
-- 'Nothing' for the indefinite form. The reserved 0xFF byte
-- (used as a marker in some restricted BER profiles) is rejected.
decodeLengthMaybe :: ByteString -> Int -> Either String (Maybe Int, Int)
decodeLengthMaybe bs off = do
  ensure bs off 1
  let !b0 = rdByte bs off
  if b0 < 128
    then Right (Just (fromIntegral b0), off + 1)
    else if b0 == 0x80
    then Right (Nothing, off + 1)
    else if b0 == 0xFF
    then Left "ASN1.Decode: reserved length octet 0xFF"
    else do
      let !numBytes = fromIntegral (b0 .&. 0x7F) :: Int
      ensure bs (off + 1) numBytes
      let !len = foldl (\acc i -> acc `shiftL` 8 .|. fromIntegral (rdByte bs (off + 1 + i)))
                       (0 :: Int) [0 .. numBytes - 1]
      Right (Just len, off + 1 + numBytes)

decodeIntegerBytes :: ByteString -> Integer
decodeIntegerBytes bs
  | BS.null bs = 0
  | testBit (BS.index bs 0) 7 =
      let !pos = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) (0 :: Integer) bs
          !bitLen = 8 * BS.length bs
      in pos - (1 `shiftL` bitLen)
  | otherwise =
      BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) (0 :: Integer) bs

-- | Decode an OID content body.
--
-- The first sub-identifier on the wire is the base-128 encoding of
-- @40*X1 + X2@. Per X.690 \xC2\xA78.19.4 the first arc value is in {0,1,2};
-- if @combined < 80@ then @X1 = combined / 40@ and @X2 = combined mod 40@,
-- otherwise @X1 = 2@ and @X2 = combined - 80@. Crucially the
-- combined sub-identifier is /not/ restricted to one byte: a value
-- like 2.999 packs as two base-128 bytes (0x88 0x37).
decodeOID :: ByteString -> Either String [Word64]
decodeOID bs
  | BS.null bs = Right []
  | otherwise = do
      (combined, off1) <- decodeBase128 bs 0
      let (!c1, !c2) =
            if combined < 80
              then (combined `div` 40, combined `mod` 40)
              else (2, combined - 80)
      rest <- decodeOIDComponents bs off1 (BS.length bs)
      Right (c1 : c2 : rest)

decodeOIDComponents :: ByteString -> Int -> Int -> Either String [Word64]
decodeOIDComponents bs off end
  | off >= end = Right []
  | otherwise = do
      (val, off') <- decodeBase128 bs off
      rest <- decodeOIDComponents bs off' end
      Right (val : rest)

-- | Decode a base-128 / variable-length integer from @bs@ starting at
-- @off@. Per X.690 §8.19.2 (DER OID encoding) the leading octet of a
-- multi-octet sub-identifier MUST NOT be @0x80@ — that would represent
-- a leading zero which is forbidden by the canonical-encoding rules.
decodeBase128 :: ByteString -> Int -> Either String (Word64, Int)
decodeBase128 bs off
  | off >= BS.length bs =
      Left "ASN1.Decode: unterminated base-128 integer"
  | rdByte bs off == 0x80 && off + 1 < BS.length bs
      && testBit (rdByte bs (off + 1)) 7 =
      -- Loose check: a leading 0x80 followed by another continuation
      -- byte is the prototypical non-canonical form. Stricter spec
      -- compliance: any single 0x80 leading octet that isn't the
      -- only octet is non-minimal.
      Left "ASN1.Decode: non-minimal base-128 encoding (leading 0x80)"
  | rdByte bs off == 0x80 =
      -- Even a lone 0x80 leading + one terminator is non-canonical
      -- (0x80 0xNN where 0xNN < 0x80) — that's 7 bits of zero
      -- prefix. The plain value 0xNN encodes as just 0xNN.
      Left "ASN1.Decode: non-minimal base-128 encoding (leading 0x80)"
  | otherwise = go off 0
  where
    go !o !acc
      | o >= BS.length bs = Left "ASN1.Decode: unterminated base-128 integer"
      | otherwise =
          let !b = rdByte bs o
              !val = acc `shiftL` 7 .|. fromIntegral (b .&. 0x7F)
          in if testBit b 7
             then go (o + 1) val
             else Right (val, o + 1)

rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}

ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off < 0 || n < 0 = Left "ASN1.Decode: negative offset/length"
  | n > BS.length bs - off =
      Left "ASN1.Decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}
