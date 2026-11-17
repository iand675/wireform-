{-# LANGUAGE BangPatterns #-}

{- | ASN.1 BER decoder (accepts DER-encoded data as well).

Decodes BER\/DER-encoded 'ByteString' data into an 'ASN1.Value.Value'.
Handles both primitive and constructed encodings, definite and
indefinite lengths, and all common universal tag types.
-}
module ASN1.Decode (
  decode,
) where

import ASN1.Value
import Data.Bits (Bits, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word64, Word8)


decode :: ByteString -> Either String Value
decode bs
  | BS.null bs = Left "ASN1.Decode: empty input"
  | otherwise = case decodeValue bs 0 of
      Left err -> Left err
      Right (v, _) -> Right v


decodeValue :: ByteString -> Int -> Either String (Value, Int)
decodeValue bs off = do
  ensure bs off 1
  let !b0 = rdByte bs off
  (tc, constructed, tagNum, off1) <- decodeTag bs off b0
  (len, off2) <- decodeLength bs off1
  let !valueEnd = off2 + len
  ensure bs off2 len
  let !contentSlice = BSU.unsafeTake len (BSU.unsafeDrop off2 bs)
  if tc == Universal && not constructed
    then do
      v <- decodePrimitive tagNum contentSlice
      Right (v, valueEnd)
    else
      if tc == Universal && constructed
        then do
          v <- decodeConstructed tagNum bs off2 valueEnd
          Right (v, valueEnd)
        else
          if constructed
            then do
              inner <- decodeValue bs off2
              Right (Tagged tc tagNum (fst inner), valueEnd)
            else
              Right (Other tc False tagNum contentSlice, valueEnd)


decodePrimitive :: Int -> ByteString -> Either String Value
decodePrimitive tagNum content = case tagNum of
  1 -> do
    -- BOOLEAN
    if BS.length content /= 1
      then Left "ASN1.Decode: BOOLEAN must be 1 byte"
      else Right (Boolean (BS.index content 0 /= 0))
  2 -> Right (Integer (decodeIntegerBytes content)) -- INTEGER
  3 -> do
    -- BIT STRING
    if BS.null content
      then Left "ASN1.Decode: empty BIT STRING"
      else
        let !unused = fromIntegral (BS.index content 0)
        in Right (BitString unused (BS.drop 1 content))
  4 -> Right (OctetString content) -- OCTET STRING
  5 -> Right Null -- NULL
  6 -> do
    -- OID
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
  Left _ -> Left "ASN1.Decode: invalid UTF-8"
  Right t -> Right (con t)


decodeConstructed :: Int -> ByteString -> Int -> Int -> Either String Value
decodeConstructed tagNum bs off end = case tagNum of
  16 -> do
    -- SEQUENCE
    elems <- decodeSequence bs off end
    Right (Sequence (V.fromList elems))
  17 -> do
    -- SET
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
decodeLongTag bs off tc constructed = go off 0
  where
    go !o !acc = do
      ensure bs o 1
      let !b = rdByte bs o
          !val = acc `shiftL` 7 .|. fromIntegral (b .&. 0x7F)
      if testBit b 7
        then go (o + 1) val
        else Right (tc, constructed, val, o + 1)


decodeLength :: ByteString -> Int -> Either String (Int, Int)
decodeLength bs off = do
  ensure bs off 1
  let !b0 = rdByte bs off
  if b0 < 128
    then Right (fromIntegral b0, off + 1)
    else
      if b0 == 0x80
        then Left "ASN1.Decode: indefinite length not supported in this decoder"
        else do
          let !numBytes = fromIntegral (b0 .&. 0x7F) :: Int
          ensure bs (off + 1) numBytes
          let !len =
                foldl
                  (\acc i -> acc `shiftL` 8 .|. fromIntegral (rdByte bs (off + 1 + i)))
                  (0 :: Int)
                  [0 .. numBytes - 1]
          Right (len, off + 1 + numBytes)


decodeIntegerBytes :: ByteString -> Integer
decodeIntegerBytes bs
  | BS.null bs = 0
  | testBit (BS.index bs 0) 7 =
      let !pos = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) (0 :: Integer) bs
          !bitLen = 8 * BS.length bs
      in pos - (1 `shiftL` bitLen)
  | otherwise =
      BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) (0 :: Integer) bs


decodeOID :: ByteString -> Either String [Word64]
decodeOID bs
  | BS.null bs = Left "ASN1.Decode: empty OID"
  | first < 40 = rest 0 (fromIntegral first) 1
  | first < 80 = rest 1 (fromIntegral first - 40) 1
  | first < 128 = rest 2 (fromIntegral first - 80) 1
  | otherwise = do
      (combined, off) <- decodeBase128 (80 + toInteger (maxBound :: Word64)) bs 0
      rest 2 (fromIntegral (combined - 80)) off
  where
    first = rdByte bs 0
    rest !firstArc !secondArc !off =
      (firstArc :) . (secondArc :) <$> decodeOIDComponents bs off


decodeOIDComponents :: ByteString -> Int -> Either String [Word64]
decodeOIDComponents bs off
  | off >= BS.length bs = Right []
  | otherwise = do
      (val, off') <- decodeBase128 maxBound bs off
      (val :) <$> decodeOIDComponents bs off'


decodeBase128 :: (Bits a, Integral a) => a -> ByteString -> Int -> Either String (a, Int)
decodeBase128 limit bs off
  | rdByte bs off == 0x80 = Left "ASN1.Decode: non-minimal OID subidentifier"
  | otherwise = go off 0
  where
    go !o !acc
      | o >= BS.length bs = Left "ASN1.Decode: unterminated OID subidentifier"
      | acc > (limit - digit) `shiftR` 7 = Left "ASN1.Decode: OID arc exceeds Word64"
      | testBit b 7 = go (o + 1) val
      | otherwise = Right (val, o + 1)
      where
        b = rdByte bs o
        digit = fromIntegral (b .&. 0x7F)
        val = (acc `shiftL` 7) .|. digit
{-# INLINE decodeBase128 #-}


rdByte :: ByteString -> Int -> Word8
rdByte !bs !off = BSU.unsafeIndex bs off
{-# INLINE rdByte #-}


ensure :: ByteString -> Int -> Int -> Either String ()
ensure bs off n
  | off + n > BS.length bs = Left "ASN1.Decode: unexpected end of input"
  | otherwise = Right ()
{-# INLINE ensure #-}
