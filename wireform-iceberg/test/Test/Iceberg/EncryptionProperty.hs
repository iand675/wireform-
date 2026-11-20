{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Hedgehog property tests for "Parquet.Encryption" — the AES-GCM /
AES-CTR module envelope that the Parquet writer uses for every
encrypted module.
-}
module Test.Iceberg.EncryptionProperty (tests) where

import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Encryption qualified as Enc
import Test.Syd
import Test.Syd.Hedgehog ()


tests :: Spec
tests =
  describe "Parquet.Encryption properties" $
    sequence_
      [ it
          "GCM encrypt/decrypt round-trip with random nonce"
          propGcmRoundTrip
      , it
          "GCM rejects flipped tag bits"
          propGcmTamperTag
      , it
          "GCM rejects flipped ciphertext bits"
          propGcmTamperCiphertext
      , it
          "GCM detects AAD mismatch"
          propGcmAadMismatch
      , it
          "CTR encrypt/decrypt round-trip"
          propCtrRoundTrip
      , it
          "framed module: writeFramed -> readFramed -> decrypt"
          propFramedRoundTrip
      , it
          "data-page AAD suffix preserves the file ID and appends seven bytes"
          propAadSuffixLength
      ]


-- ============================================================
-- Generators
-- ============================================================

genKey :: Gen ByteString
genKey =
  Gen.choice
    [ Gen.bytes (Range.singleton 16) -- AES-128
    , Gen.bytes (Range.singleton 24) -- AES-192
    , Gen.bytes (Range.singleton 32) -- AES-256
    ]


genAad :: Gen ByteString
genAad = Gen.bytes (Range.linear 0 64)


genPlaintext :: Gen ByteString
genPlaintext = Gen.bytes (Range.linear 0 200)


-- ============================================================
-- Properties
-- ============================================================

propGcmRoundTrip :: Property
propGcmRoundTrip = property $ do
  key <- forAll genKey
  aad <- forAll genAad
  plain <- forAll genPlaintext
  encrypted <- evalIO (Enc.encryptModule key aad plain)
  case encrypted of
    Left e -> footnote ("encrypt failed: " ++ e) >> failure
    Right ct -> case Enc.decryptGcmModule key aad ct of
      Left e -> footnote ("decrypt failed: " ++ e) >> failure
      Right p -> p === plain


propGcmTamperTag :: Property
propGcmTamperTag = property $ do
  key <- forAll genKey
  aad <- forAll genAad
  plain <- forAll genPlaintext
  encrypted <- evalIO (Enc.encryptModule key aad plain)
  case encrypted of
    Left e -> footnote ("encrypt failed: " ++ e) >> failure
    Right ct -> do
      let !n = BS.length ct
      idx <- forAll (Gen.int (Range.linear (n - 16) (n - 1)))
      -- last 16 bytes are the GCM tag; flipping any of them
      -- must invalidate authentication.
      let !tampered = flipBit ct idx
      case Enc.decryptGcmModule key aad tampered of
        Left _ -> success
        Right _ -> footnote "tampered tag passed auth" >> failure


propGcmTamperCiphertext :: Property
propGcmTamperCiphertext = property $ do
  key <- forAll genKey
  aad <- forAll genAad
  plain <- forAll (Gen.bytes (Range.linear 1 200)) -- non-empty so there's CT to flip
  encrypted <- evalIO (Enc.encryptModule key aad plain)
  case encrypted of
    Left e -> footnote ("encrypt failed: " ++ e) >> failure
    Right ct -> do
      let !n = BS.length ct
      -- flip a byte inside the ciphertext, between nonce(12) and tag(16).
      idx <- forAll (Gen.int (Range.linear 12 (n - 17)))
      let !tampered = flipBit ct idx
      case Enc.decryptGcmModule key aad tampered of
        Left _ -> success
        Right _ -> footnote "tampered ct passed auth" >> failure


propGcmAadMismatch :: Property
propGcmAadMismatch = property $ do
  key <- forAll genKey
  aad1 <- forAll genAad
  aad2 <- forAll (Gen.filter (/= aad1) genAad)
  plain <- forAll genPlaintext
  encrypted <- evalIO (Enc.encryptModule key aad1 plain)
  case encrypted of
    Left e -> footnote ("encrypt failed: " ++ e) >> failure
    Right ct -> case Enc.decryptGcmModule key aad2 ct of
      Left _ -> success
      Right _ -> footnote "decrypt accepted wrong AAD" >> failure


propCtrRoundTrip :: Property
propCtrRoundTrip = property $ do
  key <- forAll genKey
  plain <- forAll genPlaintext
  encrypted <- evalIO (Enc.encryptModuleCtr key plain)
  case encrypted of
    Left e -> footnote ("encrypt failed: " ++ e) >> failure
    Right ct -> case Enc.decryptCtrModule key ct of
      Left e -> footnote ("decrypt failed: " ++ e) >> failure
      Right p -> p === plain


propFramedRoundTrip :: Property
propFramedRoundTrip = property $ do
  key <- forAll genKey
  aad <- forAll genAad
  plain <- forAll genPlaintext
  encrypted <- evalIO (Enc.encryptGcmModuleFramed key aad plain)
  case encrypted of
    Left e -> footnote ("framed encrypt failed: " ++ e) >> failure
    Right framed ->
      case Enc.readFramedModule framed 0 of
        Left e -> footnote ("readFramed failed: " ++ e) >> failure
        Right (raw, end) -> do
          end === BS.length framed
          case Enc.decryptGcmModule key aad raw of
            Left e -> footnote ("decrypt failed: " ++ e) >> failure
            Right p -> p === plain


propAadSuffixLength :: Property
propAadSuffixLength = property $ do
  fileId <- forAll (Gen.bytes (Range.linear 0 16))
  rg <- forAll (Gen.int16 Range.linearBounded)
  col <- forAll (Gen.int16 Range.linearBounded)
  pg <- forAll (Gen.int16 Range.linearBounded)
  let !suffix = Enc.buildAadSuffix fileId Enc.ModuleDataPage rg col pg
  -- The file identifier is implementation-defined; data pages append
  -- one module byte and three little-endian 16-bit ordinals.
  BS.length suffix === BS.length fileId + 7


-- ============================================================
-- Helpers
-- ============================================================

flipBit :: ByteString -> Int -> ByteString
flipBit bs i
  | i < 0 || i >= BS.length bs = bs
  | otherwise =
      let !old = BS.index bs i :: Word8
          !new = old `xor` 0x01
      in BS.take i bs <> BS.singleton new <> BS.drop (i + 1) bs
