{-# LANGUAGE PatternSynonyms #-}

module EncryptionFooterRegression (tests) where

import Control.Monad (forM_, unless)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Int (Int16)
import Data.Vector qualified as V
import Parquet.Encryption qualified as E
import Parquet.Footer qualified as F
import Parquet.Read qualified as R
import Parquet.Thrift.Schema
import Parquet.Types
import Thrift.Encode (encodeCompact)
import Thrift.Value qualified as TV


tests :: IO ()
tests = do
  forM_ [16, 24, 32] $ \width ->
    forM_ [E.AesGcmV1, E.AesGcmCtrV1] $ \algorithm ->
      forM_ [False, True] $ \omitted ->
        forM_ [False, True] $ \signed -> validFooter width algorithm omitted signed
  malformedMetadata
  ordinary <- right (R.loadParquetFileEncrypted (R.FooterDecryption "unused" "unused" "unused") ordinaryFile)
  check "ordinary unsigned files remain accepted" (R.pfFooter ordinary == metadata)
  check "overlapping header/footer is rejected" (isLeft (E.authenticateFooter key Nothing Nothing (F.writeFooter metadata)))
  putStrLn "OK: encrypted and signed footer authentication (24 configurations plus metadata and tampering regressions)"


metadata :: FileMetadata
metadata = FileMetadata 1 (V.singleton (SchemaElement "schema" Nothing Nothing (Just 0) Nothing Nothing Nothing Nothing)) 0 V.empty Nothing Nothing Nothing


ordinaryFile :: BS.ByteString
ordinaryFile = F.parquetMagic <> F.writeFooter metadata


key, fileId, prefix, keyMetadata, nonce :: BS.ByteString
key = BS.replicate 16 0x42
fileId = "non-eight-byte-file-id"
prefix = "expected-file-version"
keyMetadata = BS.pack [0xff, 0x80, 0, 0x61]
nonce = BS.replicate 12 0x11


description :: E.EncryptionAlgorithm -> Bool -> V.Vector (DataField)
description algorithm omitted =
  V.singleton $
    variant $
      V.fromList $
        [AesGcmV1_AadFileUnique fileId]
          ++ if omitted then [AesGcmV1_SupplyAadPrefix True] else [AesGcmV1_AadPrefix prefix]
  where
    variant = case algorithm of
      E.AesGcmV1 -> EncryptionAlgorithm_AesGcmV1
      E.AesGcmCtrV1 -> EncryptionAlgorithm_AesGcmCtrV1


type DataField = (Int16, TV.Value)


cryptoValue :: V.Vector DataField -> TV.Value
cryptoValue algorithm = TV.Struct (V.fromList [FileCryptoMetaData_EncryptionAlgorithm algorithm, FileCryptoMetaData_KeyMetadata keyMetadata])


aad :: BS.ByteString
aad = E.buildAad prefix (E.buildAadSuffix fileId E.ModuleFooter 0 0 0)


encryptedFile :: BS.ByteString -> TV.Value -> BS.ByteString -> Either String BS.ByteString
encryptedFile aes crypto plaintext = do
  ciphertext <- E.encryptGcmModuleFramedWithNonce aes nonce aad plaintext
  pure (F.parquetEncryptedMagic <> F.writeRawFooter F.parquetEncryptedMagic (encodeCompact crypto <> ciphertext))


signedFile :: BS.ByteString -> TV.Value -> Either String BS.ByteString
signedFile aes footer = do
  let plaintext = encodeCompact footer
  ciphertext <- E.encryptGcmModuleWithNonce aes nonce aad plaintext
  let signature = BS.take 12 ciphertext <> BS.takeEnd 16 ciphertext
  pure (F.parquetMagic <> F.writeRawFooter F.parquetMagic (plaintext <> signature))


validFooter :: Int -> E.EncryptionAlgorithm -> Bool -> Bool -> IO ()
validFooter width algorithm omitted signed = do
  let aes = BS.replicate width 0x42
      fields = case F.fileMetadataToThrift metadata of
        TV.Struct raw -> raw
        _ -> error "expected footer struct"
      -- Reverse the fields so verification must use the original serialized
      -- footer, rather than canonicalizing its order before calculating a tag.
      plaintext = TV.Struct (V.reverse (fields <> V.fromList [FileMetadata_EncryptionAlgorithm (description algorithm omitted), FileMetadata_FooterSigningKeyMetadata keyMetadata]))
      expected = R.FooterDecryption aes fileId prefix
  bytes <-
    right $
      if signed
        then signedFile aes plaintext
        else encryptedFile aes (cryptoValue (description algorithm omitted)) (encodeCompact (F.fileMetadataToThrift metadata))
  info <- right (E.readFooterCryptoMetadata bytes) >>= maybe (fail "missing crypto metadata") pure
  check "opaque key metadata is preserved" (E.fcmKeyMetadata info == Just keyMetadata)
  check "crypto identity is preserved" (E.fcmFileId info == fileId && E.fcmAlgorithm info == algorithm && E.fcmSupplyAadPrefix info == omitted)
  loaded <- right (R.loadParquetFileEncrypted expected bytes)
  check "authenticated footer recovers metadata" (R.pfFooter loaded == metadata)
  _ <- right (E.authenticateFooter aes (Just prefix) Nothing bytes)
  if omitted
    then check "omitted prefix is required" (isLeft (E.authenticateFooter aes Nothing Nothing bytes))
    else do
      _ <- right (E.authenticateFooter aes Nothing Nothing bytes)
      pure ()
  check "expected prefix is checked" (isLeft (E.authenticateFooter aes (Just "other") Nothing bytes))
  check "expected file identifier is checked" (isLeft (R.loadParquetFileEncrypted expected {R.fdFileId = "other"} bytes))
  check "wrong footer key fails authentication" (isLeft (R.loadParquetFileEncrypted expected {R.fdKey = BS.replicate width 0x43} bytes))
  forM_ [BS.length bytes - 9, BS.length bytes - 20, BS.length bytes - 36] $ \offset ->
    check "altered tag/nonce/ciphertext is rejected" (isLeft (R.loadParquetFileEncrypted expected (flipByte offset bytes)))
  check "leading and trailing magic must match" (isLeft (R.loadParquetFileEncrypted expected (BS.replicate 4 0 <> BS.drop 4 bytes)))
  if signed
    then do
      trailer <- right (F.readFooterTrailer bytes)
      let raw = F.ftBytes trailer
          incomplete = F.parquetMagic <> F.writeRawFooter F.parquetMagic (BS.init raw)
          changed = F.parquetMagic <> F.writeRawFooter F.parquetMagic (flipByte 2 raw)
      check "truncated signature is rejected" (isLeft (R.loadParquetFileEncrypted expected incomplete))
      check "altered signed metadata is rejected" (isLeft (R.loadParquetFileEncrypted expected changed))
    else pure ()


malformedMetadata :: IO ()
malformedMetadata = do
  let normal = V.fromList [AesGcmV1_AadFileUnique fileId, AesGcmV1_AadPrefix prefix]
      variant = EncryptionAlgorithm_AesGcmV1
      invalid =
        [ V.empty
        , V.fromList [variant normal, EncryptionAlgorithm_AesGcmCtrV1 normal]
        , V.singleton (99, TV.Struct normal)
        , V.singleton (variant (V.singleton (AesGcmV1_AadPrefix prefix)))
        , V.singleton (variant (V.snoc normal (AesGcmV1_AadFileUnique fileId)))
        , V.singleton (variant (V.snoc normal (AesGcmV1_SupplyAadPrefix True)))
        , V.singleton (variant (V.snoc normal (fst (AesGcmV1_SupplyAadPrefix False), TV.I32 1)))
        , V.singleton (variant (V.fromList [AesGcmV1_AadFileUnique fileId, (fst (AesGcmV1_AadPrefix prefix), TV.I32 0)]))
        ]
      plaintext = encodeCompact (F.fileMetadataToThrift metadata)
      expected = R.FooterDecryption key fileId prefix
  forM_ invalid $ \algorithm -> do
    bytes <- right (encryptedFile key (cryptoValue algorithm) plaintext)
    check "invalid crypto metadata is rejected even with a valid GCM footer" (isLeft (R.loadParquetFileEncrypted expected bytes))
  let duplicate = TV.Struct (V.fromList [FileCryptoMetaData_EncryptionAlgorithm (description E.AesGcmV1 False), FileCryptoMetaData_KeyMetadata keyMetadata, FileCryptoMetaData_KeyMetadata keyMetadata])
  duplicateFile <- right (encryptedFile key duplicate plaintext)
  check "duplicate key routing fields are rejected" (isLeft (R.loadParquetFileEncrypted expected duplicateFile))
  trailing <- right (encryptedFile key (cryptoValue (description E.AesGcmV1 False)) (plaintext <> "junk"))
  check "authenticated trailing footer bytes are rejected" (isLeft (R.loadParquetFileEncrypted expected trailing))
  padded <- right (encryptedFile key (cryptoValue (description E.AesGcmV1 False)) (plaintext <> BS.replicate 1600 0))
  paddedFile <- right (R.loadParquetFileEncrypted expected padded)
  check "authenticated parquet-mr zero padding is accepted" (R.pfFooter paddedFile == metadata)
  check "padding is authenticated before it is ignored" (isLeft (R.loadParquetFileEncrypted expected (flipByte (BS.length padded - 25) padded)))


flipByte :: Int -> BS.ByteString -> BS.ByteString
flipByte index bytes = BS.take index bytes <> BS.singleton (BS.index bytes index `xor` 1) <> BS.drop (index + 1) bytes


right :: Either String a -> IO a
right = either fail pure


check :: String -> Bool -> IO ()
check label valid = unless valid (fail ("EncryptionFooterRegression: " ++ label))
