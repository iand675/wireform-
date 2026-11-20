{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Apache Parquet modular encryption (parquet-format/Encryption.md).

Implements the spec's two encryption algorithms:

- 'AesGcmV1' (full AES-GCM): every encrypted module - footer, column
  metadata, column index, offset index, bloom filter, and each page -
  is encrypted as @nonce(12) || ciphertext || tag(16)@. The data and the
  AAD are both authenticated, so any tampering with the layout aborts
  the read.

- 'AesGcmCtrV1' (mixed mode): metadata modules (footer, column metadata,
  page headers, indexes, bloom filter) use AES-GCM as above; data
  pages and dictionary pages use AES-CTR for speed (no authentication on
  the data bytes themselves, only on the encrypted page header).

The 'AAD' (Additional Authenticated Data) for each module is the
concatenation of:

@
AAD = aad_prefix || aad_suffix
aad_suffix = aad_file_id(variable) || module_type(1)
             || row_group_ordinal(2) || column_ordinal(2)
             || page_ordinal(2)
@

not all modules use every suffix component; this module exposes
'buildAadSuffix' / 'buildAad' helpers that match the spec's wire layout.

The actual key material is supplied by the caller through
'EncryptionKeys'; this module does not assume a particular KMS. The
opaque @key_metadata@ bytes that should be stored on the encrypted
column chunks (and on the Iceberg @data_file.key_metadata@ field) round-
trip unchanged through 'EncryptionConfig'.
-}
module Parquet.Encryption (
  -- * Algorithms
  EncryptionAlgorithm (..),
  ModuleType (..),
  moduleTypeId,

  -- * AAD construction
  buildAadSuffix,
  buildAad,

  -- * Encrypt / decrypt
  encryptModule,
  decryptModule,
  encryptModuleCtr,
  decryptModuleCtr,

  -- * Explicit nonces (must be freshly generated for every module)
  encryptGcmModuleWithNonce,
  encryptCtrModuleWithNonce,
  decryptGcmModule,
  decryptCtrModule,

  -- * Wire framing (4-byte length prefix per the spec, section 5.1)
  encryptGcmModuleFramed,
  encryptCtrModuleFramed,
  encryptGcmModuleFramedWithNonce,
  encryptCtrModuleFramedWithNonce,
  readFramedModule,

  -- * File footer authentication
  FooterCryptoMetadata (..),
  AuthenticatedFooter (..),
  readFooterCryptoMetadata,
  authenticateFooter,

  -- * Key configuration
  EncryptionKeys (..),
  EncryptionConfig (..),
  unencrypted,
) where

import Crypto.Cipher.AES qualified as Crypto
import Crypto.Cipher.Types (AEADMode (..), BlockCipher, IV)
import Crypto.Cipher.Types qualified as Cipher
import Crypto.Error (CryptoFailable (..))
import Crypto.Random.Types qualified as RNG
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word32)
import Parquet.Footer qualified as Footer
import Parquet.Thrift.Schema
import Thrift.Decode (decodeCompactFrom)
import Thrift.Value qualified as TV
import Wireform.Builder qualified as B


{- | Unauthenticated routing information read from the file. The key metadata
is opaque: callers may resolve it with their own KMS before authenticating.
Neither this structure nor its AAD fields establish the file's identity
until 'authenticateFooter' has succeeded.
-}
data FooterCryptoMetadata = FooterCryptoMetadata
  { fcmAlgorithm :: !EncryptionAlgorithm
  , fcmFileId :: !ByteString
  , fcmStoredAadPrefix :: !(Maybe ByteString)
  , fcmSupplyAadPrefix :: !Bool
  , fcmKeyMetadata :: !(Maybe ByteString)
  }
  deriving stock (Show, Eq)


{- | The authenticated Thrift footer, retaining its crypto fields for column
decryption. Ordinary, unsigned files have no crypto metadata.
-}
data AuthenticatedFooter = AuthenticatedFooter
  { afValue :: !TV.Value
  , afCryptoMetadata :: !(Maybe FooterCryptoMetadata)
  }
  deriving stock (Show, Eq)


{- | Inspect the untrusted key metadata and AAD description. This does not
decrypt the footer or verify its signature. A returned file identifier is
supplied to the cipher, never used as evidence that the file is authentic.
-}
readFooterCryptoMetadata :: ByteString -> Either String (Maybe FooterCryptoMetadata)
readFooterCryptoMetadata bytes = do
  trailer <- checkedCryptoTrailer bytes
  if Footer.ftMagic trailer == Footer.parquetEncryptedMagic
    then do
      (value, _) <- decodeCompactFrom (Footer.ftBytes trailer) 0
      Just <$> encryptedFooterMetadata value
    else do
      (value, _) <- decodeCompactFrom (Footer.ftBytes trailer) 0
      plaintextFooterMetadata value


{- | Authenticate an encrypted footer or a signed plaintext footer. The
optional expected prefix is compared with a stored prefix, or supplied
when the writer omitted it. The optional expected file identifier supports
callers migrating from the original explicit-AAD reader; normally the
identifier comes from the file's crypto metadata.

Plaintext footer signatures authenticate the original serialized bytes,
not a re-encoding of the parsed Thrift value. No column pages are decrypted
by this function. Unsigned ordinary files remain accepted.
-}
authenticateFooter
  :: ByteString
  -> Maybe ByteString
  -> Maybe ByteString
  -> ByteString
  -> Either String AuthenticatedFooter
authenticateFooter key expectedPrefix expectedFileId bytes = do
  trailer <- checkedCryptoTrailer bytes
  let footerBytes = Footer.ftBytes trailer
  if Footer.ftMagic trailer == Footer.parquetEncryptedMagic
    then do
      (cryptoValue, encryptedStart) <- decodeCompactFrom footerBytes 0
      metadata <- encryptedFooterMetadata cryptoValue
      aad <- footerAad metadata expectedPrefix expectedFileId
      (ciphertext, afterModule) <- readFramedModule footerBytes encryptedStart
      requireCrypto (afterModule == BS.length footerBytes) "trailing bytes after encrypted footer"
      plaintext <- decryptGcmModule key aad ciphertext
      (value, end) <- decodeCompactFrom plaintext 0
      -- parquet-mr encrypts a capacity-sized serialization buffer in several
      -- official fixtures. Its unused tail consists of authenticated zeros.
      -- Check the complete GCM module first, then permit only that padding.
      requireCrypto (BS.all (== 0) (BS.drop end plaintext)) "nonzero trailing bytes in decrypted footer"
      pure (AuthenticatedFooter value (Just metadata))
    else do
      (value, end) <- decodeCompactFrom footerBytes 0
      metadata <- plaintextFooterMetadata value
      case metadata of
        Nothing -> do
          requireCrypto (end == BS.length footerBytes) "trailing bytes after unsigned footer"
          pure (AuthenticatedFooter value Nothing)
        Just crypto -> do
          aad <- footerAad crypto expectedPrefix expectedFileId
          let signature = BS.drop end footerBytes
          requireCrypto (BS.length signature == 28) "plaintext footer signature must contain 28 bytes"
          let nonce = BS.take 12 signature
          encrypted <- encryptGcmModuleWithNonce key nonce aad (BS.take end footerBytes)
          requireCrypto (BA.constEq (BS.drop 12 signature) (BS.takeEnd 16 encrypted)) "plaintext footer signature mismatch"
          pure (AuthenticatedFooter value metadata)


checkedCryptoTrailer :: ByteString -> Either String Footer.FooterTrailer
checkedCryptoTrailer bytes = do
  trailer <- Footer.readFooterTrailer bytes
  requireCrypto (BS.take 4 bytes == Footer.ftMagic trailer) "leading and trailing magic differ"
  requireCrypto (BS.length (Footer.ftBytes trailer) <= BS.length bytes - 12) "footer overlaps leading magic"
  pure trailer


encryptedFooterMetadata :: TV.Value -> Either String FooterCryptoMetadata
encryptedFooterMetadata value = do
  fields <- cryptoStruct value
  algorithm <- requiredCryptoField (FileCryptoMetaData_EncryptionAlgorithm V.empty) fields
  keyMetadata <- optionalCryptoField (FileCryptoMetaData_KeyMetadata BS.empty) fields >>= traverse cryptoBytes
  parseFooterAlgorithm keyMetadata algorithm


plaintextFooterMetadata :: TV.Value -> Either String (Maybe FooterCryptoMetadata)
plaintextFooterMetadata value = do
  fields <- cryptoStruct value
  algorithm <- optionalCryptoField (FileMetadata_EncryptionAlgorithm V.empty) fields
  keyMetadata <- optionalCryptoField (FileMetadata_FooterSigningKeyMetadata BS.empty) fields >>= traverse cryptoBytes
  case algorithm of
    Nothing -> do
      requireCrypto (keyMetadata == Nothing) "footer signing key metadata lacks an encryption algorithm"
      pure Nothing
    Just description -> Just <$> parseFooterAlgorithm keyMetadata description


parseFooterAlgorithm :: Maybe ByteString -> TV.Value -> Either String FooterCryptoMetadata
parseFooterAlgorithm keyMetadata value = do
  union <- cryptoStruct value
  requireCrypto (V.length union == 1) "encryption algorithm must have exactly one variant"
  (algorithm, description) <- case V.head union of
    EncryptionAlgorithm_AesGcmV1 fields -> Right (AesGcmV1, TV.Struct fields)
    EncryptionAlgorithm_AesGcmCtrV1 fields -> Right (AesGcmCtrV1, TV.Struct fields)
    _ -> Left "Parquet.Encryption: unsupported encryption algorithm"
  fields <- cryptoStruct description
  prefix <- optionalCryptoField (AesGcmV1_AadPrefix BS.empty) fields >>= traverse cryptoBytes
  fileId <- requiredCryptoField (AesGcmV1_AadFileUnique BS.empty) fields >>= cryptoBytes
  requireCrypto (not (BS.null fileId)) "missing unique file identifier"
  supplied <-
    optionalCryptoField (AesGcmV1_SupplyAadPrefix False) fields >>= \case
      Nothing -> Right False
      Just (TV.Bool flag) -> Right flag
      _ -> Left "Parquet.Encryption: supply_aad_prefix must be boolean"
  requireCrypto (not supplied || prefix == Nothing) "AAD prefix is both stored and required from caller"
  pure (FooterCryptoMetadata algorithm fileId prefix supplied keyMetadata)


footerAad :: FooterCryptoMetadata -> Maybe ByteString -> Maybe ByteString -> Either String ByteString
footerAad metadata expectedPrefix expectedFileId = do
  case expectedFileId of
    Nothing -> pure ()
    Just expected -> requireCrypto (expected == fcmFileId metadata) "file identifier differs from expected value"
  prefix <- case fcmStoredAadPrefix metadata of
    Just stored -> do
      case expectedPrefix of
        Nothing -> pure ()
        Just expected -> requireCrypto (expected == stored) "AAD prefix differs from expected value"
      pure stored
    Nothing -> do
      requireCrypto (not (fcmSupplyAadPrefix metadata) || expectedPrefix /= Nothing) "file requires an AAD prefix from caller"
      let supplied = fromMaybe BS.empty expectedPrefix
      requireCrypto (fcmSupplyAadPrefix metadata || BS.null supplied) "file has no AAD prefix"
      pure supplied
  pure (buildAad prefix (buildAadSuffix (fcmFileId metadata) ModuleFooter 0 0 0))


cryptoStruct :: TV.Value -> Either String (V.Vector (Int16, TV.Value))
cryptoStruct (TV.Struct fields) = do
  let unique = V.foldl' (\known (tag, _) -> Map.insert tag () known) Map.empty fields
  requireCrypto (Map.size unique == V.length fields) "duplicate crypto metadata field"
  pure fields
cryptoStruct _ = Left "Parquet.Encryption: expected crypto metadata struct"


optionalCryptoField :: (Int16, TV.Value) -> V.Vector (Int16, TV.Value) -> Either String (Maybe TV.Value)
optionalCryptoField (wanted, _) = pure . fmap snd . V.find ((== wanted) . fst)


requiredCryptoField :: (Int16, TV.Value) -> V.Vector (Int16, TV.Value) -> Either String TV.Value
requiredCryptoField tag fields = optionalCryptoField tag fields >>= maybe (Left "Parquet.Encryption: missing crypto metadata field") Right


cryptoBytes :: TV.Value -> Either String ByteString
cryptoBytes (TV.Binary bytes) = Right bytes
cryptoBytes (TV.String text) = Right (TE.encodeUtf8 text)
cryptoBytes _ = Left "Parquet.Encryption: expected binary crypto metadata"


requireCrypto :: Bool -> String -> Either String ()
requireCrypto True _ = Right ()
requireCrypto False message = Left ("Parquet.Encryption: " ++ message)


-- ============================================================
-- Spec types
-- ============================================================

data EncryptionAlgorithm = AesGcmV1 | AesGcmCtrV1
  deriving (Show, Eq)


data ModuleType
  = ModuleFooter
  | ModuleColumnMetaData
  | ModuleDataPage
  | ModuleDictionaryPage
  | ModuleDataPageHeader
  | ModuleDictionaryPageHeader
  | ModuleColumnIndex
  | ModuleOffsetIndex
  | ModuleBloomFilterHeader
  | ModuleBloomFilterBitset
  deriving (Show, Eq)


-- | The byte constant the spec assigns to each module type.
moduleTypeId :: ModuleType -> Int
moduleTypeId = \case
  ModuleFooter -> 0
  ModuleColumnMetaData -> 1
  ModuleDataPage -> 2
  ModuleDictionaryPage -> 3
  ModuleDataPageHeader -> 4
  ModuleDictionaryPageHeader -> 5
  ModuleColumnIndex -> 6
  ModuleOffsetIndex -> 7
  ModuleBloomFilterHeader -> 8
  ModuleBloomFilterBitset -> 9


-- ============================================================
-- AAD framing
-- ============================================================

buildAadSuffix
  :: ByteString
  -- ^ aad_file_unique, preserved verbatim (the format leaves its length to the writer).
  -> ModuleType
  -> Int16
  -- ^ row group ordinal.
  -> Int16
  -- ^ column ordinal.
  -> Int16
  -- ^ page ordinal.
  -> ByteString
buildAadSuffix fileId mt rg col pg
  | BS.length fileId < 0 || BS.length fileId > maxBound - extra = error "Parquet.Encryption: AAD suffix exceeds addressable size"
  | otherwise =
      B.toStrictByteStringExact (BS.length fileId + extra) $
        B.byteString fileId
          <> B.word8 (fromIntegral (moduleTypeId mt))
          <> (if mt == ModuleFooter then mempty else B.int16LE rg <> B.int16LE col)
          <> ( case mt of
                 ModuleDataPage -> B.int16LE pg
                 ModuleDataPageHeader -> B.int16LE pg
                 _ -> mempty
             )
  where
    extra = case mt of
      ModuleFooter -> 1
      ModuleDataPage -> 7
      ModuleDataPageHeader -> 7
      _ -> 5


buildAad
  :: ByteString
  -- ^ aad_prefix (caller-defined; can be empty).
  -> ByteString
  -- ^ aad_suffix produced by 'buildAadSuffix'.
  -> ByteString
buildAad = (<>)


-- ============================================================
-- AES-GCM
-- ============================================================

-- | Encrypt a module with AES-GCM. Output: @nonce(12) || ciphertext || tag(16)@.
encryptModule
  :: RNG.MonadRandom m
  => ByteString
  -- ^ AES key (16, 24, or 32 bytes).
  -> ByteString
  -- ^ AAD.
  -> ByteString
  -- ^ Plaintext.
  -> m (Either String ByteString)
encryptModule key aad plaintext = do
  nonce <- RNG.getRandomBytes 12 :: RNG.MonadRandom m => m BS.ByteString
  pure (encryptGcmWithNonce key nonce aad plaintext)


decryptModule
  :: ByteString
  -- ^ key
  -> ByteString
  -- ^ aad
  -> ByteString
  -- ^ wire bytes (nonce || ciphertext || tag)
  -> Either String ByteString
decryptModule key aad bs = case BS.length bs of
  n
    | n < 28 -> Left "Parquet.Encryption: ciphertext too short"
    | otherwise ->
        let (nonce, rest) = BS.splitAt 12 bs
            (ct, tagBs) = BS.splitAt (BS.length rest - 16) rest
        in withAes
             key
             (\(c :: Crypto.AES128) -> gcmDecrypt c nonce aad ct tagBs)
             (\(c :: Crypto.AES192) -> gcmDecrypt c nonce aad ct tagBs)
             (\(c :: Crypto.AES256) -> gcmDecrypt c nonce aad ct tagBs)


encryptGcmWithNonce :: ByteString -> ByteString -> ByteString -> ByteString -> Either String ByteString
encryptGcmWithNonce key nonce aad plaintext =
  withAes
    key
    (\(c :: Crypto.AES128) -> gcmEncrypt c nonce aad plaintext)
    (\(c :: Crypto.AES192) -> gcmEncrypt c nonce aad plaintext)
    (\(c :: Crypto.AES256) -> gcmEncrypt c nonce aad plaintext)


gcmEncrypt :: forall c. BlockCipher c => c -> ByteString -> ByteString -> ByteString -> Either String ByteString
gcmEncrypt cipher nonce aad plaintext = do
  -- AES-GCM uses a 12-byte nonce; aeadInit accepts any ByteArrayAccess
  -- so we pass the nonce bytes directly. The IV-length restriction in
  -- @makeIV@ doesn't apply to AEAD initialisation.
  aead0 <- adapt (Cipher.aeadInit AEAD_GCM cipher nonce)
  let aead1 = Cipher.aeadAppendHeader aead0 aad
      (ciphertext, aead2) = Cipher.aeadEncrypt aead1 plaintext
      tag = Cipher.aeadFinalize aead2 16
  Right $ BS.concat [nonce, ciphertext, BA.convert tag]


gcmDecrypt :: forall c. BlockCipher c => c -> ByteString -> ByteString -> ByteString -> ByteString -> Either String ByteString
gcmDecrypt cipher nonce aad ct tagBs = do
  aead0 <- adapt (Cipher.aeadInit AEAD_GCM cipher nonce)
  let aead1 = Cipher.aeadAppendHeader aead0 aad
      (plaintext, aead2) = Cipher.aeadDecrypt aead1 ct
      expected = Cipher.aeadFinalize aead2 16
  if BA.constEq expected tagBs
    then Right plaintext
    else Left "Parquet.Encryption: GCM tag mismatch"


-- ============================================================
-- AES-CTR (mixed-mode payload encryption)
-- ============================================================

{- | Encrypt a module with AES-CTR (no authentication). Wire format:
@nonce(12) || ciphertext@. Used for data\/dictionary pages in
'AesGcmCtrV1'.
-}
encryptModuleCtr
  :: RNG.MonadRandom m
  => ByteString
  -- ^ key
  -> ByteString
  -- ^ plaintext
  -> m (Either String ByteString)
encryptModuleCtr key plaintext = do
  nonce <- RNG.getRandomBytes 12 :: RNG.MonadRandom m => m BS.ByteString
  pure (ctrEncryptWithNonce key nonce plaintext)


decryptModuleCtr :: ByteString -> ByteString -> Either String ByteString
decryptModuleCtr key bs
  | BS.length bs < 12 = Left "Parquet.Encryption: CTR ciphertext too short"
  | otherwise =
      let (nonce, ct) = BS.splitAt 12 bs
      in withAes
           key
           (\(c :: Crypto.AES128) -> ctrCombine c nonce ct)
           (\(c :: Crypto.AES192) -> ctrCombine c nonce ct)
           (\(c :: Crypto.AES256) -> ctrCombine c nonce ct)


ctrEncryptWithNonce :: ByteString -> ByteString -> ByteString -> Either String ByteString
ctrEncryptWithNonce key nonce plaintext =
  fmap
    (BS.append nonce)
    ( withAes
        key
        (\(c :: Crypto.AES128) -> ctrCombine c nonce plaintext)
        (\(c :: Crypto.AES192) -> ctrCombine c nonce plaintext)
        (\(c :: Crypto.AES256) -> ctrCombine c nonce plaintext)
    )


ctrCombine :: forall c. BlockCipher c => c -> ByteString -> ByteString -> Either String ByteString
ctrCombine cipher nonce buf = do
  -- Per Parquet spec: the 16-byte AES-CTR IV is the 12-byte module nonce
  -- followed by the big-endian initial counter 1 (Encryption.md §4.2.2).
  iv <- mkIVFor cipher (BS.append nonce (BS.pack [0, 0, 0, 1]))
  Right (Cipher.ctrCombine cipher iv buf)


-- ============================================================
-- Key configuration
-- ============================================================

data EncryptionKeys = EncryptionKeys
  { ekFooterKey :: !ByteString
  , ekColumnKeys :: !(Map Text ByteString)
  }
  deriving (Show, Eq)


data EncryptionConfig = EncryptionConfig
  { encAlgorithm :: !EncryptionAlgorithm
  , encKeys :: !EncryptionKeys
  , encAadFileId :: !ByteString
  , encAadPrefix :: !ByteString
  , encKeyMetadata :: !ByteString
  }
  deriving (Show, Eq)


unencrypted :: EncryptionConfig
unencrypted =
  EncryptionConfig
    { encAlgorithm = AesGcmV1
    , encKeys = EncryptionKeys BS.empty Map.empty
    , encAadFileId = BS.empty
    , encAadPrefix = BS.empty
    , encKeyMetadata = BS.empty
    }


-- ============================================================
-- Explicit nonce primitives
-- ============================================================

{- | Encrypt one GCM module. The caller must supply a fresh, random
12-byte nonce for every invocation with a given key. Prefer
'encryptModule', which obtains that nonce from 'RNG.MonadRandom'.
-}
encryptGcmModuleWithNonce
  :: ByteString -> ByteString -> ByteString -> ByteString -> Either String ByteString
encryptGcmModuleWithNonce key nonce aad plaintext
  | BS.length nonce /= 12 = Left "Parquet.Encryption: nonce must contain 12 bytes"
  | otherwise = encryptGcmWithNonce key nonce aad plaintext


{- | Explicit-nonce CTR counterpart of 'encryptModuleCtr'. The nonce
must be fresh, random, and exactly 12 bytes. CTR does not authenticate.
-}
encryptCtrModuleWithNonce
  :: ByteString -> ByteString -> ByteString -> Either String ByteString
encryptCtrModuleWithNonce key nonce plaintext
  | BS.length nonce /= 12 = Left "Parquet.Encryption: nonce must contain 12 bytes"
  | otherwise = ctrEncryptWithNonce key nonce plaintext


{- | Decrypt an @nonce || ciphertext || tag@ blob. Same as
'decryptModule' but exported under a name that pairs nicely with
'encryptGcmModuleWithNonce'.
-}
decryptGcmModule :: ByteString -> ByteString -> ByteString -> Either String ByteString
decryptGcmModule = decryptModule


-- | Decrypt an @nonce || ciphertext@ blob.
decryptCtrModule :: ByteString -> ByteString -> Either String ByteString
decryptCtrModule = decryptModuleCtr


-- ============================================================
-- Spec-compliant wire framing (length-prefixed)
-- ============================================================
--
-- Per parquet-format Encryption.md §5.1, every encrypted module on
-- the wire has a 4-byte little-endian length prefix giving the
-- length of the @nonce || ct || (tag)@ blob. The previous helpers
-- emit the raw blob; the *Framed wrappers add the length prefix so
-- the writer stays byte-compatible with parquet-mr / arrow-rs.

-- | Encrypt a module using a fresh random nonce and the section 5.1 framing.
encryptGcmModuleFramed
  :: RNG.MonadRandom m
  => ByteString -> ByteString -> ByteString -> m (Either String ByteString)
encryptGcmModuleFramed key aad plain = case checkFramedPlainSize 28 plain of
  Left err -> pure (Left err)
  Right () -> fmap frameModule <$> encryptModule key aad plain


-- | CTR counterpart of 'encryptGcmModuleFramed'.
encryptCtrModuleFramed
  :: RNG.MonadRandom m
  => ByteString -> ByteString -> m (Either String ByteString)
encryptCtrModuleFramed key plain = case checkFramedPlainSize 12 plain of
  Left err -> pure (Left err)
  Right () -> fmap frameModule <$> encryptModuleCtr key plain


-- | Frame a GCM module encrypted with an explicitly supplied fresh nonce.
encryptGcmModuleFramedWithNonce
  :: ByteString -> ByteString -> ByteString -> ByteString -> Either String ByteString
encryptGcmModuleFramedWithNonce key nonce aad plain = do
  checkFramedPlainSize 28 plain
  frameModule <$> encryptGcmModuleWithNonce key nonce aad plain


-- | Frame a CTR module encrypted with an explicitly supplied fresh nonce.
encryptCtrModuleFramedWithNonce
  :: ByteString -> ByteString -> ByteString -> Either String ByteString
encryptCtrModuleFramedWithNonce key nonce plain = do
  checkFramedPlainSize 12 plain
  frameModule <$> encryptCtrModuleWithNonce key nonce plain


{- | Read one length-prefixed encrypted module from @bs@ starting at
@off@. Returns the raw module bytes (nonce || ct || (tag)) plus
the byte offset just past the module. Caller decrypts via
'decryptGcmModule' / 'decryptCtrModule'.
-}
readFramedModule :: ByteString -> Int -> Either String (ByteString, Int)
readFramedModule bs off
  | off < 0 || off > BS.length bs - 4 =
      Left "Parquet.Encryption: short buffer reading framed module length"
  | otherwise =
      let !len = readLE32 bs off
      in if len > BS.length bs - off - 4
           then Left "Parquet.Encryption: framed module runs past end of buffer"
           else Right (BS.take len (BS.drop (off + 4) bs), off + 4 + len)


-- | Prepend a 4-byte little-endian length to the module bytes.
frameModule :: ByteString -> ByteString
frameModule bs
  | toInteger len > min (toInteger (maxBound :: Word32)) (toInteger (maxBound :: Int) - 4) || len < 0 = error "Parquet.Encryption: encrypted module exceeds framing limit"
  | otherwise =
      B.toStrictByteStringExact (len + 4) $
        B.word32LE (fromIntegral len)
          <> B.byteString bs
  where
    !len = BS.length bs


-- Reject before random generation, cipher allocation or a payload read.
-- The prefix counts nonce/ciphertext/tag; the complete buffer also needs
-- four bytes for that prefix and must fit the host address space.
checkFramedPlainSize :: Int -> ByteString -> Either String ()
checkFramedPlainSize overhead plain =
  let size = toInteger (BS.length plain)
      limit = min (toInteger (maxBound :: Word32) - toInteger overhead) (toInteger (maxBound :: Int) - toInteger overhead - 4)
  in if size < 0 || size > limit
       then Left "Parquet.Encryption: encrypted module exceeds framing limit"
       else Right ()


-- | Read a 32-bit little-endian length from @bs@ at @off@.
readLE32 :: ByteString -> Int -> Int
readLE32 bs off =
  let !b0 = fromIntegral (BS.index bs off) :: Int
      !b1 = fromIntegral (BS.index bs (off + 1)) :: Int
      !b2 = fromIntegral (BS.index bs (off + 2)) :: Int
      !b3 = fromIntegral (BS.index bs (off + 3)) :: Int
  in b0 + b1 * 256 + b2 * 65536 + b3 * 16777216


-- ============================================================
-- Internals
-- ============================================================

{- | Dispatch on key length to one of the three concrete AES variants.
The three branches exist because @crypton@ uses different types for
AES-128 / 192 / 256 to encode key sizes statically; the call-site
continues with whichever branch was selected.
-}
withAes
  :: ByteString
  -> (Crypto.AES128 -> Either String r)
  -> (Crypto.AES192 -> Either String r)
  -> (Crypto.AES256 -> Either String r)
  -> Either String r
withAes key k128 k192 k256 = case BS.length key of
  16 -> case Cipher.cipherInit key :: CryptoFailable Crypto.AES128 of
    CryptoPassed c -> k128 c
    CryptoFailed e -> Left ("Parquet.Encryption: AES128 init: " ++ show e)
  24 -> case Cipher.cipherInit key :: CryptoFailable Crypto.AES192 of
    CryptoPassed c -> k192 c
    CryptoFailed e -> Left ("Parquet.Encryption: AES192 init: " ++ show e)
  32 -> case Cipher.cipherInit key :: CryptoFailable Crypto.AES256 of
    CryptoPassed c -> k256 c
    CryptoFailed e -> Left ("Parquet.Encryption: AES256 init: " ++ show e)
  n -> Left ("Parquet.Encryption: unsupported key length " ++ show n)


{- | Construct an IV whose phantom cipher type matches the supplied cipher.
Forces the @IV@'s polymorphic block size to the concrete one that
'aeadInit' / 'ctrCombine' expect later.
-}
mkIVFor :: forall c. BlockCipher c => c -> ByteString -> Either String (IV c)
mkIVFor _ bs = case Cipher.makeIV bs of
  Just v -> Right v
  Nothing -> Left "Parquet.Encryption: invalid IV length"


adapt :: CryptoFailable a -> Either String a
adapt (CryptoPassed a) = Right a
adapt (CryptoFailed e) = Left ("Parquet.Encryption: " ++ show e)
