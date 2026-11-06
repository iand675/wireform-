{-# LANGUAGE PatternSynonyms #-}

{- | Key routing and authenticated metadata for Parquet Modular Encryption.
Key metadata is opaque; applications can resolve it through their own KMS
and supply the resulting keys. This module performs no network access.
-}
module Parquet.Decryption (
  DecryptionConfig (..),
  defaultDecryptionConfig,
  DecryptedMetadata (..),
  DecryptionState,
  ColumnCrypto,
  ColumnKeyMetadata (..),
  columnKeyMetadata,
  decryptFileMetadata,
  columnCrypto,
  cryptoColumnMetadata,
  decryptColumnModule,
  decryptColumnPageBody,
  decryptColumnPageHeader,
) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Parquet.Encryption qualified as E
import Parquet.Footer qualified as F
import Parquet.Levels qualified as Levels
import Parquet.Thrift.Schema
import Parquet.Types
import Thrift.Decode (decodeCompactFrom)
import Thrift.Value qualified as TV


data DecryptionConfig = DecryptionConfig
  { dcFooterKey :: !(Maybe ByteString)
  -- ^ Explicit footer encryption/signing key, overriding key metadata.
  , dcColumnKeys :: !(Map (V.Vector Text) ByteString)
  -- ^ Explicit keys by complete schema path, overriding key metadata.
  , dcKeyMetadataKeys :: !(Map ByteString ByteString)
  -- ^ Keys resolved by the application from opaque key metadata.
  , dcAadPrefix :: !(Maybe ByteString)
  -- ^ Expected stored prefix, or the prefix omitted by the writer.
  , dcExpectedFileId :: !(Maybe ByteString)
  -- ^ Optional identity check for callers using the original explicit-AAD API.
  }
  deriving stock (Eq)


instance Show DecryptionConfig where
  show config =
    "DecryptionConfig {footerKey = "
      ++ maybe "absent" (const "<redacted>") (dcFooterKey config)
      ++ ", columnKeys = "
      ++ show (Map.size (dcColumnKeys config))
      ++ ", metadataKeys = "
      ++ show (Map.size (dcKeyMetadataKeys config))
      ++ "}"


defaultDecryptionConfig :: DecryptionConfig
defaultDecryptionConfig = DecryptionConfig Nothing Map.empty Map.empty Nothing Nothing


data ColumnOrdinal = ColumnOrdinal
  { ordinalGroup :: {-# UNPACK #-} !Int
  , ordinalColumn :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord)


newtype DecryptionState = DecryptionState (Map ColumnOrdinal ColumnCrypto)
  deriving stock (Eq)


instance Show DecryptionState where
  show (DecryptionState columns) = "DecryptionState {encryptedColumns = " ++ show (Map.size columns) ++ "}"


data ColumnCrypto = ColumnCrypto
  { cryptoKey :: !(Either String ByteString)
  , cryptoAlgorithm :: !E.EncryptionAlgorithm
  , cryptoFileId :: !ByteString
  , cryptoPrefix :: !ByteString
  , cryptoRowGroup :: {-# UNPACK #-} !Int16
  , cryptoOrdinal :: {-# UNPACK #-} !Int16
  , cryptoColumnMetadata :: !(Either String ColumnMetadata)
  , cryptoKeyPath :: !(Maybe (V.Vector Text))
  , cryptoKeyMetadata :: !(Maybe ByteString)
  }
  deriving stock (Eq)


{- | Authenticated routing information, without the resolved encryption key.
Inspect these entries after opening a file with its footer key, resolve
their opaque metadata through an application KMS, then supply the keys in
a configuration for the file. Entries are per row group/physical column.
-}
data ColumnKeyMetadata = ColumnKeyMetadata
  { ckmRowGroup :: {-# UNPACK #-} !Int
  , ckmColumn :: {-# UNPACK #-} !Int
  , ckmPath :: !(V.Vector Text)
  , ckmKeyMetadata :: !(Maybe ByteString)
  }
  deriving stock (Show, Eq)


columnKeyMetadata :: DecryptionState -> V.Vector ColumnKeyMetadata
columnKeyMetadata (DecryptionState columns) = V.fromList (Map.foldrWithKey collect [] columns)
  where
    collect ordinal context entries = case cryptoKeyPath context of
      Nothing -> entries
      Just path -> ColumnKeyMetadata (ordinalGroup ordinal) (ordinalColumn ordinal) path (cryptoKeyMetadata context) : entries


data DecryptedMetadata = DecryptedMetadata
  { decryptedMetadata :: !FileMetadata
  , decryptionState :: !DecryptionState
  }
  deriving stock (Show, Eq)


columnCrypto :: DecryptionState -> Int -> Int -> Maybe ColumnCrypto
columnCrypto (DecryptionState columns) group column = Map.lookup (ColumnOrdinal group column) columns


{- | Authenticate the footer and resolve available column metadata. An absent
or incorrect key for an unrequested column does not prevent projection of
other columns. Its metadata remains absent, so statistics cannot prune on
unreadable data; requesting that column reports its key/authentication error.
-}
decryptFileMetadata :: DecryptionConfig -> ByteString -> Either String DecryptedMetadata
decryptFileMetadata config bytes = do
  routing <- E.readFooterCryptoMetadata bytes
  key <- case routing of
    Nothing -> Right BS.empty
    Just info -> resolveKey (dcFooterKey config) (E.fcmKeyMetadata info)
  authenticated <- E.authenticateFooter key (dcAadPrefix config) (dcExpectedFileId config) bytes
  metadata <- F.thriftToFileMetadata (E.afValue authenticated)
  case E.afCryptoMetadata authenticated of
    Nothing -> Right (DecryptedMetadata metadata (DecryptionState Map.empty))
    Just info -> do
      fields <- struct (E.afValue authenticated)
      rawGroups <- required (FileMetadata_RowGroups V.empty) fields >>= list
      unless (V.length rawGroups == V.length (fmRowGroups metadata)) (Left "Parquet.Decryption: row-group count mismatch")
      let prefix = maybe (maybe BS.empty id (E.fcmStoredAadPrefix info)) id (dcAadPrefix config)
      groups <- V.imapM (prepareGroup metadata info prefix key) rawGroups
      let updated = V.map groupMetadata groups
          contexts = V.foldl' (\allContexts group -> Map.union allContexts (groupContexts group)) Map.empty groups
          sanitized = case fmColumnOrders metadata of
            Nothing -> updated
            Just orders -> V.map (F.sanitizeColumnOrders (fmSchema metadata) orders) updated
      Right (DecryptedMetadata metadata {fmRowGroups = sanitized} (DecryptionState contexts))
  where
    resolveKey explicit opaque = case explicit of
      Just key -> Right key
      Nothing -> maybe (Left "Parquet.Decryption: no key for encrypted metadata") Right (opaque >>= (`Map.lookup` dcKeyMetadataKeys config))
    prepareGroup metadataForGroup info prefix footerKey groupIndex raw = do
      fields <- struct raw
      rawColumns <- required (RowGroup_Columns V.empty) fields >>= list
      let group = fmRowGroups metadataForGroup V.! groupIndex
      unless (V.length rawColumns == V.length (rgColumns group)) (Left "Parquet.Decryption: column count mismatch")
      ordinal <-
        optional (RowGroup_Ordinal 0) fields >>= \case
          Nothing -> Right groupIndex
          Just (TV.I16 value) -> Right (fromIntegral value)
          _ -> Left "Parquet.Decryption: invalid row-group ordinal"
      columns <- V.imapM (prepareColumn metadataForGroup info prefix footerKey groupIndex ordinal) rawColumns
      Right (PreparedGroup group {rgColumns = V.map preparedColumn columns} (V.foldl' (\m col -> maybe m (\context -> Map.insert (preparedOrdinal col) context m) (preparedCrypto col)) Map.empty columns))
    prepareColumn metadata info prefix footerKey groupIndex groupOrdinal columnIndex raw = do
      fields <- struct raw
      description <- optional (ColumnChunk_CryptoMetadata V.empty) fields
      -- The ordinary footer parser conservatively removes index locations
      -- when column metadata is unavailable. Start with the original chunk,
      -- decrypt its metadata, then apply that ordering policy to the result.
      column <- F.thriftToColumnChunk raw
      let ordinal = ColumnOrdinal groupIndex columnIndex
      case description of
        Nothing -> do
          encrypted <- optional (ColumnChunk_EncryptedColumnMetadata BS.empty) fields
          unless (encrypted == Nothing) (Left "Parquet.Decryption: encrypted metadata lacks crypto description")
          Right (PreparedColumn column ordinal Nothing)
        Just descriptionValue -> do
          variants <- struct descriptionValue
          unless (V.length variants == 1) (Left "Parquet.Decryption: column crypto union needs one variant")
          colOrdinal <- checkedOrdinal columnIndex
          rowOrdinal <- checkedOrdinal groupOrdinal
          keyChoice <- case V.head variants of
            ColumnCryptoMetaData_EncryptionWithFooterKey _ -> Right (KeyChoice (Right footerKey) Nothing Nothing)
            ColumnCryptoMetaData_EncryptionWithColumnKey entries -> do
              keyFields <- struct (TV.Struct entries)
              path <- required (EncryptionWithColumnKey_PathInSchema V.empty) keyFields >>= list >>= V.mapM text
              unless (not (V.null path)) (Left "Parquet.Decryption: empty encrypted column path")
              _ <- Levels.maxLevelsForColumnPath (fmSchema metadata) path
              opaque <- optional (EncryptionWithColumnKey_KeyMetadata BS.empty) keyFields >>= traverse binary
              Right (KeyChoice (resolveKey (Map.lookup path (dcColumnKeys config)) opaque) (Just path) opaque)
            _ -> Left "Parquet.Decryption: unsupported column crypto variant"
          encryptedMetadata <- optional (ColumnChunk_EncryptedColumnMetadata BS.empty) fields >>= traverse binary
          let baseContext = ColumnCrypto (chosenKey keyChoice) (E.fcmAlgorithm info) (E.fcmFileId info) prefix rowOrdinal colOrdinal (Left "Parquet.Decryption: column metadata has not been resolved") (chosenPath keyChoice) (chosenMetadata keyChoice)
              context = baseContext {cryptoColumnMetadata = resolved}
              resolved = do
                _ <- chosenKey keyChoice
                decoded <- case encryptedMetadata of
                  Nothing
                    | chosenPath keyChoice /= Nothing || BS.take 4 bytes == F.parquetMagic -> Left "Parquet.Decryption: missing encrypted column metadata"
                    | otherwise -> maybe (Left "Parquet.Decryption: missing column metadata") Right (ccMetadata column)
                  Just ciphertext -> do
                    plaintext <- decryptColumnModule baseContext E.ModuleColumnMetaData ciphertext
                    (value, end) <- decodeCompactFrom plaintext 0
                    unless (BS.all (== 0) (BS.drop end plaintext)) (Left "Parquet.Decryption: trailing encrypted column metadata")
                    F.thriftToColumnMetadata value
                case chosenPath keyChoice of
                  Nothing -> Right ()
                  Just path -> unless (path == cmPathInSchema decoded) (Left "Parquet.Decryption: encrypted column path mismatch")
                Right decoded
          Right (PreparedColumn column {ccMetadata = either (const Nothing) Just resolved} ordinal (Just context))


data KeyChoice = KeyChoice
  { chosenKey :: !(Either String ByteString)
  , chosenPath :: !(Maybe (V.Vector Text))
  , chosenMetadata :: !(Maybe ByteString)
  }


data PreparedGroup = PreparedGroup
  { groupMetadata :: !RowGroup
  , groupContexts :: !(Map ColumnOrdinal ColumnCrypto)
  }


data PreparedColumn = PreparedColumn
  { preparedColumn :: !ColumnChunk
  , preparedOrdinal :: !ColumnOrdinal
  , preparedCrypto :: !(Maybe ColumnCrypto)
  }


checkedOrdinal :: Int -> Either String Int16
checkedOrdinal value
  | value < 0 || value > 32767 = Left "Parquet.Decryption: encryption ordinal exceeds signed 16-bit range"
  | otherwise = Right (fromIntegral value)


decryptColumnModule :: ColumnCrypto -> E.ModuleType -> ByteString -> Either String ByteString
decryptColumnModule context kind bytes = do
  key <- cryptoKey context
  (ciphertext, end) <- E.readFramedModule bytes 0
  unless (end == BS.length bytes) (Left "Parquet.Decryption: trailing encrypted module bytes")
  E.decryptGcmModule key (columnAad context kind 0) ciphertext


decryptColumnPageBody :: ColumnCrypto -> E.ModuleType -> Int -> ByteString -> Either String ByteString
decryptColumnPageBody context kind page bytes = do
  ordinal <- checkedOrdinal page
  key <- cryptoKey context
  case cryptoAlgorithm context of
    E.AesGcmV1 -> E.decryptGcmModule key (columnAad context kind ordinal) bytes
    E.AesGcmCtrV1 -> E.decryptCtrModule key bytes


decryptColumnPageHeader :: ColumnCrypto -> E.ModuleType -> Int -> ByteString -> Either String ByteString
decryptColumnPageHeader context kind page bytes = do
  ordinal <- checkedOrdinal page
  key <- cryptoKey context
  E.decryptGcmModule key (columnAad context kind ordinal) bytes


columnAad :: ColumnCrypto -> E.ModuleType -> Int16 -> ByteString
columnAad context kind page = E.buildAad (cryptoPrefix context) (E.buildAadSuffix (cryptoFileId context) kind (cryptoRowGroup context) (cryptoOrdinal context) page)


struct :: TV.Value -> Either String (V.Vector (Int16, TV.Value))
struct (TV.Struct fields) = do
  unless (Map.size (V.foldl' (\seen (tag, _) -> Map.insert tag () seen) Map.empty fields) == V.length fields) (Left "Parquet.Decryption: duplicate metadata field")
  Right fields
struct _ = Left "Parquet.Decryption: expected metadata struct"


list :: TV.Value -> Either String (V.Vector TV.Value)
list (TV.List _ values) = Right values
list _ = Left "Parquet.Decryption: expected metadata list"


optional :: (Int16, TV.Value) -> V.Vector (Int16, TV.Value) -> Either String (Maybe TV.Value)
optional (tag, _) = Right . fmap snd . V.find ((== tag) . fst)


required :: (Int16, TV.Value) -> V.Vector (Int16, TV.Value) -> Either String TV.Value
required tag fields = optional tag fields >>= maybe (Left "Parquet.Decryption: missing metadata field") Right


binary :: TV.Value -> Either String ByteString
binary (TV.Binary bytes) = Right bytes
binary (TV.String value) = Right (TE.encodeUtf8 value)
binary _ = Left "Parquet.Decryption: expected binary metadata"


text :: TV.Value -> Either String Text
text (TV.String value) = Right value
text (TV.Binary bytes) = either (const (Left "Parquet.Decryption: invalid UTF-8 column path")) Right (TE.decodeUtf8' bytes)
text _ = Left "Parquet.Decryption: expected text column path"
