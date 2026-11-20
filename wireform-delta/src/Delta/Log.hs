{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Delta Lake transaction log reader.

Delta Lake sits on top of Parquet (or, more recently, Iceberg
/ Lance / arbitrary content) data files plus a transaction
log written into a @_delta_log/@ subdirectory of the table
root. The log consists of:

  * Per-version JSON commit files
    @0000000000000000NNNN.json@. Each is newline-delimited
    JSON; each line is a single-key object describing one
    'DeltaAction'.
  * Periodic checkpoint Parquet snapshots
    @0000000000000000NNNN.checkpoint.parquet@ that fold the
    active state into one file so readers don't have to walk
    the entire log from version 0.
  * A @_last_checkpoint@ JSON pointer file naming the most
    recent checkpoint version + size.

Each line in a commit file is one of:

@
{"add":        {"path": ..., "size": ..., "stats": ..., ... }}
{"remove":     {"path": ..., "deletionTimestamp": ..., ... }}
{"metaData":   {"id": ..., "schemaString": ..., ... }}
{"protocol":   {"minReaderVersion": ..., "minWriterVersion": ... }}
{"commitInfo": {"timestamp": ..., "operation": ..., ... }}
{"txn":        {"appId": ..., "version": ..., ... }}
{"cdc":        {"path": ..., ...}}
@

This module:

  * Parses every documented action variant into a strict
    ADT ('DeltaAction').
  * Decodes the schema-string field of @metaData@ into a
    typed 'DeltaSchema' (Spark / Delta JSON schema).
  * Decodes the @stats@ string of an 'AddAction' into a
    typed 'AddStats' record.
  * Replays a sequence of actions into a 'TableSnapshot'
    ('snapshotFromActions') that combines protocol +
    metadata + active file set, which is what scan planners
    consume.
  * Discovers and parses @_last_checkpoint@.

File scans and deletion-vector application live in "Delta.Scan".
-}
module Delta.Log (
  -- * Actions
  DeltaAction (.., ActionCheckpointMetadata),
  AddAction (..),
  RemoveAction (..),
  MetaDataAction (..),
  ProtocolAction (..),
  protocolFromFeatureLists,
  canonicalProtocolFeature,
  CommitInfoAction (..),
  TxnAction (..),
  DomainMetadataAction (..),
  CdcAction (..),
  SidecarAction (..),
  AddStats (..),
  NullCount (..),

  -- * Delta schema (decoded from @metaData.schemaString@)
  DeltaSchema (..),
  DeltaField (..),
  DeltaType (..),
  SparkInterval (..),
  IntervalPart (..),
  parseDeltaSchema,
  parseDeltaType,

  -- * Stats helper
  decodeAddStats,
  normalizePartitionValue,
  normalizeExtendedFileMetadata,
  effectiveTightBounds,

  -- * Table snapshot
  TableSnapshot (TableSnapshot, tsProtocol, tsMetaData, tsFiles, tsAppIds, tsDomains, tsLastCommit),
  emptySnapshot,
  applyAction,
  applyMetadataAction,
  setReplayFiles,
  SnapshotFileMetrics (..),
  snapshotFileMetrics,
  cachedFileMetrics,
  cacheFileMetrics,
  updateFileMetrics,
  restoreStoredFiles,
  resolveSnapshotFilePath,
  sameDeletionVector,
  snapshotFromActions,

  -- * Last-checkpoint pointer
  LastCheckpoint (..),
  parseLastCheckpoint,
  parseLastCheckpointWithPath,

  -- * Log parsing
  parseLogFile,
  parseLogFileChecked,
  foldLogFileChecked,
  foldCommitBytes,
  foldCompactedBytes,
  foldCheckpointJson,
  parseLogLine,
  parseCommitInfo,
  CommitHistoryActions (..),
  parseCommitHistory,
  decodeChecksumFields,
  decodeOptionalChecksumFiles,
  decodeChecksumMetricsFields,
  foldChecksumFiles,
  ChecksumFileHints (..),
  checksumFileHints,
  truncatedCommitPrefix,

  -- * Active-file derivation (compatibility helper)
  activeFiles,
) where

import Crypto.Hash (Digest, MD5, hash)
import Data.Aeson (
  FromJSON (..),
  Value (..),
  withObject,
  (.:),
  (.:?),
 )
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (Lit (..), Number (..), TkArray (..), TkRecord (..), Tokens (..))
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Types (JSONPathElement (Key), Parser, parseEither, parseMaybe, (<?>))
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Builder.Extra qualified as BB
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.HashMap.Strict qualified as HM
import Data.Int (Int32, Int64)
import Data.List (isPrefixOf, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Data.Word (Word8, Word64)
import Delta.DeletionVector (descriptorCardinality)
import Delta.Path (filePathKey, storedPathKeys)
import GHC.Records (HasField (..))


-- ============================================================
-- Actions
-- ============================================================

{- | One line of a Delta log file. The JSON shape is a
single-key object whose key picks the variant.
-}
data DeltaAction
  = ActionAdd !AddAction
  | ActionRemove !RemoveAction
  | ActionMetaData !MetaDataAction
  | ActionProtocol !ProtocolAction
  | ActionCommitInfo !CommitInfoAction
  | ActionTxn !TxnAction
  | ActionDomainMetadata !DomainMetadataAction
  | ActionCdc !CdcAction
  | ActionCheckpointMetadataWithTags !Word64 !(Map.Map Text Text)
  | ActionSidecar !SidecarAction
  | -- | Tagged with the variant name for forward compat.
    ActionOther !Text
  deriving (Show, Eq)


-- | Match checkpoint metadata by version or construct it without tags.
pattern ActionCheckpointMetadata :: Word64 -> DeltaAction
pattern ActionCheckpointMetadata version <- ActionCheckpointMetadataWithTags version _
  where
    ActionCheckpointMetadata version = ActionCheckpointMetadataWithTags version Map.empty


-- | Reference to a Parquet file containing checkpoint file actions.
data SidecarAction = SidecarAction
  { sidecarPath :: !Text
  , sidecarSize :: !Word64
  }
  deriving (Show, Eq)


instance FromJSON SidecarAction where
  parseJSON = withObject "sidecar" $ \o -> do
    path <- o .: "path"
    size <- o .: "sizeInBytes" :: Parser Int64
    _ <- o .: "modificationTime" :: Parser Int64
    if size < 0
      then fail "negative sidecar size"
      else pure (SidecarAction path (fromIntegral size))


-- Delta stores sizes, counts, and checkpoint versions as signed Longs.
-- Use a nonnegative representation after enforcing the wire range.
nonnegativeLong :: Value -> Parser Word64
nonnegativeLong value = do
  n <- parseJSON value :: Parser Int64
  if n >= 0 then pure (fromIntegral n) else fail "expected a nonnegative Delta Long"


{- | The @add@ action: a Parquet (or other-format) data file is
now part of the table.
-}
data AddAction = AddAction
  { addPath :: !Text
  , addSize :: !Word64
  , addModificationTime :: !Int64
  , addDataChange :: !Bool
  , addStats :: !(Maybe Text)
  {- ^ JSON-encoded statistics (numRecords, minValues,
  maxValues, nullCount). See 'decodeAddStats'.
  -}
  , addParsedStats :: !(Maybe AddStats)
  -- ^ Structured checkpoint statistics when JSON statistics are absent.
  , addPartitionValues :: !(Map.Map Text (Maybe Text))
  , addTags :: !(Map.Map Text Text)
  , addDeletionVector :: !(Maybe Value)
  -- ^ Deletion-vector descriptor. See "Delta.DeletionVector" for decoding.
  , addBaseRowId :: !(Maybe Int64)
  , addDefaultRowCommitVersion :: !(Maybe Int64)
  }
  deriving (Show, Eq)


{- | The @remove@ action: a previously-added file is no longer
in the table. Combined with subsequent @add@s, this is how
Delta encodes overwrites.
-}
data RemoveAction = RemoveAction
  { removePath :: !Text
  , removeDeletionTimestamp :: !(Maybe Int64)
  , removeDataChange :: !Bool
  , removeExtendedFileMetadata :: !(Maybe Bool)
  , removeSize :: !(Maybe Word64)
  , removePartitionValues :: !(Map.Map Text (Maybe Text))
  , removeDeletionVector :: !(Maybe Value)
  , removeBaseRowId :: !(Maybe Int64)
  , removeDefaultRowCommitVersion :: !(Maybe Int64)
  }
  deriving (Show, Eq)


{- | The @metaData@ action: table-level schema + properties.
Carried in the very first commit of a table; subsequent
@metaData@s are schema-evolution boundaries.
-}
data MetaDataAction = MetaDataAction
  { mdId :: !Text
  , mdName :: !(Maybe Text)
  , mdDescription :: !(Maybe Text)
  , mdFormat :: !(Maybe (Text, Map.Map Text Text))
  {- ^ @(provider, options)@. The provider is conventionally
  @"parquet"@ for ordinary tables.
  -}
  , mdSchemaString :: !Text
  -- ^ JSON-encoded Spark schema. See 'parseDeltaSchema'.
  , mdPartitionColumns :: ![Text]
  , mdConfiguration :: !(Map.Map Text Text)
  , mdCreatedTime :: !(Maybe Int64)
  }
  deriving (Show, Eq)


{- | The @protocol@ action: minimum reader / writer versions
the table demands. Readers below 'pMinReaderVersion' must
refuse to open the table.
-}
data ProtocolAction = ProtocolAction
  { pMinReaderVersion :: !Int
  , pMinWriterVersion :: !Int
  , pReaderFeatures :: ![Text]
  -- ^ Reader feature names enabled at writer-protocol-7+.
  , pWriterFeatures :: ![Text]
  }
  deriving (Show, Eq)


{- | The @commitInfo@ action: free-form metadata about the
commit. Spark / delta-rs add their own fields here; we keep
the typed slots that are practically standardised and stash
the rest as a 'HM.HashMap'.
-}
data CommitInfoAction = CommitInfoAction
  { ciTimestamp :: !(Maybe Int64)
  , ciInCommitTimestamp :: !(Maybe Int64)
  , ciOperation :: !(Maybe Text)
  , ciOperationParameters :: !(HM.HashMap Text Value)
  , ciIsolationLevel :: !(Maybe Text)
  , ciIsBlindAppend :: !(Maybe Bool)
  , ciTxnVersion :: !(Maybe Word64)
  , ciExtra :: !(HM.HashMap Text Value)
  }
  deriving (Show, Eq)


{- | The @txn@ action: idempotency / set-commit marker for
streaming jobs.
-}
data TxnAction = TxnAction
  { txnAppId :: !Text
  , txnVersion :: !Int64
  , txnLastUpdated :: !(Maybe Int64)
  }
  deriving (Show, Eq)


{- | Configuration in a named metadata domain. Removed actions suppress
the older live configuration during replay.
-}
data DomainMetadataAction = DomainMetadataAction
  { domainName :: !Text
  , domainConfiguration :: !Text
  , domainRemoved :: !Bool
  }
  deriving (Show, Eq)


instance FromJSON DomainMetadataAction where
  parseJSON = withObject "domainMetadata" $ \o ->
    DomainMetadataAction <$> o .: "domain" <*> o .: "configuration" <*> o .: "removed"


{- | The @cdc@ action: a change-data-capture file emitted as
part of an UPDATE / DELETE / MERGE so downstream consumers
can read the diff.
-}
data CdcAction = CdcAction
  { cdcPath :: !Text
  , cdcSize :: !Word64
  , cdcDataChange :: !Bool
  , cdcPartitionValues :: !(Map.Map Text (Maybe Text))
  , cdcTags :: !(Map.Map Text Text)
  }
  deriving (Show, Eq)


-- ============================================================
-- Aeson instances
-- ============================================================

instance FromJSON AddAction where
  parseJSON = withObject "AddAction" $ \o ->
    AddAction
      <$> o .: "path"
      <*> (o .: "size" >>= nonnegativeLong)
      <*> o .: "modificationTime"
      <*> o .: "dataChange"
      <*> o .:? "stats"
      <*> pure Nothing
      <*> (maybe Map.empty (Map.map normalizePartitionValue) <$> o .:? "partitionValues")
      <*> (maybe Map.empty id <$> (o .:? "tags" >>= traverse parseAddTags))
      <*> o .:? "deletionVector"
      <*> o .:? "baseRowId"
      <*> o .:? "defaultRowCommitVersion"


-- Match the checkpoint representation, where null-valued tags are omitted
-- from the public string map. Undefined tags and empty strings differ.
parseAddTags :: Value -> Parser (Map.Map Text Text)
parseAddTags value = Map.mapMaybe id <$> parseJSON value


instance FromJSON RemoveAction where
  parseJSON = withObject "RemoveAction" $ \o -> do
    extended <- o .:? "extendedFileMetadata"
    size <- o .:? "size" >>= traverse nonnegativeLong
    partitions <- o .:? "partitionValues"
    RemoveAction
      <$> o .: "path"
      <*> o .:? "deletionTimestamp"
      <*> (fromMaybe True <$> o .:? "dataChange")
      <*> pure (normalizeExtendedFileMetadata extended (not (isNothing size || isNothing partitions)))
      <*> pure size
      <*> pure (maybe Map.empty (Map.map normalizePartitionValue) partitions)
      <*> o .:? "deletionVector"
      <*> o .:? "baseRowId"
      <*> o .:? "defaultRowCommitVersion"


-- Older writers could assert extended metadata even when size or the
-- partition map was null. Keep the tombstone and any metadata it carries.
normalizeExtendedFileMetadata :: Maybe Bool -> Bool -> Maybe Bool
normalizeExtendedFileMetadata (Just True) False = Just False
normalizeExtendedFileMetadata flag _ = flag
{-# INLINE normalizeExtendedFileMetadata #-}


instance FromJSON MetaDataAction where
  parseJSON = withObject "MetaDataAction" $ \o -> do
    format <- traverse (\value -> parseFormat value <?> Key "format") (AKM.lookup "format" o)
    MetaDataAction
      <$> o .: "id"
      <*> o .:? "name"
      <*> o .:? "description"
      <*> pure format
      <*> o .: "schemaString"
      <*> (fromMaybe [] <$> o .:? "partitionColumns")
      <*> (fromMaybe Map.empty <$> o .:? "configuration")
      <*> o .:? "createdTime"
    where
      parseFormat = withObject "metaData.format" $ \format -> do
        provider <- format .: "provider"
        options <- case AKM.lookup "options" format of
          Nothing -> pure Map.empty
          Just value -> parseJSON value <?> Key "options"
        pure (provider, options)


instance FromJSON ProtocolAction where
  parseJSON = withObject "ProtocolAction" $ \o -> do
    reader <- o .: "minReaderVersion"
    writer <- o .: "minWriterVersion"
    readerFeatures <- o .:? "readerFeatures"
    writerFeatures <- o .:? "writerFeatures"
    either fail pure (protocolFromFeatureLists reader writer readerFeatures writerFeatures)


-- | Validate the version-dependent presence of feature lists before
-- converting an absent list to the public representation.
protocolFromFeatureLists :: Int -> Int -> Maybe [Text] -> Maybe [Text] -> Either String ProtocolAction
protocolFromFeatureLists reader writer readerFeatures writerFeatures
  | reader < 1 || reader > versionLimit = Left "minReaderVersion must be a positive Delta Int"
  | writer < 1 || writer > versionLimit = Left "minWriterVersion must be a positive Delta Int"
  | reader == 3 && writer /= 7 = Left "reader version 3 requires writer version 7"
  | reader == 3 && isNothing readerFeatures = Left "readerFeatures is required at reader version 3"
  | reader < 3 && not (isNothing readerFeatures) = Left "readerFeatures requires reader version 3"
  | writer == 7 && isNothing writerFeatures = Left "writerFeatures is required at writer version 7"
  | writer < 7 && not (isNothing writerFeatures) = Left "writerFeatures requires writer version 7"
  | feature : _ <- filter (`notElem` canonicalWriterFeatures) canonicalReaderFeatures =
      Left ("readerFeatures entry missing from writerFeatures: " ++ T.unpack feature)
  | feature : _ <- filter orphaned canonicalWriterFeatures =
      Left ("writerFeatures entry missing from readerFeatures: " ++ T.unpack feature)
  | otherwise = Right (ProtocolAction reader writer (fromMaybe [] readerFeatures) (fromMaybe [] writerFeatures))
  where
    versionLimit = fromIntegral (maxBound :: Int32)
    canonicalReaderFeatures = map canonicalProtocolFeature (fromMaybe [] readerFeatures)
    canonicalWriterFeatures = map canonicalProtocolFeature (fromMaybe [] writerFeatures)
    orphaned feature =
      feature `elem` readerWriterFeatures
        && feature `notElem` canonicalReaderFeatures
        && not (feature == "columnMapping" && reader >= 2)


-- | Older producers used this spelling of the timestamp feature.
canonicalProtocolFeature :: Text -> Text
canonicalProtocolFeature "timestampWithoutTimezone" = "timestampNtz"
canonicalProtocolFeature feature = feature


-- Unknown writer features may be writer-only. Known read/write features
-- must appear on the reader side too, including ones we cannot implement.
readerWriterFeatures :: [Text]
readerWriterFeatures =
  [ "columnMapping"
  , "deletionVectors"
  , "timestampNtz"
  , "v2Checkpoint"
  , "vacuumProtocolCheck"
  , "typeWidening"
  , "typeWidening-preview"
  , "variantType"
  , "variantType-preview"
  , "variantShredding"
  , "variantShredding-preview"
  , "catalogManaged"
  , "catalogOwned"
  , "catalogOwned-preview"
  , "adaptiveMetadata-preview"
  , "fileType"
  , "fileType-preview"
  , "geospatial"
  ]


instance FromJSON CommitInfoAction where
  parseJSON = withObject "CommitInfoAction" $ \o -> do
    let (t, afterTimestamp) = takeCommitInfoField "timestamp" o
        (ict, afterInCommit) = takeCommitInfoField "inCommitTimestamp" afterTimestamp
        (op, afterOperation) = takeCommitInfoField "operation" afterInCommit
        (opPar, afterParameters) = takeCommitInfoField "operationParameters" afterOperation
        (iso, afterIsolation) = takeCommitInfoField "isolationLevel" afterParameters
        (blind, afterBlindAppend) = takeCommitInfoField "isBlindAppend" afterIsolation
        (txnVer, extra) = takeCommitInfoField "txnVersion" afterBlindAppend
    pure
      CommitInfoAction
        { ciTimestamp = t
        , ciInCommitTimestamp = ict
        , ciOperation = op
        , ciOperationParameters = fromMaybe HM.empty opPar
        , ciIsolationLevel = iso
        , ciIsBlindAppend = blind
        , ciTxnVersion = txnVer
        , ciExtra = AKM.toHashMapText extra
        }


-- The protocol lets writers store arbitrary JSON in commitInfo. Keep
-- conventional fields in the typed slots when their shape matches, and
-- preserve other values (including unconventional types) in ciExtra.
takeCommitInfoField :: FromJSON a => AK.Key -> AKM.KeyMap Value -> (Maybe a, AKM.KeyMap Value)
takeCommitInfoField name fields =
  case parseMaybe (.:? name) fields of
    Just value -> (value, AKM.delete name fields)
    Nothing -> (Nothing, fields)


instance FromJSON TxnAction where
  parseJSON = withObject "TxnAction" $ \o ->
    TxnAction
      <$> o .: "appId"
      <*> o .: "version"
      <*> o .:? "lastUpdated"


instance FromJSON CdcAction where
  parseJSON = withObject "CdcAction" $ \o ->
    CdcAction
      <$> o .: "path"
      <*> (o .: "size" >>= nonnegativeLong)
      <*> (fromMaybe False <$> o .:? "dataChange")
      <*> (maybe Map.empty (Map.map normalizePartitionValue) <$> o .:? "partitionValues")
      <*> (fromMaybe Map.empty <$> o .:? "tags")


{- | Delta's partition serialization uses both JSON null and the empty
string for a null partition value.
-}
normalizePartitionValue :: Maybe Text -> Maybe Text
normalizePartitionValue (Just value) | T.null value = Nothing
normalizePartitionValue value = value


-- ============================================================
-- Stats parsing (the @stats@ string of an AddAction)
-- ============================================================

-- | Decoded @stats@ JSON for an 'AddAction'.
data AddStats = AddStats
  { asNumRecords :: !(Maybe Word64)
  , asMinValues :: !(HM.HashMap Text Value)
  , asMaxValues :: !(HM.HashMap Text Value)
  , asNullCount :: ~(HM.HashMap Text Word64)
  -- ^ Counts for top-level scalar columns.
  , asNullCounts :: !(HM.HashMap Text NullCount)
  -- ^ Complete null counts, including nested struct fields.
  , asTightBounds :: !(Maybe Bool)
  }
  deriving (Show, Eq)


data NullCount = NullCount !Word64 | NestedNullCounts !(HM.HashMap Text NullCount)
  deriving (Show, Eq)


instance FromJSON NullCount where
  parseJSON (Object children) = NestedNullCounts <$> parseNullCounts children
  parseJSON value = NullCount <$> nonnegativeLong value


parseNullCounts :: AKM.KeyMap Value -> Parser (HM.HashMap Text NullCount)
parseNullCounts = parseJSON . Object . AKM.filter (/= Null)


-- | Delta uses tight bounds when the writer omits @tightBounds@.
effectiveTightBounds :: AddStats -> Bool
effectiveTightBounds = fromMaybe True . asTightBounds


instance FromJSON AddStats where
  parseJSON = withObject "AddStats" $ \o -> do
    n <- o .:? "numRecords" >>= traverse nonnegativeLong
    mn <- fromMaybe HM.empty <$> o .:? "minValues"
    mx <- fromMaybe HM.empty <$> o .:? "maxValues"
    nullCounts <- maybe (pure HM.empty) parseNullCounts =<< o .:? "nullCount"
    tb <- o .:? "tightBounds"
    pure
      AddStats
        { asNumRecords = n
        , asMinValues = mn
        , asMaxValues = mx
        , asNullCount = HM.mapMaybe scalarCount nullCounts
        , asNullCounts = nullCounts
        , asTightBounds = tb
        }
    where
      scalarCount (NullCount n) = Just n
      scalarCount (NestedNullCounts _) = Nothing


{- | Decode an add action's JSON or structured checkpoint statistics.
Returns:

  * 'Nothing'        — the writer did not emit statistics for this
                       file (legal per the Delta spec).
  * 'Just (Right s)' — typed 'AddStats' decoded successfully.
  * 'Just (Left e)'  — @stats@ was present but could not be decoded.

Saves callers from manually plumbing through
@decode . encodeUtf8 . addStats@.
-}
decodeAddStats :: AddAction -> Maybe (Either String AddStats)
decodeAddStats a = case addStats a of
  Nothing -> Right <$> addParsedStats a
  Just t -> Just (decodeJsonAddStats t)
{-# INLINE decodeAddStats #-}


-- Keep the JSON decoder out of the small dispatch that callers can fuse
-- with their checks for absent or already-decoded statistics.
decodeJsonAddStats :: Text -> Either String AddStats
decodeJsonAddStats t = case decodeDeltaJson (TE.encodeUtf8 t) of
  Right stats -> Right stats
  Left err -> Left ("Delta.Log.decodeAddStats: " ++ err)
{-# NOINLINE decodeJsonAddStats #-}


-- ============================================================
-- Delta schema (subset — primitives + structs + arrays + maps)
-- ============================================================

{- | The decoded form of an @AddAction@'s @schemaString@. Mirrors
the JSON shape Spark / Delta emit:

@
{ "type": "struct", "fields": [
    { "name": "id", "type": "long", "nullable": false, "metadata": {} },
    ...
  ]
}
@
-}
data DeltaSchema = DeltaSchema
  { dsFields :: ![DeltaField]
  }
  deriving (Show, Eq)


data DeltaField = DeltaField
  { dfName :: !Text
  , dfType :: !DeltaType
  , dfNullable :: !Bool
  , dfMetadata :: !(HM.HashMap Text Value)
  }
  deriving (Show, Eq)


{- | Delta type names follow Spark's: primitives are strings
(@"long"@, @"string"@, @"boolean"@, …), and @struct@,
@array@, @map@ are nested objects with their own @type@ tags.
-}
data DeltaType
  = DTString
  | DTLong
  | DTInteger
  | DTShort
  | DTByte
  | DTFloat
  | DTDouble
  | DTBoolean
  | DTBinary
  | DTDate
  | DTTimestamp
  | DTTimestampNtz
  | DTVoid
  | DTVariant
  | DTSparkInterval !SparkInterval
  | -- | precision, scale
    DTDecimal !Int !Int
  | -- | element type + containsNull
    DTArray !DeltaType !Bool
  | -- | key, value, valueContainsNull
    DTMap !DeltaType !DeltaType !Bool
  | DTStruct ![DeltaField]
  | DTUnknown !Text
  deriving (Show, Eq)


-- | Spark's ANSI interval extensions. Qualifiers specify the fields
-- permitted in serialized partition values.
data IntervalPart = Year | Month | Day | Hour | Minute | Second
  deriving (Show, Eq, Ord, Enum)


data SparkInterval = SparkInterval !IntervalPart !IntervalPart
  deriving (Show, Eq)


parseSparkInterval :: Text -> Maybe SparkInterval
parseSparkInterval name = do
  qualifier <- T.stripPrefix "interval " name
  let (first, suffix) = T.breakOn " to " qualifier
  start <- part first
  end <- if T.null suffix then Just start else T.stripPrefix " to " suffix >>= part
  if start <= end && (start <= Month) == (end <= Month)
    then Just (SparkInterval start end)
    else Nothing
  where
    part "year" = Just Year
    part "month" = Just Month
    part "day" = Just Day
    part "hour" = Just Hour
    part "minute" = Just Minute
    part "second" = Just Second
    part _ = Nothing


-- | Decode a raw @schemaString@ into a typed 'DeltaSchema'.
parseDeltaSchema :: Text -> Either String DeltaSchema
parseDeltaSchema source = decodeDeltaJson (TE.encodeUtf8 source) >>= parseEither parseSchema
  where
    parseSchema value = do
      ty <- parseDeltaType value
      case ty of
        DTStruct fields -> pure (DeltaSchema fields)
        _ -> fail "expected a struct schema"


parseDeltaType :: Value -> Parser DeltaType
parseDeltaType (String name) = case name of
  "string" -> pure DTString
  "long" -> pure DTLong
  "integer" -> pure DTInteger
  "short" -> pure DTShort
  "byte" -> pure DTByte
  "float" -> pure DTFloat
  "double" -> pure DTDouble
  "boolean" -> pure DTBoolean
  "binary" -> pure DTBinary
  "date" -> pure DTDate
  "timestamp" -> pure DTTimestamp
  "timestamp_ntz" -> pure DTTimestampNtz
  "void" -> pure DTVoid
  "variant" -> pure DTVariant
  other
    | "decimal(" `T.isPrefixOf` other ->
        case parseDecimal other of
          Just (precision, scale) -> pure (DTDecimal precision scale)
          Nothing -> fail "invalid decimal precision or scale"
    | Just interval <- parseSparkInterval other -> pure (DTSparkInterval interval)
    | otherwise -> pure (DTUnknown other)
parseDeltaType value = withObject "Delta schema type" parseComplex value
  where
    parseComplex o = do
      name <- o .: "type"
      case name of
        "struct" -> do
          fields <- o .: "fields" >>= traverse parseField
          let names = Set.fromList (map (T.toLower . dfName) fields)
          if Set.size names == length fields
            then pure (DTStruct fields)
            else fail "duplicate Delta field names within a struct"
        "array" -> DTArray <$> (o .: "elementType" >>= parseDeltaType) <*> o .: "containsNull"
        "map" -> DTMap <$> (o .: "keyType" >>= parseDeltaType) <*> (o .: "valueType" >>= parseDeltaType) <*> o .: "valueContainsNull"
        "variant" -> pure DTVariant
        other -> pure (DTUnknown other)


parseField :: Value -> Parser DeltaField
parseField = withObject "Delta schema field" $ \o ->
  DeltaField
    <$> o .: "name"
    <*> (o .: "type" >>= parseDeltaType)
    <*> o .: "nullable"
    <*> o .: "metadata"


-- @"decimal(10,2)"@ → @Just (10, 2)@. Used by 'parseDeltaType'.
parseDecimal :: Text -> Maybe (Int, Int)
parseDecimal t = do
  rest1 <- T.stripPrefix "decimal(" t
  rest2 <- T.stripSuffix ")" rest1
  case T.splitOn "," rest2 of
    [a, b] -> do
      pa <- readInt (T.strip a)
      pb <- readInt (T.strip b)
      if 1 <= pa && pa <= 38 && 0 <= pb && pb <= pa
        then Just (fromInteger pa, fromInteger pb)
        else Nothing
    _ -> Nothing
  where
    readInt s = case TR.decimal s of
      Right (n, rest) | T.null rest -> Just (n :: Integer)
      _ -> Nothing


-- ============================================================
-- Snapshot
-- ============================================================

{- | Active state of the table at a particular log version.
Combines protocol, metadata, the live file set, and the
last recorded transaction version for each app id.
-}
data TableSnapshot = Snapshot
  { snapshotProtocol :: !(Maybe ProtocolAction)
  , snapshotMetadata :: !(Maybe MetaDataAction)
  , snapshotFiles :: !(Map.Map Text AddAction)
  , snapshotAppIds :: !(Map.Map Text Int64)
  , snapshotDomains :: !(Map.Map Text Text)
  , snapshotLastCommit :: !(Maybe CommitInfoAction)
  -- Build lazily for snapshots supplied by callers. Internal updates
  -- carry the index forward to the next file action.
  , snapshotAliases :: ~(Map.Map Text (Set.Set Text))
  , cachedFileMetrics :: !(Maybe SnapshotFileMetrics)
  }


-- | Live file keys retain the spelling stored in each add action.
pattern TableSnapshot
  :: Maybe ProtocolAction
  -> Maybe MetaDataAction
  -> Map.Map Text AddAction
  -> Map.Map Text Int64
  -> Map.Map Text Text
  -> Maybe CommitInfoAction
  -> TableSnapshot
pattern TableSnapshot {tsProtocol, tsMetaData, tsFiles, tsAppIds, tsDomains, tsLastCommit} <-
  Snapshot tsProtocol tsMetaData tsFiles tsAppIds tsDomains tsLastCommit _ _
  where
    TableSnapshot protocol metadata files apps domains commit =
      Snapshot protocol metadata files apps domains commit (pathAliases files) Nothing
{-# COMPLETE TableSnapshot #-}

instance HasField "tsProtocol" TableSnapshot (Maybe ProtocolAction) where
  getField = tsProtocol

instance HasField "tsMetaData" TableSnapshot (Maybe MetaDataAction) where
  getField = tsMetaData

instance HasField "tsFiles" TableSnapshot (Map.Map Text AddAction) where
  getField = tsFiles

instance HasField "tsAppIds" TableSnapshot (Map.Map Text Int64) where
  getField = tsAppIds

instance HasField "tsDomains" TableSnapshot (Map.Map Text Text) where
  getField = tsDomains

instance HasField "tsLastCommit" TableSnapshot (Maybe CommitInfoAction) where
  getField = tsLastCommit


instance Eq TableSnapshot where
  a == b =
    tsProtocol a == tsProtocol b
      && tsMetaData a == tsMetaData b
      && tsFiles a == tsFiles b
      && tsAppIds a == tsAppIds b
      && tsDomains a == tsDomains b
      && tsLastCommit a == tsLastCommit b


instance Show TableSnapshot where
  showsPrec d snapshot = showParen (d > 10) $
    showString "TableSnapshot {tsProtocol = " . shows (tsProtocol snapshot)
      . showString ", tsMetaData = " . shows (tsMetaData snapshot)
      . showString ", tsFiles = " . shows (tsFiles snapshot)
      . showString ", tsAppIds = " . shows (tsAppIds snapshot)
      . showString ", tsDomains = " . shows (tsDomains snapshot)
      . showString ", tsLastCommit = " . shows (tsLastCommit snapshot)
      . showChar '}'


emptySnapshot :: TableSnapshot
emptySnapshot =
  TableSnapshot
    { tsProtocol = Nothing
    , tsMetaData = Nothing
    , tsFiles = Map.empty
    , tsAppIds = Map.empty
    , tsDomains = Map.empty
    , tsLastCommit = Nothing
    }


{- | Apply one action to the snapshot. Strict in the snapshot
so a long replay doesn't build a thunk chain.
-}
applyAction :: TableSnapshot -> DeltaAction -> TableSnapshot
applyAction !snapshot = \case
  ActionAdd add ->
    let !updatedFiles = Map.insert path add (if previous == path then files else Map.delete previous files)
        path = addPath add
        key = filePathKey path
        previous = resolve key path
        withoutPrevious = if previous == path then aliases else deletePathAlias key previous aliases
        !updatedAliases = insertPathAlias key path withoutPrevious
    in snapshot {snapshotFiles = updatedFiles, snapshotAliases = updatedAliases, cachedFileMetrics = Nothing}
  ActionRemove remove ->
    let path = removePath remove
        key = filePathKey path
        stored = resolve key path
    in case Map.lookup stored files of
      Just add | Nothing <- removeMatching remove add ->
        let !updatedAliases = deletePathAlias key stored aliases
        in snapshot {snapshotFiles = Map.delete stored files, snapshotAliases = updatedAliases, cachedFileMetrics = Nothing}
      _ -> snapshot
  action -> applyMetadataAction snapshot action
  where
    files = snapshotFiles snapshot
    aliases = snapshotAliases snapshot
    resolve key path
      | Map.member path files = path
      | Just alternate <- Map.lookup key aliases >>= Set.lookupMin
      , alternate < key || not (Map.member key files) = alternate
      | otherwise = key


pathAliases :: Map.Map Text a -> Map.Map Text (Set.Set Text)
pathAliases = Map.foldlWithKey' collect Map.empty
  where
    collect aliases path _ = insertPathAlias (filePathKey path) path aliases


-- | Find a unique stored spelling for a path whose direct lookup failed.
-- If no alias is indexed, return the canonical spelling for lookup.
resolveSnapshotFilePath :: Text -> TableSnapshot -> Either String Text
resolveSnapshotFilePath path snapshot =
  case Map.lookup key (snapshotAliases snapshot) of
    Nothing -> Right key
    Just aliases
      | Set.size aliases == 1, not (Map.member key (tsFiles snapshot)) -> Right (Set.findMin aliases)
      | otherwise -> Left "contains an ambiguous path"
  where
    key = filePathKey path


insertPathAlias :: Text -> Text -> Map.Map Text (Set.Set Text) -> Map.Map Text (Set.Set Text)
insertPathAlias key path aliases
  | key == path = aliases
  | otherwise = Map.insertWith Set.union key (Set.singleton path) aliases
{-# INLINE insertPathAlias #-}


deletePathAlias :: Text -> Text -> Map.Map Text (Set.Set Text) -> Map.Map Text (Set.Set Text)
deletePathAlias key path aliases
  | key == path = aliases
  | otherwise = Map.update remaining key aliases
  where
    remaining paths =
      let updated = Set.delete path paths
      in if Set.null updated then Nothing else Just updated
{-# INLINE deletePathAlias #-}


-- | Install files keyed by reconciliation path during bulk replay.
setReplayFiles :: Map.Map Text AddAction -> TableSnapshot -> TableSnapshot
setReplayFiles files snapshot = snapshot {snapshotFiles = files, snapshotAliases = Map.empty, cachedFileMetrics = Nothing}


-- A table can have enough individually valid signed Long values to overflow
-- Word64. Keep the carries so adding or removing files remains reversible.
data SnapshotFileMetrics = SnapshotFileMetrics
  {-# UNPACK #-} !Word64 {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Word64 {-# UNPACK #-} !Int
  deriving stock (Eq)


snapshotFileMetrics :: TableSnapshot -> SnapshotFileMetrics
snapshotFileMetrics snapshot =
  fromMaybe (Map.foldl' (updateFileMetrics 1) (SnapshotFileMetrics 0 0 0 0 0) (tsFiles snapshot)) (cachedFileMetrics snapshot)


cacheFileMetrics :: SnapshotFileMetrics -> TableSnapshot -> TableSnapshot
cacheFileMetrics metrics snapshot = snapshot {cachedFileMetrics = Just metrics}


-- Replayed descriptors have already been checked. Public snapshots can also
-- be assembled manually, so obtaining their totals must be total.
updateFileMetrics :: Int -> SnapshotFileMetrics -> AddAction -> SnapshotFileMetrics
updateFileMetrics direction (SnapshotFileMetrics bytes byteCarry vectors deleted rowCarry) add =
  case addDeletionVector add of
    Nothing -> SnapshotFileMetrics size sizeCarry vectors deleted rowCarry
    Just dv ->
      let count = either (const 0) id (descriptorCardinality dv)
          rows = adjust deleted count
      in SnapshotFileMetrics size sizeCarry (vectors + direction) rows (carry rowCarry deleted rows)
  where
    adding = direction > 0
    adjust = if adding then (+) else (-)
    carry high previous next
      | adding && next < previous = high + 1
      | not adding && next > previous = high - 1
      | otherwise = high
    size = adjust bytes (addSize add)
    sizeCarry = carry byteCarry bytes size
{-# INLINE updateFileMetrics #-}


-- | Restore the public file keys and keep an index of aliases for incremental updates.
restoreStoredFiles :: TableSnapshot -> TableSnapshot
restoreStoredFiles snapshot =
  snapshot {snapshotFiles = restored, snapshotAliases = pathAliases restored}
  where
    restored = storedPathKeys addPath (snapshotFiles snapshot)


applyActionWithPaths
  :: (AddAction -> Map.Map Text AddAction -> Map.Map Text AddAction)
  -> (RemoveAction -> Map.Map Text AddAction -> Map.Map Text AddAction)
  -> TableSnapshot
  -> DeltaAction
  -> TableSnapshot
applyActionWithPaths addFile removeFile !s = \case
  ActionAdd a -> s {snapshotFiles = addFile a (snapshotFiles s), cachedFileMetrics = Nothing}
  ActionRemove r -> s {snapshotFiles = removeFile r (snapshotFiles s), cachedFileMetrics = Nothing}
  action -> applyMetadataAction s action


-- | Apply non-file actions while the caller reconciles file entries.
applyMetadataAction :: TableSnapshot -> DeltaAction -> TableSnapshot
applyMetadataAction !s = \case
  ActionMetaData md -> s {snapshotMetadata = Just md}
  ActionProtocol p -> s {snapshotProtocol = Just p}
  ActionCommitInfo c -> s {snapshotLastCommit = Just c}
  ActionTxn t -> s {snapshotAppIds = Map.insert (txnAppId t) (txnVersion t) (snapshotAppIds s)}
  ActionDomainMetadata d ->
    s
      { snapshotDomains =
          if domainRemoved d
            then Map.delete (domainName d) (snapshotDomains s)
            else Map.insert (domainName d) (domainConfiguration d) (snapshotDomains s)
      }
  _ -> s


-- A tombstone for an earlier deletion vector can appear after its replacement.
removeMatching :: RemoveAction -> AddAction -> Maybe AddAction
removeMatching r a
  | sameDeletionVector (removeDeletionVector r) (addDeletionVector a) = Nothing
  | otherwise = Just a


-- Only these descriptor fields identify a deletion vector. Size and
-- cardinality describe its contents, and are irrelevant to reconciliation.
sameDeletionVector :: Maybe Value -> Maybe Value -> Bool
sameDeletionVector Nothing Nothing = True
sameDeletionVector (Just (Object left)) (Just (Object right)) =
  matches "storageType" && matches "pathOrInlineDv" && offset left == offset right
  where
    matches key = case AKM.lookup key left of
      Just v@(String _) -> AKM.lookup key right == Just v
      _ -> False
    offset = fromMaybe Null . AKM.lookup "offset"
sameDeletionVector _ _ = False


{- | Replay a chronologically-ordered list of actions into a
'TableSnapshot'.
-}
snapshotFromActions :: [DeltaAction] -> TableSnapshot
snapshotFromActions actions =
  restoreStoredFiles (foldl' (applyActionWithPaths addFile removeFile) emptySnapshot actions)
  where
    addFile a = Map.insert (filePathKey (addPath a)) a
    removeFile r = Map.update (removeMatching r) (filePathKey (removePath r))


-- ============================================================
-- Last checkpoint pointer (@_last_checkpoint@ JSON)
-- ============================================================

{- | Decoded @_last_checkpoint@ pointer. Lives at the table
root in @_delta_log/_last_checkpoint@. Lets readers skip the
log walk by jumping straight to the most recent checkpoint
file.
-}
data LastCheckpoint = LastCheckpoint
  { lcVersion :: !Word64
  , lcSize :: !Word64
  -- ^ Number of actions in the checkpoint.
  , lcParts :: !(Maybe Word64)
  {- ^ For multi-part checkpoints: number of @.parquet@ files
  holding this checkpoint.
  -}
  , lcSizeInBytes :: !(Maybe Word64)
  , lcNumOfAddFiles :: !(Maybe Word64)
  }
  deriving (Show, Eq)


instance FromJSON LastCheckpoint where
  parseJSON = withObject "LastCheckpoint" $ \o ->
    LastCheckpoint
      <$> (o .: "version" >>= nonnegativeLong)
      <*> o .: "size"
      <*> o .:? "parts"
      <*> o .:? "sizeInBytes"
      <*> o .:? "numOfAddFiles"


{- | Parse @_last_checkpoint@ JSON. Returns 'Nothing' on
parser failure (the file is allowed to be absent or
malformed; readers fall back to a full log walk).
-}
parseLastCheckpoint :: BS.ByteString -> Maybe LastCheckpoint
parseLastCheckpoint = fmap fst . parseLastCheckpointWithPath


-- | Also extract the optional V2 manifest filename. Some writers include
-- large embedded schemas and actions which the table opener does not need.
parseLastCheckpointWithPath :: BS.ByteString -> Maybe (LastCheckpoint, Maybe Text)
parseLastCheckpointWithPath input = either (const Nothing) Just $ do
  value <- decodeDeltaJson input
  parseEither (withObject "LastCheckpoint" validate) value
  where
    validate fields = do
      case AKM.lookup "checksum" fields of
        Nothing -> pure ()
        Just (String checksum)
          | checkpointChecksumMatches checksum input -> pure ()
        _ -> fail "_last_checkpoint checksum mismatch"
      let !manifest = case AKM.lookup "v2Checkpoint" fields of
            Just (Object v2) | Just (String path) <- AKM.lookup "path" v2 -> Just path
            _ -> Nothing
      (,manifest) <$> parseJSON (Object fields)


-- Called after decodeDeltaJson has validated the syntax and rejected duplicates.
checkpointChecksumMatches :: Text -> BS.ByteString -> Bool
checkpointChecksumMatches checksum input =
  T.length checksum == 32 && case collect BS.empty (checkpointNumberLiterals input) (bsToTokens input) [] of
    Just ([], entries, _) ->
      matches (map render (sortOn fst entries)) || matches (sort (map render entries))
    _ -> False
  where
    render (path, value) = path <> "=" <> value
    matches pairs = checksum == T.pack (show (hash (BS.intercalate "," pairs) :: Digest MD5))
    -- The protocol sorts by path. Spark sorts joined path/value pairs,
    -- which differs for paths ending in prefix-related array indices.
    collect :: BS.ByteString -> [BS.ByteString] -> Tokens k String -> [(BS.ByteString, BS.ByteString)]
      -> Maybe ([BS.ByteString], [(BS.ByteString, BS.ByteString)], k)
    collect !path numbers token !entries = case token of
      TkRecordOpen members -> object path numbers members entries
      TkArrayOpen items -> array path 0 numbers items entries
      TkText str next -> Just (numbers, (path, quoted str) : entries, next)
      TkLit literal next -> Just (numbers, (path, renderLiteral literal) : entries, next)
      TkNumber _ next -> case numbers of
        raw : rest -> Just (rest, (path, raw) : entries, next)
        [] -> Nothing
      TkErr _ -> Nothing
    object !path numbers members !entries = case members of
      TkPair "checksum" (TkText _ next) | BS.null path -> object path numbers next entries
      TkPair key child -> do
        (rest, updated, next) <- collect (append path (quoted (AK.toText key))) numbers child entries
        object path rest next updated
      TkRecordEnd next -> Just (numbers, entries, next)
      TkRecordErr _ -> Nothing
    array !path !index numbers items !entries = case items of
      TkItem child -> do
        (rest, updated, next) <- collect (append path (BSC.pack (show (index :: Int)))) numbers child entries
        array path (index + 1) rest next updated
      TkArrayEnd next -> Just (numbers, entries, next)
      TkArrayErr _ -> Nothing
    renderLiteral LitNull = "null"
    renderLiteral LitFalse = "false"
    renderLiteral LitTrue = "true"
    append parent component
      | BS.null parent = component
      | otherwise = parent <> "+" <> component
    quoted str = BL.toStrict $ BB.toLazyByteStringWith (BB.safeStrategy 64 256) BL.empty $
      BB.char8 '"' <> BS.foldl' escape mempty (TE.encodeUtf8 str) <> BB.char8 '"'
    escape builder byte
      | (byte >= 65 && byte <= 90) || (byte >= 97 && byte <= 122)
          || (byte >= 48 && byte <= 57) || byte == 45 || byte == 46 || byte == 95 || byte == 126 =
          builder <> BB.word8 byte
      | otherwise =
          let (high, low) = byte `quotRem` 16
          in builder <> BB.char8 '%' <> BB.word8 (hex high) <> BB.word8 (hex low)
    hex digit = if digit < 10 then digit + 48 else digit + 55


-- Aeson's tokens preserve structure and decoded strings. Take numeric
-- literals from the same valid JSON to retain exponents and signed zero.
checkpointNumberLiterals :: BS.ByteString -> [BS.ByteString]
checkpointNumberLiterals bytes = case BS.findIndex startsToken bytes of
  Nothing -> []
  Just offset ->
    let token = BS.drop offset bytes
    in if BSU.unsafeHead token == 0x22
        then checkpointNumberLiterals (skipString (BS.drop 1 token))
        else let (number, rest) = BS.span inNumber token in number : checkpointNumberLiterals rest
  where
    isDigit byte = byte - 0x30 <= 9
    startsToken byte = byte == 0x22 || byte == 0x2d || isDigit byte
    inNumber byte = isDigit byte || byte == 0x2d || byte == 0x2b || byte == 0x2e || byte == 0x45 || byte == 0x65
    skipString contents = case BS.findIndex (\byte -> byte == 0x22 || byte == 0x5c) contents of
      Nothing -> BS.empty
      Just offset
        | BSU.unsafeIndex contents offset == 0x22 -> BS.drop (offset + 1) contents
        | otherwise -> skipString (BS.drop (offset + 2) contents)


-- ============================================================
-- Log line / file parsers
-- ============================================================

-- | Parse one NDJSON line of a Delta log file. A line must contain one
-- action, and the payload of a known action must match its schema.
parseLogLine :: BL.ByteString -> Maybe DeltaAction
parseLogLine = either (const Nothing) Just . decodeActionLine Nothing . BL.toStrict


logActionParsers :: [(AK.Key, Value -> Parser DeltaAction)]
logActionParsers =
  [ ("add", fmap ActionAdd . parseJSON)
  , ("remove", fmap ActionRemove . parseJSON)
  , ("metaData", fmap ActionMetaData . parseJSON)
  , ("protocol", fmap ActionProtocol . parseJSON)
  , ("commitInfo", fmap ActionCommitInfo . parseJSON)
  , ("txn", fmap ActionTxn . parseJSON)
  , ("domainMetadata", fmap ActionDomainMetadata . parseJSON)
  , ("cdc", fmap ActionCdc . parseJSON)
  , ("checkpointMetadata", withObject "checkpointMetadata" (\o ->
        ActionCheckpointMetadataWithTags <$> (o .: "version" >>= nonnegativeLong) <*> (fromMaybe Map.empty <$> o .:? "tags")))
  , ("sidecar", fmap ActionSidecar . parseJSON)
  ]


-- | Parse every NDJSON line of a single log file.
parseLogFile :: BL.ByteString -> [DeltaAction]
parseLogFile bs = go (BL.split 0x0A bs)
  where
    go [] = []
    go (l : ls)
      | BL.null l = go ls
      | otherwise = case parseLogLine l of
          Just a -> a : go ls
          Nothing -> go ls


-- | Find the provenance record without decoding unrelated commit actions.
parseCommitInfo :: BS.ByteString -> Maybe CommitInfoAction
parseCommitInfo = historyCommitInfo . parseHistoryActions False


data CommitHistoryActions = CommitHistoryActions
  { historyCommitInfo :: !(Maybe CommitInfoAction)
  , historyFirstTransaction :: !(Maybe TxnAction)
  }


-- | Read provenance and the first transaction marker in one pass through a
-- commit. History needs neither file actions nor the other snapshot changes.
parseCommitHistory :: BS.ByteString -> CommitHistoryActions
parseCommitHistory = parseHistoryActions True


parseHistoryActions :: Bool -> BS.ByteString -> CommitHistoryActions
parseHistoryActions includeTransaction = go Nothing Nothing
  where
    go !info !transaction input
      | BS.null input = CommitHistoryActions info transaction
      | otherwise = case BS.elemIndex 0x0a input of
          Nothing -> visit info transaction (BS.length input) input BS.empty
          Just end -> visit info transaction end input (BSU.unsafeDrop (end + 1) input)

    visit info transaction end input next
      -- Data commits mostly contain add rows. Test their literal prefix
      -- before allocating any line or key slices.
      | BS.isPrefixOf "{\"add\"" input = go info transaction next
      | readInfo || readTransaction =
          case decodeActionLine (Just Commit) line of
            Right (ActionCommitInfo value) -> go (Just value) transaction next
            Right (ActionTxn value) | wantsTransaction -> go info (Just value) next
            _ -> go info transaction next
      | otherwise = go info transaction next
      where
        line = BSU.unsafeTake end input
        candidate = BS.dropWhile isJsonWhitespace (BS.drop 1 (BS.dropWhile isJsonWhitespace line))
        key = BS.takeWhile (/= 0x22) (BS.drop 1 candidate)
        wantsTransaction = includeTransaction && isNothing transaction
        couldContainActions = not (BS.isPrefixOf "\"" candidate) || BS.elem 0x5c key
        readInfo = key == "commitInfo" || couldContainActions
        readTransaction = wantsTransaction && (key == "txn" || couldContainActions)
{-# INLINE parseHistoryActions #-}


{- | Parse a commit for snapshot replay. Unknown action names are retained
as 'ActionOther'. Malformed lines or known actions fail the entire commit.
-}
parseLogFileChecked :: BL.ByteString -> Either String [DeltaAction]
parseLogFileChecked input = reverse <$> foldLogFileChecked (flip (:)) [] input


-- | Decode and fold a commit without retaining its action list.
foldLogFileChecked :: (a -> DeltaAction -> a) -> a -> BL.ByteString -> Either String a
foldLogFileChecked step initial = foldCommitBytes step initial . BL.toStrict


-- | Fold an on-disk commit buffer one line at a time.
foldCommitBytes :: (a -> DeltaAction -> a) -> a -> BS.ByteString -> Either String a
foldCommitBytes = foldActionsWith Commit


-- | A reconciled range carries state changes but no commit provenance.
foldCompactedBytes :: (a -> DeltaAction -> a) -> a -> BS.ByteString -> Either String a
foldCompactedBytes = foldActionsWith Compacted


-- | Decode V2 checkpoint JSON, including checkpoint-only actions.
foldCheckpointJson :: (a -> DeltaAction -> a) -> a -> BS.ByteString -> Either String a
foldCheckpointJson = foldActionsWith Checkpoint


-- Recover a zero-byte commit or a final JSON action cut short by EOF.
-- Action validation and non-EOF syntax errors remain fatal.
truncatedCommitPrefix :: BS.ByteString -> Maybe BS.ByteString
truncatedCommitPrefix bytes
  | BS.null bytes = Just BS.empty
  | BS.isPrefixOf "{" (BS.dropWhile isJsonWhitespace partial)
  , Left err <- decodeActionLine (Just Commit) partial
  , "Unexpected end-of-input" `isPrefixOf` err = Just complete
  | otherwise = Nothing
  where
    (complete, partial) = BSC.breakEnd (== '\n') bytes


data LogKind = Commit | Compacted | Checkpoint
  deriving stock (Eq)


foldActionsWith :: LogKind -> (a -> DeltaAction -> a) -> a -> BS.ByteString -> Either String a
foldActionsWith kind step initial = go 1 initial
  where
    go !n !acc input
      | BS.null input = Right acc
      | otherwise =
          let (line, suffix) = BSC.break (== '\n') input
              !rest = BS.drop 1 suffix
              !next = n + (1 :: Int)
          in if BS.all isJsonWhitespace line
               then go next acc rest
               else case decodeActionLine (Just kind) line of
                 Left err -> Left ("invalid Delta action at line " ++ show n ++ ": " ++ err)
                 Right action -> go next (step acc action) rest


-- An absent log kind lets parseLogLine inspect any single action.
decodeActionLine :: Maybe LogKind -> BS.ByteString -> Either String DeltaAction
decodeActionLine kind line = case bsToTokens line of
  TkRecordOpen (TkPair "add" (TkRecordOpen fields))
    | Just add <- decodeAddTokens line fields -> Right (ActionAdd add)
  tokens
    | not (jsonExponentsFit line) -> Left jsonExponentError
    | otherwise -> decodeTokens tokens
  where
    decodeTokens = \case
      -- Check the outer record before converting it to a map, where
      -- repeating the same action name would collapse to a single field.
      TkRecordOpen (TkPair name tokens) -> do
        (value, fields) <- checkedJsonValue tokens
        case fields of
          TkRecordEnd rest | BS.all isJsonWhitespace rest -> parseEither (parseOne name) value
          TkRecordErr err -> Left err
          _ -> Left "expected exactly one action"
      TkRecordOpen (TkRecordErr err) -> Left err
      TkErr err -> Left err
      _ -> Left "expected exactly one action"

    parseOne name value = case lookup name logActionParsers of
      Just parser -> do
        action <- parser value
        case action of
          ActionCheckpointMetadata _ | disallows Checkpoint -> fail checkpointOnly
          ActionSidecar _ | disallows Checkpoint -> fail checkpointOnly
          ActionCommitInfo _ | disallows Commit -> fail "commitInfo is invalid in a checkpoint or compaction"
          ActionCdc _ | disallows Commit -> fail "cdc is invalid in a checkpoint or compaction"
          _ -> pure action
      Nothing -> pure (ActionOther (AK.toText name))
    checkpointOnly = "checkpoint actions are invalid in a JSON commit"
    disallows allowed = maybe False (/= allowed) kind
{-# INLINE decodeActionLine #-}


-- Decode common add actions straight from Aeson's validated token stream.
-- Fall back for unusual representations and errors. The seen mask checks
-- all fields accepted here, including those whose value can be null.
-- Known scalar fields here accept integer tokens only. Partition values
-- and tags contain only strings or nulls. Check exponent spellings when
-- retaining arbitrary JSON, such as a deletion-vector descriptor.
decodeAddTokens :: BS.ByteString -> TkRecord (TkRecord BS.ByteString String) String -> Maybe AddAction
decodeAddTokens line record =
  decodeAddRecord record finish
  where
    finish seen path size mtime changed stats partitions tags dv rowId commitVersion = \case
      TkRecordEnd remainder
        | seen .&. 15 == 15 && BS.all isJsonWhitespace remainder
        , seen .&. addExponentCheck == 0 || jsonExponentsFit line ->
            Just (AddAction path size mtime changed stats Nothing partitions tags dv rowId commitVersion)
      _ -> Nothing
-- Keep the commit fold from inlining the field loop for each caller.
{-# NOINLINE decodeAddTokens #-}


addExponentCheck :: Int
addExponentCheck = 1024


-- Keep the continuation at the end of the field loop so the commit
-- reader can check its enclosing object before allocating an AddAction.
decodeAddRecord
  :: TkRecord k String
  -> (Int -> Text -> Word64 -> Int64 -> Bool -> Maybe Text -> Map.Map Text (Maybe Text)
        -> Map.Map Text Text -> Maybe Value -> Maybe Int64 -> Maybe Int64 -> k -> Maybe a)
  -> Maybe a
decodeAddRecord input finish = fields 0 T.empty 0 0 False Nothing Map.empty Map.empty Nothing Nothing Nothing input
  where
    fields !seen !path !size !mtime !changed stats partitions tags dv rowId commitVersion record = case record of
      TkPair "path" (TkText value next)
        | unseen 1 -> fields (seen .|. 1) value size mtime changed stats partitions tags dv rowId commitVersion next
      TkPair "size" (TkNumber (NumInteger value) next)
        | unseen 2
        , value >= 0
        , value <= toInteger (maxBound :: Int64) ->
            fields (seen .|. 2) path (fromInteger value) mtime changed stats partitions tags dv rowId commitVersion next
      TkPair "modificationTime" (TkNumber (NumInteger value) next)
        | unseen 4
        , value >= toInteger (minBound :: Int64)
        , value <= toInteger (maxBound :: Int64) ->
            fields (seen .|. 4) path size (fromInteger value) changed stats partitions tags dv rowId commitVersion next
      TkPair "dataChange" (TkLit value next)
        | unseen 8
        , value /= LitNull ->
            fields (seen .|. 8) path size mtime (value == LitTrue) stats partitions tags dv rowId commitVersion next
      TkPair "stats" (TkText value next)
        | unseen 16 -> fields (seen .|. 16) path size mtime changed (Just value) partitions tags dv rowId commitVersion next
      TkPair "stats" (TkLit LitNull next)
        | unseen 16 -> fields (seen .|. 16) path size mtime changed Nothing partitions tags dv rowId commitVersion next
      TkPair "partitionValues" token | unseen 32 -> case token of
        TkRecordOpen (TkRecordEnd next) -> fields (seen .|. 32) path size mtime changed stats Map.empty tags dv rowId commitVersion next
        TkLit LitNull next -> fields (seen .|. 32) path size mtime changed stats Map.empty tags dv rowId commitVersion next
        TkRecordOpen entries -> do
          (part, next) <- textMap Map.empty entries
          fields (seen .|. 32) path size mtime changed stats (Map.map normalizePartitionValue part) tags dv rowId commitVersion next
        _ -> Nothing
      TkPair "tags" token | unseen 64 -> case token of
        TkRecordOpen (TkRecordEnd next) -> fields (seen .|. 64) path size mtime changed stats partitions Map.empty dv rowId commitVersion next
        TkLit LitNull next -> fields (seen .|. 64) path size mtime changed stats partitions Map.empty dv rowId commitVersion next
        TkRecordOpen entries -> do
          (parsed, next) <- textMap Map.empty entries
          fields (seen .|. 64) path size mtime changed stats partitions (Map.mapMaybe id parsed) dv rowId commitVersion next
        _ -> Nothing
      TkPair "deletionVector" (TkLit LitNull next)
        | unseen 128 -> fields (seen .|. 128) path size mtime changed stats partitions tags Nothing rowId commitVersion next
      TkPair "deletionVector" token | unseen 128 -> do
        (value, next) <- either (const Nothing) Just (checkedJsonValue token)
        fields (seen .|. 128 .|. addExponentCheck) path size mtime changed stats partitions tags (Just value) rowId commitVersion next
      TkPair "baseRowId" (TkNumber (NumInteger value) next)
        | unseen 256, signedLong value ->
            fields (seen .|. 256) path size mtime changed stats partitions tags dv (Just (fromInteger value)) commitVersion next
      TkPair "baseRowId" (TkLit LitNull next)
        | unseen 256 -> fields (seen .|. 256) path size mtime changed stats partitions tags dv Nothing commitVersion next
      TkPair "defaultRowCommitVersion" (TkNumber (NumInteger value) next)
        | unseen 512, signedLong value ->
            fields (seen .|. 512) path size mtime changed stats partitions tags dv rowId (Just (fromInteger value)) next
      TkPair "defaultRowCommitVersion" (TkLit LitNull next)
        | unseen 512 -> fields (seen .|. 512) path size mtime changed stats partitions tags dv rowId Nothing next
      TkRecordEnd next -> finish seen path size mtime changed stats partitions tags dv rowId commitVersion next
      _ -> Nothing
      where
        unseen mask = (seen :: Int) .&. mask == 0
    signedLong value = value >= toInteger (minBound :: Int64) && value <= toInteger (maxBound :: Int64)
    textMap !entries = \case
      TkPair key (TkText value next) -> addText entries key (Just value) next
      TkPair key (TkLit LitNull next) -> addText entries key Nothing next
      TkRecordEnd next -> Just (entries, next)
      _ -> Nothing
    addText entries key value next = do
      updated <- Map.alterF insert (AK.toText key) entries
      textMap updated next
      where
        insert (Just _) = Nothing
        insert Nothing = Just (Just value)
{-# INLINE decodeAddRecord #-}


isJsonWhitespace :: Word8 -> Bool
isJsonWhitespace b = b == 0x20 || b == 0x09 || b == 0x0d || b == 0x0a


-- Decode the file list directly. The rest of the checksum is small and
-- uses the usual checked JSON representation, including unknown fields.
data ChecksumFileList = ChecksumFileList {-# UNPACK #-} !Int ![AddAction]


decodeChecksumFields :: BS.ByteString -> Either String (AKM.KeyMap Value, V.Vector AddAction)
decodeChecksumFields bytes = do
  (fields, files) <- decodeOptionalChecksumFiles bytes
  case files of
    Just adds -> Right (fields, adds)
    Nothing -> Left "missing allFiles"


-- | Decode the checksum and distinguish an omitted live-file list from an empty one.
decodeOptionalChecksumFiles :: BS.ByteString -> Either String (AKM.KeyMap Value, Maybe (V.Vector AddAction))
decodeOptionalChecksumFiles bytes = do
  (fields, files) <- foldChecksumFiles collect (ChecksumFileList 0 []) bytes
  pure (fields, fmap toVector files)
  where
    collect (ChecksumFileList count files) add = ChecksumFileList (count + 1) (add : files)
    toVector (ChecksumFileList count reversed) = V.fromListN count (reverse reversed)


-- | Decode checksum metadata without retaining the optional live-file list.
decodeChecksumMetricsFields :: BS.ByteString -> Either String (AKM.KeyMap Value)
decodeChecksumMetricsFields = fmap fst . decodeChecksum Nothing ()


data ChecksumFileHints = ChecksumFileHints
  { hintedFileCount :: !(Maybe Int64)
  , hintedFileBytes :: !(Maybe Int64)
  , hintedDeletionVectors :: !(Maybe Int64)
  , hintedDeletedRows :: !(Maybe Int64)
  }


-- | Read the file counters from a bounded checksum prefix when it gives
-- enough information to skip full decoding. JSON fields may come in any order.
checksumFileHints :: BS.ByteString -> Maybe ChecksumFileHints
checksumFileHints bytes = case bsToTokens bytes of
  TkRecordOpen fields -> counters False empty fields
  _ -> Nothing
  where
    empty = ChecksumFileHints Nothing Nothing Nothing Nothing
    counters noDeletionVectors hints fields
      | haveRequired hints
      , haveDeletionCounts hints = Just hints
      | haveRequired hints
      , noDeletionVectors = Just hints
          { hintedDeletionVectors = Just (fromMaybe 0 (hintedDeletionVectors hints))
          , hintedDeletedRows = Just (fromMaybe 0 (hintedDeletedRows hints))
          }
      | otherwise = case fields of
          TkPair "numFiles" (TkNumber (NumInteger value) rest) ->
            counters noDeletionVectors (hints {hintedFileCount = nonnegative rest value}) rest
          TkPair "tableSizeBytes" (TkNumber (NumInteger value) rest) ->
            counters noDeletionVectors (hints {hintedFileBytes = nonnegative rest value}) rest
          TkPair "numDeletionVectorsOpt" (TkNumber (NumInteger value) rest) ->
            counters noDeletionVectors (hints {hintedDeletionVectors = nonnegative rest value}) rest
          TkPair "numDeletedRecordsOpt" (TkNumber (NumInteger value) rest) ->
            counters noDeletionVectors (hints {hintedDeletedRows = nonnegative rest value}) rest
          TkPair "protocol" tokens -> do
            (value, rest) <- either (const Nothing) Just (checkedJsonValue tokens)
            let impossible = maybe False (notElem "deletionVectors" . pReaderFeatures) (parseMaybe parseJSON value)
            counters impossible hints rest
          -- If a large field covers the rest of the prefix, check later
          -- counters from the full file. A small list can be skipped here.
          TkPair _ tokens -> do
            (_, rest) <- either (const Nothing) Just (checkedJsonValue tokens)
            counters noDeletionVectors hints rest
          TkRecordEnd _ | haveRequired hints -> Just hints
          _ -> Nothing
    haveRequired hints = not (isNothing (hintedFileCount hints) || isNothing (hintedFileBytes hints))
    haveDeletionCounts hints = not (isNothing (hintedDeletionVectors hints) || isNothing (hintedDeletedRows hints))
    -- The tokenizer may report a number ending exactly at the prefix
    -- boundary. Require evidence that its enclosing field ended.
    nonnegative (TkRecordErr _) _ = Nothing
    nonnegative _ value
      | value >= 0 && value <= toInteger (maxBound :: Int64) = Just (fromInteger value)
      | otherwise = Nothing


-- | Fold the optional checksum file list without retaining its entries.
foldChecksumFiles :: (a -> AddAction -> a) -> a -> BS.ByteString -> Either String (AKM.KeyMap Value, Maybe a)
foldChecksumFiles step = decodeChecksum (Just step)
{-# INLINE foldChecksumFiles #-}


decodeChecksum :: Maybe (a -> AddAction -> a) -> a -> BS.ByteString -> Either String (AKM.KeyMap Value, Maybe a)
decodeChecksum collect initial bytes
  | not (jsonExponentsFit bytes) = Left jsonExponentError
  | otherwise = case bsToTokens bytes of
      TkRecordOpen fields -> object AKM.empty False Nothing fields
      TkErr err -> Left err
      _ -> Left "expected a version checksum object"
  where
    object !known !seenFiles !adds = \case
      TkPair "allFiles" tokens
        | seenFiles -> Left "duplicate JSON key \"allFiles\""
        | otherwise -> case tokens of
            TkArrayOpen items -> case collect of
              Just step -> readAdds step known initial items
              Nothing -> skipArray items >>= object known True Nothing
            TkErr err -> Left err
            _ -> Left "allFiles must be an array"
      TkPair key tokens
        | AKM.member key known -> Left ("duplicate JSON key " ++ show key)
        | otherwise -> do
            (value, remaining) <- checkedJsonValue tokens
            object (AKM.insert key value known) seenFiles adds remaining
      TkRecordEnd remaining
        | BS.all isJsonWhitespace remaining ->
            Right (known, adds)
        | otherwise -> Left "trailing bytes after JSON value"
      TkRecordErr err -> Left err

    readAdds step !known !files = \case
      TkItem tokens ->
        case tokens of
          TkRecordOpen fields
            | Just next <- decodeAddRecord fields finish ->
                next
          _ -> do
            (value, remaining) <- checkedJsonValue tokens
            add <- parseEither parseJSON value
            readAdds step known (step files add) remaining
      TkArrayEnd remaining -> object known True (Just files) remaining
      TkArrayErr err -> Left err
      where
        finish seen path size mtime changed stats partitions tags dv rowId commitVersion remaining
          | seen .&. 15 == 15 =
              let !add = AddAction path size mtime changed stats Nothing partitions tags dv rowId commitVersion
              in Just (readAdds step known (step files add) remaining)
          | otherwise = Nothing

    -- The metrics-only decoder validates the JSON without retaining file actions.
    skipArray = \case
      TkItem tokens -> case tokens of
        TkRecordOpen fields
          | Just remaining <- decodeAddRecord fields (\_ _ _ _ _ _ _ _ _ _ _ next -> Just next) ->
              skipArray remaining
        _ -> do
          (_, remaining) <- checkedJsonValue tokens
          skipArray remaining
      TkArrayEnd remaining -> Right remaining
      TkArrayErr err -> Left err
{-# INLINE decodeChecksum #-}


decodeDeltaJson :: FromJSON a => BS.ByteString -> Either String a
decodeDeltaJson bytes
  | not (jsonExponentsFit bytes) = Left jsonExponentError
  | otherwise = do
      (value, remaining) <- checkedJsonValue (bsToTokens bytes)
      if BS.all isJsonWhitespace remaining
        then parseEither parseJSON value
        else Left "trailing bytes after JSON value"


-- Use Aeson's tokenization, but reject duplicates before a key map can
-- discard them. Escaped keys already have their decoded spelling here.
checkedJsonValue :: Tokens k String -> Either String (Value, k)
checkedJsonValue tokens = case jsonValue tokens of
  (# (# value, next #) | #) -> Right (value, next)
  (# | err #) -> Left err
{-# INLINE checkedJsonValue #-}


jsonValue :: Tokens k String -> (# (# Value, k #) | String #)
jsonValue = \case
  TkLit LitNull next -> (# (# Null, next #) | #)
  TkLit LitTrue next -> (# (# Bool True, next #) | #)
  TkLit LitFalse next -> (# (# Bool False, next #) | #)
  TkText value next -> (# (# String value, next #) | #)
  TkNumber number next -> let !value = Number (numeric number) in (# (# value, next #) | #)
  TkRecordOpen fields -> object AKM.empty fields
  TkArrayOpen items -> array 0 [] items
  TkErr err -> (# | err #)
  where
    numeric (NumInteger n) = fromInteger n
    numeric (NumDecimal n) = n
    numeric (NumScientific n) = n
    object !fields = \case
      TkPair key tokens -> case jsonValue tokens of
        (# (# value, rest #) | #) -> case AKM.alterF (insert value) key fields of
          Just updated -> object updated rest
          Nothing -> (# | "duplicate JSON key " ++ show key #)
        (# | err #) -> (# | err #)
      TkRecordEnd next -> (# (# Object fields, next #) | #)
      TkRecordErr err -> (# | err #)
    insert value = \case
      Just _ -> Nothing
      Nothing -> Just (Just value)
    array !count !values = \case
      TkItem tokens -> case jsonValue tokens of
        (# (# value, rest #) | #) -> array (count + 1) (value : values) rest
        (# | err #) -> (# | err #)
      TkArrayEnd next -> (# (# Array (V.fromListN count (reverse values)), next #) | #)
      TkArrayErr err -> (# | err #)


jsonExponentError :: String
jsonExponentError = "JSON number exponent exceeds the representable range"


-- Aeson narrows a numeric literal's exponent to Int before building
-- Scientific. Leave room for the fractional digits and coefficient when
-- adjusting that exponent. Aeson still validates the JSON grammar.
jsonExponentsFit :: BS.ByteString -> Bool
jsonExponentsFit bytes = outside 0
  where
    end = BS.length bytes
    byte = BSU.unsafeIndex bytes
    limit = maxBound - end
    (tens, units) = quotRem limit 10
    outside !i
      | i >= end = True
      | otherwise = case byte i of
          0x22 -> quoted (i + 1)
          0x65 -> sign (i + 1)
          0x45 -> sign (i + 1)
          _ -> outside (i + 1)
    quoted !start = case BS.elemIndex 0x22 (BS.drop start bytes) of
      Nothing -> True
      Just offset ->
        let close = start + offset
            after = close + 1
        in if escaped start False (close - 1) then quoted after else outside after
    escaped !start !oddSlashes !i
      | i >= start && byte i == 0x5c = escaped start (not oddSlashes) (i - 1)
      | otherwise = oddSlashes
    sign !i
      | i >= end = True
      | c == 0x2b || c == 0x2d = digits 0 (i + 1)
      | otherwise = digits 0 i
      where
        c = byte i
    digits !value !i
      | i >= end = True
      | digit > 9 = outside i
      | value < tens || (value == tens && digit <= units) = digits (value * 10 + digit) (i + 1)
      | otherwise = False
      where
        digit = fromIntegral (byte i - 0x30)
{-# NOINLINE jsonExponentsFit #-}


-- ============================================================
-- Active file derivation (legacy helper)
-- ============================================================

{- | Apply every action in order to derive the table's current
active file set. Equivalent to
@Map.elems . tsFiles . snapshotFromActions@; kept for
compatibility with the original skeleton API.
-}
activeFiles :: [DeltaAction] -> [AddAction]
activeFiles = Map.elems . tsFiles . snapshotFromActions
