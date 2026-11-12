{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

Out of scope (still): column mapping, deletion vectors,
generated columns, time travel by timestamp, V2 checkpoint
format, log compaction.
-}
module Delta.Log (
  -- * Actions
  DeltaAction (..),
  AddAction (..),
  RemoveAction (..),
  MetaDataAction (..),
  ProtocolAction (..),
  CommitInfoAction (..),
  TxnAction (..),
  CdcAction (..),
  AddStats (..),

  -- * Delta schema (decoded from @metaData.schemaString@)
  DeltaSchema (..),
  DeltaField (..),
  DeltaType (..),
  parseDeltaSchema,

  -- * Stats helper
  decodeAddStats,

  -- * Table snapshot
  TableSnapshot (..),
  emptySnapshot,
  applyAction,
  snapshotFromActions,

  -- * Last-checkpoint pointer
  LastCheckpoint (..),
  parseLastCheckpoint,

  -- * Log parsing
  parseLogFile,
  parseLogLine,

  -- * Active-file derivation (compatibility helper)
  activeFiles,
) where

import Data.Aeson (
  FromJSON (..),
  Result (..),
  Value (..),
  decode,
  decodeStrict',
  fromJSON,
  withObject,
  (.:),
  (.:?),
 )
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HM
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word64)


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
  | ActionCdc !CdcAction
  | -- | Tagged with the variant name for forward compat.
    ActionOther !Text
  deriving (Show, Eq)


{- | The @add@ action: a Parquet (or other-format) data file is
now part of the table.
-}
data AddAction = AddAction
  { addPath :: !Text
  , addSize :: !Word64
  , addModificationTime :: !Word64
  , addDataChange :: !Bool
  , addStats :: !(Maybe Text)
  {- ^ JSON-encoded statistics (numRecords, minValues,
  maxValues, nullCount). See 'parseAddStats'.
  -}
  , addPartitionValues :: !(Map.Map Text (Maybe Text))
  , addTags :: !(Map.Map Text Text)
  , addDeletionVector :: !(Maybe Value)
  {- ^ Raw deletion-vector descriptor object (V2). Kept as the
  aeson 'Value' until a typed decoder lands.
  -}
  }
  deriving (Show, Eq)


{- | The @remove@ action: a previously-added file is no longer
in the table. Combined with subsequent @add@s, this is how
Delta encodes overwrites.
-}
data RemoveAction = RemoveAction
  { removePath :: !Text
  , removeDeletionTimestamp :: !(Maybe Word64)
  , removeDataChange :: !Bool
  , removeExtendedFileMetadata :: !(Maybe Bool)
  , removeSize :: !(Maybe Word64)
  , removePartitionValues :: !(Map.Map Text (Maybe Text))
  , removeDeletionVector :: !(Maybe Value)
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
  , mdCreatedTime :: !(Maybe Word64)
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
  { ciTimestamp :: !(Maybe Word64)
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
      <*> o .: "size"
      <*> o .: "modificationTime"
      <*> o .: "dataChange"
      <*> o .:? "stats"
      <*> (fromMaybe Map.empty <$> o .:? "partitionValues")
      <*> (fromMaybe Map.empty <$> o .:? "tags")
      <*> o .:? "deletionVector"


instance FromJSON RemoveAction where
  parseJSON = withObject "RemoveAction" $ \o ->
    RemoveAction
      <$> o .: "path"
      <*> o .:? "deletionTimestamp"
      <*> (fromMaybe True <$> o .:? "dataChange")
      <*> o .:? "extendedFileMetadata"
      <*> o .:? "size"
      <*> (fromMaybe Map.empty <$> o .:? "partitionValues")
      <*> o .:? "deletionVector"


instance FromJSON MetaDataAction where
  parseJSON = withObject "MetaDataAction" $ \o -> do
    fmt <- o .:? "format"
    let fmtPair = case fmt of
          Just (Object fo) -> do
            prov <- AKM.lookup "provider" fo
            opts <- AKM.lookup "options" fo
            case (prov, opts) of
              (String p, Object oo) -> Just (p, valueObjectToMap oo)
              (String p, _) -> Just (p, Map.empty)
              _ -> Nothing
          _ -> Nothing
    MetaDataAction
      <$> o .: "id"
      <*> o .:? "name"
      <*> o .:? "description"
      <*> pure fmtPair
      <*> o .: "schemaString"
      <*> (fromMaybe [] <$> o .:? "partitionColumns")
      <*> (fromMaybe Map.empty <$> o .:? "configuration")
      <*> o .:? "createdTime"


valueObjectToMap :: AKM.KeyMap Value -> Map.Map Text Text
valueObjectToMap = AKM.foldrWithKey step Map.empty
  where
    step k (String t) acc = Map.insert (AK.toText k) t acc
    step _ _ acc = acc


instance FromJSON ProtocolAction where
  parseJSON = withObject "ProtocolAction" $ \o ->
    ProtocolAction
      <$> o .: "minReaderVersion"
      <*> o .: "minWriterVersion"
      <*> (fromMaybe [] <$> o .:? "readerFeatures")
      <*> (fromMaybe [] <$> o .:? "writerFeatures")


instance FromJSON CommitInfoAction where
  parseJSON = withObject "CommitInfoAction" $ \o -> do
    t <- o .:? "timestamp"
    op <- o .:? "operation"
    opPar <- fromMaybe HM.empty <$> o .:? "operationParameters"
    iso <- o .:? "isolationLevel"
    blind <- o .:? "isBlindAppend"
    txnVer <- o .:? "txnVersion"
    let known =
          [ "timestamp"
          , "operation"
          , "operationParameters"
          , "isolationLevel"
          , "isBlindAppend"
          , "txnVersion"
          ]
        extra =
          AKM.foldrWithKey
            ( \k v acc ->
                if AK.toText k `elem` known
                  then acc
                  else HM.insert (AK.toText k) v acc
            )
            HM.empty
            o
    pure
      CommitInfoAction
        { ciTimestamp = t
        , ciOperation = op
        , ciOperationParameters = opPar
        , ciIsolationLevel = iso
        , ciIsBlindAppend = blind
        , ciTxnVersion = txnVer
        , ciExtra = extra
        }


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
      <*> o .: "size"
      <*> (fromMaybe False <$> o .:? "dataChange")
      <*> (fromMaybe Map.empty <$> o .:? "partitionValues")
      <*> (fromMaybe Map.empty <$> o .:? "tags")


-- ============================================================
-- Stats parsing (the @stats@ string of an AddAction)
-- ============================================================

{- | Decoded @stats@ JSON for an 'AddAction'. The Delta spec
requires writers to emit at least 'addNumRecords'; the rest
are best-effort and may be missing.
-}
data AddStats = AddStats
  { asNumRecords :: !(Maybe Word64)
  , asMinValues :: !(HM.HashMap Text Value)
  , asMaxValues :: !(HM.HashMap Text Value)
  , asNullCount :: !(HM.HashMap Text Word64)
  , asTightBounds :: !(Maybe Bool)
  }
  deriving (Show, Eq)


instance FromJSON AddStats where
  parseJSON = withObject "AddStats" $ \o -> do
    n <- o .:? "numRecords"
    mn <- fromMaybe HM.empty <$> o .:? "minValues"
    mx <- fromMaybe HM.empty <$> o .:? "maxValues"
    ncR <- fromMaybe HM.empty <$> o .:? "nullCount"
    tb <- o .:? "tightBounds"
    let nc = HM.mapMaybe asWord64 ncR
    pure
      AddStats
        { asNumRecords = n
        , asMinValues = mn
        , asMaxValues = mx
        , asNullCount = nc
        , asTightBounds = tb
        }
    where
      asWord64 :: Value -> Maybe Word64
      asWord64 (Number s) = toBoundedInteger s
      asWord64 _ = Nothing


{- | Decode the @stats@ field of an 'AddAction' if it's
present. Returns:

  * 'Nothing'        — the writer did not emit @stats@ for this
                       file (legal per the Delta spec).
  * 'Just (Right s)' — typed 'AddStats' decoded successfully.
  * 'Just (Left e)'  — @stats@ was present but malformed; the
                       error string is the aeson failure
                       message.

Saves callers from manually plumbing through
@decode . encodeUtf8 . addStats@.
-}
decodeAddStats :: AddAction -> Maybe (Either String AddStats)
decodeAddStats a = case addStats a of
  Nothing -> Nothing
  Just t -> case decodeStrict' (TE.encodeUtf8 t) :: Maybe Value of
    Nothing -> Just (Left "Delta.Log.decodeAddStats: malformed JSON")
    Just v -> case fromJSON v of
      Success s -> Just (Right s)
      Error e -> Just (Left ("Delta.Log.decodeAddStats: " ++ e))


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
  | -- | precision, scale
    DTDecimal !Int !Int
  | -- | element type + containsNull
    DTArray !DeltaType !Bool
  | -- | key, value, valueContainsNull
    DTMap !DeltaType !DeltaType !Bool
  | DTStruct ![DeltaField]
  | DTUnknown !Text
  deriving (Show, Eq)


-- | Decode a raw @schemaString@ into a typed 'DeltaSchema'.
parseDeltaSchema :: Text -> Either String DeltaSchema
parseDeltaSchema t =
  case decodeStrict' (TE.encodeUtf8 t) :: Maybe Value of
    Nothing -> Left "Delta.Log.parseDeltaSchema: malformed JSON"
    Just v -> case parseStructTop v of
      Just s -> Right s
      Nothing -> Left "Delta.Log.parseDeltaSchema: not a struct schema"
  where
    parseStructTop (Object o) = case AKM.lookup "type" o of
      Just (String "struct") ->
        case AKM.lookup "fields" o of
          Just (Array fs) -> Just (DeltaSchema (V.toList (V.mapMaybe parseField fs)))
          _ -> Just (DeltaSchema [])
      _ -> Nothing
    parseStructTop _ = Nothing

    parseField :: Value -> Maybe DeltaField
    parseField (Object o) = do
      String n <- AKM.lookup "name" o
      tyV <- AKM.lookup "type" o
      let nullable = case AKM.lookup "nullable" o of
            Just (Bool b) -> b
            _ -> True
      let meta = case AKM.lookup "metadata" o of
            Just (Object m) ->
              AKM.foldrWithKey
                (\k v acc -> HM.insert (AK.toText k) v acc)
                HM.empty
                m
            _ -> HM.empty
      Just
        DeltaField
          { dfName = n
          , dfType = parseDeltaType tyV
          , dfNullable = nullable
          , dfMetadata = meta
          }
    parseField _ = Nothing


parseDeltaType :: Value -> DeltaType
parseDeltaType (String s) = case s of
  "string" -> DTString
  "long" -> DTLong
  "integer" -> DTInteger
  "short" -> DTShort
  "byte" -> DTByte
  "float" -> DTFloat
  "double" -> DTDouble
  "boolean" -> DTBoolean
  "binary" -> DTBinary
  "date" -> DTDate
  "timestamp" -> DTTimestamp
  "timestamp_ntz" -> DTTimestampNtz
  other -> case parseDecimal other of
    Just (p, sc) -> DTDecimal p sc
    Nothing -> DTUnknown other
parseDeltaType (Object o) = case AKM.lookup "type" o of
  Just (String "struct") ->
    case AKM.lookup "fields" o of
      Just (Array fs) -> DTStruct (V.toList (V.mapMaybe parseFieldHelper fs))
      _ -> DTStruct []
  Just (String "array") ->
    let el = maybe (DTUnknown "<missing element>") parseDeltaType (AKM.lookup "elementType" o)
        cn = case AKM.lookup "containsNull" o of
          Just (Bool b) -> b
          _ -> True
    in DTArray el cn
  Just (String "map") ->
    let k = maybe (DTUnknown "<missing key>") parseDeltaType (AKM.lookup "keyType" o)
        v = maybe (DTUnknown "<missing value>") parseDeltaType (AKM.lookup "valueType" o)
        cn = case AKM.lookup "valueContainsNull" o of
          Just (Bool b) -> b
          _ -> True
    in DTMap k v cn
  Just (String other) -> DTUnknown other
  _ -> DTUnknown "<unknown shape>"
parseDeltaType _ = DTUnknown "<non-string non-object type>"


parseFieldHelper :: Value -> Maybe DeltaField
parseFieldHelper (Object o) = do
  String n <- AKM.lookup "name" o
  tyV <- AKM.lookup "type" o
  let nullable = case AKM.lookup "nullable" o of
        Just (Bool b) -> b
        _ -> True
  Just
    DeltaField
      { dfName = n
      , dfType = parseDeltaType tyV
      , dfNullable = nullable
      , dfMetadata = HM.empty
      }
parseFieldHelper _ = Nothing


-- @"decimal(10,2)"@ → @Just (10, 2)@. Used by 'parseDeltaType'.
parseDecimal :: Text -> Maybe (Int, Int)
parseDecimal t = do
  rest1 <- T.stripPrefix "decimal(" t
  rest2 <- T.stripSuffix ")" rest1
  case T.splitOn "," rest2 of
    [a, b] -> do
      pa <- readInt (T.strip a)
      pb <- readInt (T.strip b)
      Just (pa, pb)
    _ -> Nothing
  where
    readInt s = case reads (T.unpack s) of
      [(n, "")] -> Just n
      _ -> Nothing


-- ============================================================
-- Snapshot
-- ============================================================

{- | Active state of the table at a particular log version.
Combines protocol, metadata, the live file set, and the
last recorded transaction version for each app id.
-}
data TableSnapshot = TableSnapshot
  { tsProtocol :: !(Maybe ProtocolAction)
  , tsMetaData :: !(Maybe MetaDataAction)
  , tsFiles :: !(Map.Map Text AddAction)
  -- ^ Live data files keyed by 'addPath'.
  , tsAppIds :: !(Map.Map Text Int64)
  -- ^ Last version recorded per @txn.appId@, in commit order.
  , tsLastCommit :: !(Maybe CommitInfoAction)
  -- ^ Most recently observed @commitInfo@.
  }
  deriving (Show, Eq)


emptySnapshot :: TableSnapshot
emptySnapshot =
  TableSnapshot
    { tsProtocol = Nothing
    , tsMetaData = Nothing
    , tsFiles = Map.empty
    , tsAppIds = Map.empty
    , tsLastCommit = Nothing
    }


{- | Apply one action to the snapshot. Strict in the snapshot
so a long replay doesn't build a thunk chain.
-}
applyAction :: TableSnapshot -> DeltaAction -> TableSnapshot
applyAction !s = \case
  ActionAdd a -> s {tsFiles = Map.insert (addPath a) a (tsFiles s)}
  ActionRemove r -> s {tsFiles = Map.update (removeMatching r) (removePath r) (tsFiles s)}
  ActionMetaData md -> s {tsMetaData = Just md}
  ActionProtocol p -> s {tsProtocol = Just p}
  ActionCommitInfo c -> s {tsLastCommit = Just c}
  ActionTxn t -> s {tsAppIds = Map.insert (txnAppId t) (txnVersion t) (tsAppIds s)}
  ActionCdc _ -> s -- @cdc@ does not affect the live file set
  ActionOther _ -> s
  where
    -- A tombstone for an earlier deletion vector can appear after the
    -- replacement add, either in a commit or in a checkpoint.
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
snapshotFromActions = foldl applyAction emptySnapshot


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
      <$> o .: "version"
      <*> o .: "size"
      <*> o .:? "parts"
      <*> o .:? "sizeInBytes"
      <*> o .:? "numOfAddFiles"


{- | Parse @_last_checkpoint@ JSON. Returns 'Nothing' on
parser failure (the file is allowed to be absent or
malformed; readers fall back to a full log walk).
-}
parseLastCheckpoint :: BS.ByteString -> Maybe LastCheckpoint
parseLastCheckpoint = decodeStrict'


-- ============================================================
-- Log line / file parsers
-- ============================================================

-- | Parse one NDJSON line of a Delta log file.
parseLogLine :: BL.ByteString -> Maybe DeltaAction
parseLogLine line = do
  v <- decode line
  case v of
    Object o ->
      let pluck :: FromJSON a => Text -> Maybe a
          pluck k = do
            inner <- AKM.lookup (AK.fromText k) o
            case fromJSON inner of
              Success a -> Just a
              Error _ -> Nothing
      in case Nothing of
           _
             | Just a <- pluck "add" -> Just (ActionAdd a)
             | Just r <- pluck "remove" -> Just (ActionRemove r)
             | Just md <- pluck "metaData" -> Just (ActionMetaData md)
             | Just p <- pluck "protocol" -> Just (ActionProtocol p)
             | Just ci <- pluck "commitInfo" -> Just (ActionCommitInfo ci)
             | Just tx <- pluck "txn" -> Just (ActionTxn tx)
             | Just cd <- pluck "cdc" -> Just (ActionCdc cd)
             | otherwise -> Just (ActionOther (firstKey o))
    _ -> Nothing
  where
    firstKey o = case AKM.toList o of
      ((k, _) : _) -> AK.toText k
      [] -> T.pack "<empty>"


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
