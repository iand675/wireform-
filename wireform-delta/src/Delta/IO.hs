{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | High-level IO entry points for Delta Lake tables.

This is the surface most callers want: hand it a path on disk
and it gives back a typed snapshot of the table at the latest
version. The format-level decoders in "Delta.Log" stay
pure-data so they can be reused by tests / fixtures, the
protocol-rewriter, etc.
-}
module Delta.IO (
  -- * Discovery
  findLastCheckpoint,
  findCommits,
  findCheckpointParquet,
  listLogEntries,

  -- * High-level opener
  DeltaTable (..),
  openDeltaTable,
  openDeltaTableAt,
  openDeltaTableAtTimestamp,
  validateVersionChecksum,
  streamDeltaCommits,
  DeltaCommit (..),
  deltaCommitMillis,

  -- * Snapshot helpers
  activeFilePaths,
  dtActiveFiles,
  partitionedActiveFiles,
  validateLocalSnapshot,
  supportsDeletionVectors,

  -- * History
  HistoryEntry (..),
  historyEntries,

  -- * Re-exports
  module Delta.Log,
) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, throwIO, try)
import Control.Monad (filterM, foldM, unless, when)
import Control.Monad.ST (runST)
import Columnar.Stream (IterIO (..), IterIOStep (..))
import Data.Aeson (Key, Object, Value (String), withObject, (.:), (.:!), (.:?))
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isHexDigit)
import Data.HashMap.Strict qualified as HM
import Data.Int (Int64)
import Data.List (groupBy, sort, sortBy, stripPrefix)
import Data.Map.Internal qualified as MapInternal
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word (Word64)
import Delta.Checkpoint qualified as Checkpoint
import Delta.DeletionVector (DeletionVectorIdentity, deletionVectorIdentity, descriptorCardinality, sameDeletionVectorDescriptor)
import Delta.Log
import Delta.Path (dataFilePath, filePathKey)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.FilePath (replaceExtension, splitExtension, takeDirectory, takeExtension, takeFileName, (</>))
import System.IO (IOMode (ReadMode), hFileSize, hIsEOF, withBinaryFile)
import System.IO.Error (isDoesNotExistError)


-- ============================================================
-- Discovery
-- ============================================================

{- | Read the @_delta_log/_last_checkpoint@ pointer file, if
present. Returns 'Nothing' both when the file is absent (a
young table that hasn't been checkpointed) and when its JSON
is malformed (a partially-written checkpoint). Either way the
caller should fall back to a full log walk.
-}
findLastCheckpoint :: FilePath -> IO (Maybe LastCheckpoint)
findLastCheckpoint tableRoot = fmap fst <$> findLastCheckpointWithPath tableRoot


findLastCheckpointWithPath :: FilePath -> IO (Maybe (LastCheckpoint, Maybe Text))
findLastCheckpointWithPath tableRoot = do
  let p = tableRoot </> "_delta_log" </> "_last_checkpoint"
  ok <- doesFileExist p
  if not ok
    then pure Nothing
    else do
      res <- try @IOException (BS.readFile p)
      case res of
        Left _ -> pure Nothing
        Right bs -> pure (parseLastCheckpointWithPath bs)


{- | List every numbered commit JSON file under
@<root>/_delta_log/@, in chronological order. Each tuple is
@(version, absolute path)@. Files whose name is not a 20-digit
numeric stem followed by @.json@ are skipped (the
@_last_checkpoint@ pointer, partial / temp files, sidecar
directories, …).
-}
findCommits :: FilePath -> IO [(Word64, FilePath)]
findCommits tableRoot = do
  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok
    then pure []
    else do
      entries <- listDirectory logDir
      filterM (doesFileExist . snd) (commitsFromNames logDir entries)


commitsFromNames :: FilePath -> [FilePath] -> [(Word64, FilePath)]
commitsFromNames logDir = sort . mapMaybe commit
  where
    commit name = case numberedLogName 20 name of
      Just (version, ".json") -> Just (version, logDir </> name)
      _ -> Nothing


data LogFiles = LogFiles
  ![(Word64, FilePath)]
  ![Compaction]
  ![CheckpointFiles]
  ![CheckpointPart]
  ![Word64]


data Compaction = Compaction !Word64 !Word64 !FilePath


logFilesFromNames :: FilePath -> [FilePath] -> LogFiles
logFilesFromNames logDir = foldl' add (LogFiles [] [] [] [] [])
  where
    add files@(LogFiles commits compacted singles parts checksums) name = case numberedLogName 20 name of
      Just (version, ".json") ->
        LogFiles ((version, logDir </> name) : commits) compacted singles parts checksums
      Just (version, ".crc") -> LogFiles commits compacted singles parts (version : checksums)
      Just (version, suffix) -> addOther version suffix name files
      Nothing -> files

    addOther version suffix name files@(LogFiles commits compacted singles parts checksums) =
      case namedCheckpoint version suffix name of
        Just (NamedSingle single) -> LogFiles commits compacted (single : singles) parts checksums
        Just (NamedPart part) -> LogFiles commits compacted singles (part : parts) checksums
        Nothing
          | Just end <- compactionEnd version suffix ->
              LogFiles commits (Compaction version end (logDir </> name) : compacted) singles parts checksums
          | otherwise -> files


compactionEnd :: Word64 -> String -> Maybe Word64
compactionEnd start ('.' : rest) = case numberedLogName 20 rest of
  Just (end, ".compacted.json") | start < end -> Just end
  _ -> Nothing
compactionEnd _ _ = Nothing


parsePaddedVersion :: Int -> String -> Maybe Word64
parsePaddedVersion width stem = case numberedLogName width stem of
  Just (version, []) -> Just version
  _ -> Nothing


numberedLogName :: Int -> String -> Maybe (Word64, String)
numberedLogName width name
  | width > 0 = go width 0 name
  | otherwise = Nothing
  where
    (cutoff, lastDigit) = fromIntegral (maxBound :: Int64) `quotRem` 10
    go 0 !n suffix = Just (n, suffix)
    go k !n (c : rest)
      | k > 0 && c >= '0' && c <= '9'
      , let digit = fromIntegral (fromEnum c - fromEnum '0')
      , n < cutoff || (n == cutoff && digit <= lastDigit) =
          go (k - 1) (n * 10 + digit) rest
    go _ _ _ = Nothing


-- | List and classify the entries in @<root>/_delta_log/@.
listLogEntries :: FilePath -> IO [(FilePath, String)]
listLogEntries tableRoot = do
  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok
    then pure []
    else do
      entries <- listDirectory logDir
      pure $ map (\e -> (e, classify e)) (sort entries)
  where
    classify "_last_checkpoint" = "last_checkpoint"
    classify name = case numberedLogName 20 name of
      Just (_, ".json") -> "commit"
      Just (version, suffix)
        | isJust (namedCheckpoint version suffix name) -> "checkpoint"
        | isJust (compactionEnd version suffix) -> "compaction"
      _ -> "unknown"


-- ============================================================
-- Opener
-- ============================================================

-- | A Delta Lake table opened from disk.
data DeltaTable = DeltaTable
  { dtRoot :: !FilePath
  -- ^ Filesystem path to the table directory.
  , dtLastCheckpoint :: !(Maybe LastCheckpoint)
  -- ^ The @_last_checkpoint@ pointer, if present.
  , dtCheckpointAvailable :: !(Maybe Word64)
  {- ^ Version of the newest checkpoint candidate found on disk.
  May differ from
  'lcVersion' (e.g. when the pointer file was deleted) and
  equal 'Nothing' even when 'dtLastCheckpoint' is 'Just'
  (the parquet file was vacuumed but the pointer wasn't
  updated).
  -}
  , dtCommits :: ![(Word64, FilePath)]
  {- ^ Numbered @NNNN.json@ commit candidates through the snapshot
  version in chronological order. Includes names older than the
  checkpoint for use by 'historyEntries'.
  -}
  , dtSnapshot :: !TableSnapshot
  -- ^ Folded snapshot at the latest version.
  , dtVersion :: !(Maybe Word64)
  {- ^ Version of the reconstructed snapshot. The table opener always
  supplies a committed version.
  -}
  }
  deriving (Show, Eq)


{- | Open a Delta table from a directory.

Start from the newest readable complete checkpoint, or
from version 0 if none can be read. Numbered commits and compacted
ranges must cover every version through the target.
Returns @Left@ if the available history cannot reconstruct it.
-}
openDeltaTable :: FilePath -> IO (Either String DeltaTable)
openDeltaTable = openTable LatestSnapshot


{- | Open the snapshot at a committed version. Only checkpoints at or
before the requested version can seed replay.
-}
openDeltaTableAt :: FilePath -> Word64 -> IO (Either String DeltaTable)
openDeltaTableAt tableRoot version = openTable (SnapshotVersion version) tableRoot


-- | Open the latest version committed at or before the requested UTC
-- time. Resolution uses millisecond precision, as in the Delta log.
-- Times outside the available history and gaps without enough timestamp
-- evidence to identify the version return an error.
openDeltaTableAtTimestamp :: FilePath -> UTCTime -> IO (Either String DeltaTable)
openDeltaTableAtTimestamp tableRoot timestamp =
  openTable (SnapshotTime (floor (utcTimeToPOSIXSeconds timestamp * 1000))) tableRoot


data SnapshotRequest = LatestSnapshot | SnapshotVersion !Word64 | SnapshotTime !Integer


openTable :: SnapshotRequest -> FilePath -> IO (Either String DeltaTable)
openTable requested tableRoot = openTableInternal True requested tableRoot


openTableInternal :: Bool -> SnapshotRequest -> FilePath -> IO (Either String DeltaTable)
openTableInternal restoreNames requested tableRoot = do
  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok
    then pure (Left ("Delta.IO: missing " ++ logDir))
    else do
      pointer <- findLastCheckpointWithPath tableRoot
      entries <- listDirectory logDir
      let LogFiles unsortedCommits compactions singles parts checksumVersions = logFilesFromNames logDir entries
      checkpoints <- availableCheckpoints logDir (orderedCheckpoints singles parts)
      let commits = sort unsortedCommits
          checksums = Set.fromList checksumVersions
          checkpointOnDisk = checkpointVersion <$> listToMaybe checkpoints
          compactionEnds = map (\(Compaction _ end path) -> (end, path)) compactions
      latestCommit <- latestLogVersion checkpointOnDisk (reverse commits)
      latest <- latestLogVersion latestCommit (sortBy (comparing (Down . fst)) compactionEnds)
      let table version history snapshot =
            DeltaTable
              { dtRoot = tableRoot
              , dtLastCheckpoint = fst <$> pointer
              , dtCheckpointAvailable = checkpointOnDisk
              , dtCommits = history
              , dtSnapshot = snapshot
              , dtVersion = version
              }
          replayAt target = do
            let history = takeWhile ((<= target) . fst) commits
            eligible <- case checkpointOnDisk of
              Just newest | newest > target ->
                availableCheckpoints logDir (dropWhile ((> target) . checkpointVersion) checkpoints)
              _ -> pure checkpoints
            replayed <- replayFromCheckpoint logDir pointer checksums target history compactions eligible
            let replayBefore version = do
                  let previous = takeWhile ((< version) . fst) history
                  candidates <- availableCheckpoints logDir (dropWhile ((>= version) . checkpointVersion) eligible)
                  replayFromCheckpoint logDir pointer checksums (version - 1) previous compactions candidates
            result <- recoverVersionChecksum target history compactions checksums replayBefore replayed
            pure (table (Just target) history <$> result)
          validate tableAtVersion = do
            snapshot <- validateLocalSnapshot (dtSnapshot tableAtVersion)
            pure tableAtVersion {dtSnapshot = if restoreNames then restoreStoredFiles snapshot else snapshot}
          openAt target = fmap (>>= validate) (replayAt target)
      case (requested, latest) of
        (SnapshotVersion target, Just newest) | target <= newest -> openAt target
        (SnapshotVersion target, _) -> pure (Left ("Delta.IO: version " ++ show target ++ " is beyond the available history"))
        (_, Nothing) -> pure (Left ("Delta.IO: no committed Delta version in " ++ logDir))
        (LatestSnapshot, Just target) -> openAt target
        (SnapshotTime timestamp, Just newest) -> do
          result <- replayAt newest
          case result of
            Left err -> pure (Left err)
            Right current | needsCatalog (dtSnapshot current) -> pure (Left catalogRequired)
            Right current -> do
              selected <- versionAtTimestamp timestamp newest current replayAt
              case selected of
                Left err -> pure (Left err)
                Right target | target == newest -> pure (validate current)
                Right target -> openAt target


-- Inspect newer candidates until one exists. Earlier entries are opened
-- only when replay or history needs them.
latestLogVersion :: Maybe Word64 -> [(Word64, FilePath)] -> IO (Maybe Word64)
latestLogVersion lowerBound = go
  where
    go ((version, path) : rest)
      | Just version > lowerBound = do
          present <- doesFileExist path
          if present then pure (Just version) else go rest
    go _ = pure lowerBound


-- | Check reader capabilities before treating a snapshot as a local table.
-- Scans also call this for snapshots assembled from decoded actions.
validateLocalSnapshot :: TableSnapshot -> Either String TableSnapshot
validateLocalSnapshot snapshot =
  case tsProtocol snapshot of
    Just protocol
      | pMinReaderVersion protocol < 1 || pMinReaderVersion protocol > 3 ->
          Left ("Delta.IO: unsupported reader protocol version " ++ show (pMinReaderVersion protocol))
      | needsCatalog snapshot -> Left catalogRequired
      | feature : _ <- filter ((`notElem` snapshotFeatures) . canonicalProtocolFeature) (pReaderFeatures protocol) ->
          Left ("Delta.IO: unsupported snapshot reader feature " ++ T.unpack feature)
      | "variantShredding" `elem` pReaderFeatures protocol
      , not ("variantType" `elem` pReaderFeatures protocol && "variantType" `elem` pWriterFeatures protocol) ->
          Left "Delta.IO: variantShredding requires the variantType reader and writer feature"
    _ -> validateConfiguration
  where
    validateConfiguration = case tsMetaData snapshot of
      Just metadata
        | let config = mdConfiguration metadata
        , Just mode <- Map.lookup "delta.columnMapping.mode" config
        , mode `notElem` ["none", "name", "id"] ->
            Left ("Delta.IO: unsupported column mapping mode " ++ T.unpack mode)
        | Map.lookup "delta.enableDeletionVectors" (mdConfiguration metadata) == Just "true"
        , not (supportsDeletionVectors snapshot) ->
            Left "Delta.IO: delta.enableDeletionVectors requires the deletionVectors reader and writer feature (protocol 3/7)"
      _ -> Right snapshot
    -- These features leave the local log replay contract intact. The
    -- resulting snapshot exposes schema and file descriptors, not data rows.
    snapshotFeatures =
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
      ]


needsCatalog :: TableSnapshot -> Bool
needsCatalog snapshot =
  maybe False (any (`elem` ["catalogManaged", "catalogOwned", "catalogOwned-preview"]) . pReaderFeatures) (tsProtocol snapshot)


catalogRequired :: String
catalogRequired = "Delta.IO: catalog-managed tables require a catalog to resolve committed versions"


-- | Whether the snapshot protocol can carry deletion vectors. Existing
-- vectors remain readable even if creation of new vectors is disabled.
supportsDeletionVectors :: TableSnapshot -> Bool
supportsDeletionVectors snapshot = case tsProtocol snapshot of
  Just protocol ->
    pMinReaderVersion protocol == 3
      && pMinWriterVersion protocol == 7
      && "deletionVectors" `elem` pReaderFeatures protocol
      && "deletionVectors" `elem` pWriterFeatures protocol
  Nothing -> False


replayFromCheckpoint :: FilePath -> Maybe (LastCheckpoint, Maybe Text) -> Set.Set Word64 -> Word64 -> [(Word64, FilePath)] -> [Compaction] -> [CheckpointFiles] -> IO (Either ReplayFailure TableSnapshot)
replayFromCheckpoint logDir pointer checksums target history compactions = tryCheckpoint Nothing
  where
    tryCheckpoint rejected [] = do
      result <- replayLogTail checksums target 0 emptySnapshot history compactions
      pure $ case (rejected, result) of
        (Just (ReplayFailure err preferred), Left (ReplayFailure problem anchor)) ->
          Left (ReplayFailure ("Delta.IO: checkpoint " ++ err ++ "; " ++ problem) (preferred <|> anchor))
        _ -> result
    tryCheckpoint rejected (checkpoint : older) = do
      let version = checkpointVersion checkpoint
          collectMetrics = maybe False (<= target) (Set.lookupGE version checksums)
      loaded <- loadCheckpoint logDir collectMetrics checkpoint
      result <- case loaded >>= validateCheckpoint checkpoint of
        Left err -> pure (Left err)
        Right snapshot -> checkCoveredSnapshot logDir checksums target version snapshot
      case result of
        Left err -> tryOlder (remember rejected (ReplayFailure err Nothing)) older
        Right snapshot
          | version == target -> pure (Right snapshot)
          | otherwise -> do
              let tailCommits = dropWhile ((<= version) . fst) history
              replayed <- replayLogTail checksums target (version + 1) snapshot tailCommits compactions
              case replayed of
                Right _ -> pure replayed
                Left (ReplayFailure err anchor) ->
                  tryOlder (remember rejected (ReplayFailure ("at version " ++ show version ++ ": " ++ err) anchor)) older

    tryOlder rejected candidates = availableCheckpoints logDir candidates >>= tryCheckpoint rejected

    remember Nothing failure = Just failure
    remember (Just (ReplayFailure earlierErr earlierAnchor)) (ReplayFailure _ anchor) =
      Just (ReplayFailure earlierErr (earlierAnchor <|> anchor))

    validateCheckpoint checkpoint snapshot = do
      validateCheckpointAddCount pointer checkpoint snapshot
      case (tsProtocol snapshot, tsMetaData snapshot) of
        (Just _, Just metadata) ->
          -- An unreadable schema makes a checkpoint unusable for scanning.
          -- Replay can still recover it from a different source.
          either (Left . ((context ++ ": ") ++)) (const (Right snapshot)) (validateMetadataSchema metadata)
        _ -> requireTableState context snapshot
      where
        context = "version " ++ show (checkpointVersion checkpoint)


-- Check the add count for the checkpoint identified by the pointer.
-- The count describes live files and is independent of tombstone retention.
validateCheckpointAddCount :: Maybe (LastCheckpoint, Maybe Text) -> CheckpointFiles -> TableSnapshot -> Either String ()
validateCheckpointAddCount pointer checkpoint snapshot
  | Just (hint, manifest) <- pointer
  , lcVersion hint == checkpointVersion checkpoint
  , matchesCheckpoint manifest (lcParts hint) checkpoint
  , Just expected <- lcNumOfAddFiles hint
  , expected /= fromIntegral actual =
      Left ("at version " ++ show (lcVersion hint) ++ ": _last_checkpoint numOfAddFiles mismatch (expected "
        ++ show expected ++ ", found " ++ show actual ++ ")")
  | otherwise = Right ()
  where
    actual = Map.size (tsFiles snapshot)
    matchesCheckpoint (Just manifest) Nothing (CheckpointSingle _ name) = manifest == T.pack name
    matchesCheckpoint Nothing Nothing (CheckpointSingle _ name) = drop 20 name == ".checkpoint.parquet"
    matchesCheckpoint Nothing (Just count) (CheckpointMultipart _ parts) = count == fromIntegral (length parts)
    matchesCheckpoint _ _ _ = False


requireTableState :: String -> TableSnapshot -> Either String TableSnapshot
requireTableState context snapshot = case (tsProtocol snapshot, tsMetaData snapshot) of
  (Just _, Just _) -> Right snapshot
  _ -> Left (context ++ " is missing protocol or table metadata")


validateMetadataSchema :: MetaDataAction -> Either String ()
validateMetadataSchema metadata =
  either (Left . ("invalid table schema: " ++)) (const (Right ())) $
    parseDeltaSchema (mdSchemaString metadata)


replayLogTail :: Set.Set Word64 -> Word64 -> Word64 -> TableSnapshot -> [(Word64, FilePath)] -> [Compaction] -> IO (Either ReplayFailure TableSnapshot)
replayLogTail checksums target start initial commits compactions
  | null usable = replayCommits checksums target start prepared commits
  | otherwise = replayRanges checksums target start prepared commits usable
  where
    usable = filter (\(Compaction first end _) -> first >= start && end <= target) compactions
    prepared = case Set.lookupGE start checksums of
      Just version | version <= target, not (isJust (cachedFileMetrics initial)) ->
        cacheFileMetrics (snapshotFileMetrics initial) initial
      _ -> initial


-- A version checksum may contain a complete snapshot. It is an auxiliary
-- artifact and cannot establish a committed version on its own.
recoverVersionChecksum
  :: Word64
  -> [(Word64, FilePath)]
  -> [Compaction]
  -> Set.Set Word64
  -> (Word64 -> IO (Either ReplayFailure TableSnapshot))
  -> Either ReplayFailure TableSnapshot
  -> IO (Either String TableSnapshot)
recoverVersionChecksum _ _ _ _ _ (Right snapshot) = pure (Right snapshot)
recoverVersionChecksum target commits compactions checksums replayBefore (Left (ReplayFailure replayError anchor)) =
  tryCandidate Nothing (reverse (filter ((`Set.member` checksums) . fst) commits))
  where
    tryCandidate rejected [] =
      pure (Left (maybe replayError (\err -> replayError ++ "; " ++ err) rejected))
    tryCandidate rejected ((version, commit) : older) = do
      let checksum = replaceExtension commit "crc"
      inputCommit <- try @IOException (BS.readFile commit)
      case inputCommit of
        Right bytes | Just prefix <- truncatedCommitPrefix bytes -> do
          result <- tryChecksum version checksum prefix
          case result of
            Right _ -> pure result
            Left err -> tryCandidate (rejected <|> Just (checksum ++ ": " ++ err)) older
        _ -> tryCandidate rejected older

    tryChecksum version checksum prefix = do
      recovered <- readRecoveryChecksum priorFiles checksum prefix
      case recovered of
        Left err -> pure (Left err)
        Right snapshot
          | version == target -> pure (Right snapshot)
          | otherwise -> do
              replayed <- replayLogTail checksums target (version + 1) snapshot (dropWhile ((<= version) . fst) commits) compactions
              pure $ case replayed of
                Left (ReplayFailure err _) -> Left err
                Right updated -> validateLocalSnapshot updated
      where
        priorFiles
          | version == 0 = pure Map.empty
          | Just (ReplayAnchor nextVersion previous) <- anchor
          , nextVersion == version = pure (tsFiles previous)
          | otherwise = either (const Map.empty) tsFiles <$> replayBefore version


readRecoveryChecksum :: IO (Map.Map Text AddAction) -> FilePath -> BS.ByteString -> IO (Either String TableSnapshot)
readRecoveryChecksum priorFiles checksum prefix =
  case recoveryCommitPrefix prefix of
    Left err -> pure (Left err)
    Right reconcile -> do
      input <- try @IOException (BS.readFile checksum)
      case either (Left . show) snapshotFromVersionChecksum input >>= reconcile of
        Left err -> pure (Left err)
        Right (ReconciledChecksum _ restore) -> Right . restore <$> priorFiles


data ReconciledChecksum = ReconciledChecksum !(Maybe CommitInfoAction) (Map.Map Text AddAction -> TableSnapshot)


recoveryCommitPrefix
  :: BS.ByteString
  -> Either String (ChecksumSnapshot -> Either String ReconciledChecksum)
recoveryCommitPrefix prefix = do
  (partial, actions) <- reconcileWith False (flip (:)) [] emptySnapshot prefix
  pure (reconcilePrefix partial actions)
  where
    reconcilePrefix partial actions (ChecksumSnapshot recovered transactions checksumTimestamp checksumTxnId fields) = do
      checkChecksumHistograms fields (tsFiles recovered)
      let agrees readField = maybe True (\value -> readField recovered == Just value) (readField partial)
      unless (agrees (fmap featureSets . tsProtocol) && agrees tsMetaData) $
        Left "checksum protocol or metadata disagrees with the commit prefix"
      let txnState = Map.fromList (map (\txn -> (txnAppId txn, txn)) (V.toList transactions))
      mapM_ (verifyPrefixAction recovered txnState) actions
      let info = tsLastCommit partial
      unless (optionalFieldsAgree (commitTxnId =<< info) checksumTxnId) (disagreement "transaction ID")
      provenance <- case checksumTimestamp of
        Just stamp | inCommitTimestampsEnabled recovered -> case info of
          Just original -> do
            unless (maybe True (== stamp) (ciInCommitTimestamp original)) (disagreement "in-commit timestamp")
            pure (Just original {ciInCommitTimestamp = Just stamp})
          Nothing -> pure $ Just CommitInfoAction
            { ciTimestamp = Nothing
            , ciInCommitTimestamp = Just stamp
            , ciOperation = Nothing
            , ciOperationParameters = mempty
            , ciIsolationLevel = Nothing
            , ciIsBlindAppend = Nothing
            , ciTxnVersion = Nothing
            , ciExtra = mempty
            }
        _ -> pure info
      pure $ ReconciledChecksum provenance $ \previous ->
        let verified = Map.mapWithKey (restoreFileStats (tsFiles partial) previous) (tsFiles recovered)
            updated = setReplayFiles verified recovered
        in (cacheFileMetrics (snapshotFileMetrics recovered) updated) {tsLastCommit = provenance}

    verifyPrefixAction recovered transactions = \case
      ActionAdd add ->
        case Map.lookup (filePathKey (addPath add)) (tsFiles recovered) of
          Just recorded
            -- Spark writes reconciled allFiles with dataChange=false.
            | sameChecksumFile add recorded
            , isJust (addStats add) == isJust (addStats recorded)
            , checksumStatsAgree add recorded -> Right ()
          _ -> disagreement "add file"
      ActionRemove remove ->
        case Map.lookup (filePathKey (removePath remove)) (tsFiles recovered) of
          Nothing -> Right ()
          Just active -> do
            remaining <- traverse deletionVectorIdentity (addDeletionVector active)
            removed <- traverse deletionVectorIdentity (removeDeletionVector remove)
            when (remaining == removed) (disagreement "remove file")
      ActionTxn txn ->
        unless (Map.lookup (txnAppId txn) transactions == Just txn) (disagreement "transaction")
      ActionDomainMetadata domain ->
        let expected = if domainRemoved domain then Nothing else Just (domainConfiguration domain)
        in unless (Map.lookup (domainName domain) (tsDomains recovered) == expected) (disagreement "domain metadata")
      _ -> Right ()
    disagreement kind = Left ("checksum " ++ kind ++ " disagrees with the commit prefix")


-- Use earlier statistics only if the recovered descriptor and its
-- statistics agree. A lost add can replace the statistics of a live file.
-- For checksum-only deletion vectors, scanning verifies the physical row
-- count against the Parquet footer before applying the bitmap.
restoreFileStats :: Map.Map Text AddAction -> Map.Map Text AddAction -> Text -> AddAction -> AddAction
restoreFileStats confirmed previous path recovered = case Map.lookup path confirmed of
  Just add -> preserve add
  Nothing -> case Map.lookup path previous of
    Just add
      | sameChecksumFile add recovered
      , Just _ <- addStats add
      , Just _ <- addStats recovered
      , checksumStatsAgree add recovered -> preserve add
    _ -> recovered {addStats = Nothing, addParsedStats = physicalCount}
  where
    preserve add = recovered {addStats = addStats add, addParsedStats = addParsedStats add}
    physicalCount = do
      _ <- addDeletionVector recovered
      Right stats <- decodeAddStats recovered
      count <- asNumRecords stats
      pure (AddStats (Just count) mempty mempty mempty mempty Nothing)


-- Checking the header keeps normal replay from decoding a full file list
-- at every version. Treat the header as a hint and verify the full checksum
-- before accepting a count discrepancy as evidence of truncation.
recoverLineBoundary :: FilePath -> BS.ByteString -> TableSnapshot -> IO (Either String (Maybe TableSnapshot))
recoverLineBoundary path prefix snapshot = do
  discrepant <- readDiscrepantChecksum path snapshot
  pure $ case discrepant of
    Just checksum | checksumMatchesCommit snapshot checksum -> case checksum of
      CompleteChecksum decoded ->
        case recoveryCommitPrefix prefix >>= ($ decoded) of
          Right (ReconciledChecksum _ restore) -> Right (Just (restore (tsFiles snapshot)))
          Left err -> Left (path ++ ": " ++ err)
      IncompleteChecksum _ _ reason -> Left (incompleteChecksumRecovery path reason)
    _ -> Right Nothing


checksumMatchesCommit :: TableSnapshot -> RecoveryChecksum -> Bool
checksumMatchesCommit snapshot checksum = case checksumTransactionId checksum of
  Just txnId -> Just txnId == (tsLastCommit snapshot >>= commitTxnId)
  Nothing -> False


checksumTransactionId :: RecoveryChecksum -> Maybe Text
checksumTransactionId (CompleteChecksum (ChecksumSnapshot _ _ _ txnId _)) = txnId
checksumTransactionId (IncompleteChecksum _ txnId _) = txnId


incompleteChecksumRecovery :: FilePath -> String -> String
incompleteChecksumRecovery path reason =
  path ++ ": replayed file metrics disagree with the version checksum; recovery requires " ++ reason


readDiscrepantChecksum :: FilePath -> TableSnapshot -> IO (Maybe RecoveryChecksum)
readDiscrepantChecksum path snapshot = do
  header <- readChecksumHeader path
  maybe (pure Nothing) (checkChecksumHeader path snapshot) header


readChecksumHeader :: FilePath -> IO (Maybe BS.ByteString)
readChecksumHeader path = do
  hint <- try @IOException (withBinaryFile path ReadMode (\handle -> BS.hGet handle 1024))
  pure $ case hint of
    Right header | not (BS.null header) -> Just header
    _ -> Nothing


checkChecksumHeader :: FilePath -> TableSnapshot -> BS.ByteString -> IO (Maybe RecoveryChecksum)
checkChecksumHeader path snapshot header
  | Just hints <- checksumFileHints header
  , not (checksumCountersDisagree hints snapshot) = pure Nothing
  | otherwise = do
      input <- try @IOException (BS.readFile path)
      pure (either (const Nothing) verify input)
  where
    verify bytes = case inspectRecoveryChecksum bytes of
      Right complete@(CompleteChecksum (ChecksumSnapshot recovered _ _ _ _))
        | snapshotFileMetricsDiffer recovered snapshot -> Just complete
      Right incomplete@(IncompleteChecksum counters _ _)
        | checksumCountersDisagree counters snapshot -> Just incomplete
      _ -> Nothing


-- A checkpoint or compacted range supplies snapshot state. The numbered
-- commit supplies provenance and ties a discrepant checksum to this version.
checkCoveredSnapshot :: FilePath -> Set.Set Word64 -> Word64 -> Word64 -> TableSnapshot -> IO (Either String TableSnapshot)
checkCoveredSnapshot logDir checksums target version snapshot = do
  discrepancy <- if hasChecksum then readDiscrepantChecksum checksum checked else pure Nothing
  let checksumTxnId = discrepancy >>= checksumTransactionId
  if version == target || isJust checksumTxnId
    then do
      info <- readCoveredCommitInfo (stem ++ ".json") checksum checked
        (version == target && hasChecksum && inCommitTimestampsEnabled checked)
      pure $ case checksumTxnId of
        Just txnId | Just txnId == (info >>= commitTxnId) ->
          Left (checksum ++ ": replayed file metrics disagree with the version checksum")
        _ -> Right (if version == target then checked {tsLastCommit = info} else checked)
    else pure (Right checked)
  where
    hasChecksum = Set.member version checksums
    checked
      | hasChecksum, not (isJust (cachedFileMetrics snapshot)) = cacheFileMetrics (snapshotFileMetrics snapshot) snapshot
      | otherwise = snapshot
    digits = show version
    stem = logDir </> (replicate (20 - length digits) '0' ++ digits)
    checksum = stem ++ ".crc"


checksumCountersDisagree :: ChecksumFileHints -> TableSnapshot -> Bool
checksumCountersDisagree hints snapshot =
  differs (hintedFileCount hints) (fromIntegral (Map.size (tsFiles snapshot))) 0
    || differs (hintedFileBytes hints) bytes byteCarry
    || differs (hintedDeletionVectors hints) (fromIntegral vectors) 0
    || differs (hintedDeletedRows hints) rows rowCarry
  where
    SnapshotFileMetrics bytes byteCarry vectors rows rowCarry = snapshotFileMetrics snapshot
    differs :: Maybe Int64 -> Word64 -> Int -> Bool
    differs claimed actual carry = maybe False (\expected -> carry /= 0 || fromIntegral expected /= actual) claimed


snapshotFileMetricsDiffer :: TableSnapshot -> TableSnapshot -> Bool
snapshotFileMetricsDiffer recovered original =
  Map.size (tsFiles recovered) /= Map.size (tsFiles original)
    || snapshotFileMetrics recovered /= snapshotFileMetrics original


data ChecksumSnapshot = ChecksumSnapshot !TableSnapshot !(V.Vector TxnAction) !(Maybe Int64) !(Maybe Text) !Object


data RecoveryChecksum
  = CompleteChecksum !ChecksumSnapshot
  | IncompleteChecksum !ChecksumFileHints !(Maybe Text) !String


snapshotFromVersionChecksum :: BS.ByteString -> Either String ChecksumSnapshot
snapshotFromVersionChecksum bytes = do
  inspected <- inspectRecoveryChecksum bytes
  case inspected of
    CompleteChecksum snapshot -> Right snapshot
    IncompleteChecksum _ _ reason -> Left ("missing " ++ reason)


inspectRecoveryChecksum :: BS.ByteString -> Either String RecoveryChecksum
inspectRecoveryChecksum bytes = do
  (fields, fileList) <- decodeOptionalChecksumFiles bytes
  (ChecksumMetrics files size metadata protocol, transactions, domains, stamp, txnId) <- parseEither parseChecksum fields
  deletionCounts@(vectors, rows) <- parseEither checksumDeletionCounts fields
  unless (maybe True (\n -> n >= 0 && fromIntegral n <= files) vectors) (Left "numDeletionVectorsOpt mismatch")
  unless (maybe True (>= 0) rows) (Left "numDeletedRecordsOpt mismatch")
  when ((files == 0 || vectors == Just 0) && maybe False (> 0) rows) (Left "numDeletedRecordsOpt mismatch")
  when (files == 0 && size /= 0) (Left "tableSizeBytes mismatch")
  validateMetadataSchema metadata
  let initial = Checkpoint.emptyCheckpointSnapshot
  described <- foldM Checkpoint.applyCheckpointAction initial [ActionProtocol protocol, ActionMetaData metadata]
  withTransactions <- V.foldM' (\snapshot -> Checkpoint.applyCheckpointAction snapshot . ActionTxn) described (fromMaybe V.empty transactions)
  withDomains <- V.foldM' (\snapshot -> Checkpoint.applyCheckpointAction snapshot . ActionDomainMetadata) withTransactions (fromMaybe V.empty domains)
  assembled <- traverse (assembleFiles files size withDomains deletionCounts) fileList
  let incomplete = IncompleteChecksum (ChecksumFileHints (Just (fromIntegral files)) (Just (fromIntegral size)) vectors rows) txnId
  case (assembled, transactions, domains) of
    (Just snapshot, Just txns, Just _) -> do
      validated <- validateLocalSnapshot snapshot
      pure (CompleteChecksum (ChecksumSnapshot validated txns stamp txnId fields))
    (Nothing, _, _) -> Right (incomplete "allFiles")
    (_, Nothing, _) -> Right (incomplete "setTransactions")
    (_, _, Nothing) -> Right (incomplete "domainMetadata")
  where
    parseChecksum fields =
      (,,,,)
        <$> checksumMetrics fields
        <*> checksumTransactions fields
        <*> checksumDomains fields
        <*> checksumInCommitTimestamp fields
        <*> fields .:? "txnId"
    assembleFiles files size described deletionCounts adds = do
      unless (fromIntegral (V.length adds) == files) (Left "numFiles mismatch")
      unless (V.foldM' subtractSize size adds == Just 0) (Left "tableSizeBytes mismatch")
      snapshot <- Checkpoint.applyCheckpointAdds described adds >>= Checkpoint.finishCheckpointForReplayWithMetrics True
      let totals = snapshotFileMetrics snapshot
      checkDeletionMetrics deletionCounts totals
      pure snapshot
    subtractSize remaining add
      | addSize add <= remaining = Just (remaining - addSize add)
      | otherwise = Nothing


data ChecksumMetrics = ChecksumMetrics !Word64 !Word64 !MetaDataAction !ProtocolAction


checkDeletionMetrics :: (Maybe Int64, Maybe Int64) -> SnapshotFileMetrics -> Either String ()
checkDeletionMetrics (vectorCount, rowCount) totals = do
  let SnapshotFileMetrics _ _ vectors rows rowCarry = totals
  unless (maybe True (== fromIntegral vectors) vectorCount) (Left "numDeletionVectorsOpt mismatch")
  unless (maybe True (\count -> count >= 0 && rowCarry == 0 && fromIntegral count == rows) rowCount) (Left "numDeletedRecordsOpt mismatch")


checksumDeletionCounts :: Object -> Parser (Maybe Int64, Maybe Int64)
checksumDeletionCounts fields = (,) <$> fields .:? "numDeletionVectorsOpt" <*> fields .:? "numDeletedRecordsOpt"


checksumTransactions :: Object -> Parser (Maybe (V.Vector TxnAction))
checksumTransactions fields = fields .:! "setTransactions"


checksumDomains :: Object -> Parser (Maybe (V.Vector DomainMetadataAction))
checksumDomains fields = fields .:! "domainMetadata"


checksumInCommitTimestamp :: Object -> Parser (Maybe Int64)
checksumInCommitTimestamp fields = fields .:? "inCommitTimestampOpt"


commitTxnId :: CommitInfoAction -> Maybe Text
commitTxnId info = case HM.lookup "txnId" (ciExtra info) of
  Just (String txnId) -> Just txnId
  _ -> Nothing


optionalFieldsAgree :: (Eq a) => Maybe a -> Maybe a -> Bool
optionalFieldsAgree (Just left) (Just right) = left == right
optionalFieldsAgree _ _ = True


checksumMetrics :: Object -> Parser ChecksumMetrics
checksumMetrics fields = do
  count <- fields .: "numFiles" :: Parser Int64
  bytes <- fields .: "tableSizeBytes" :: Parser Int64
  protocols <- fields .: "numProtocol" :: Parser Int64
  metadataActions <- fields .: "numMetadata" :: Parser Int64
  unless (protocols == 1 && metadataActions == 1) (fail "expected one protocol and one metadata action")
  unless (count >= 0) (fail "negative numFiles")
  unless (bytes >= 0) (fail "negative tableSizeBytes")
  ChecksumMetrics (fromIntegral count) (fromIntegral bytes) <$> fields .: "metadata" <*> fields .: "protocol"


-- | Compare the opened snapshot to its Delta version checksum, including
-- optional provenance fields shared by both. 'Right False' means no checksum was published.
-- Read or parse failures and metric mismatches return 'Left'.
validateVersionChecksum :: DeltaTable -> IO (Either String Bool)
validateVersionChecksum table = case dtVersion table of
  Nothing -> pure (Left "Delta.IO: cannot validate a checksum without a snapshot version")
  Just version -> do
    let digits = show version
        path = dtRoot table </> "_delta_log" </> (replicate (20 - length digits) '0' ++ digits ++ ".crc")
        describe = either (Left . ((path ++ ": ") ++)) Right
    result <- try @IOException (BS.readFile path)
    pure $ case result of
      Left err | isDoesNotExistError err -> Right False
      Left err -> describe (Left (show err))
      Right bytes -> describe $ do
        let snapshot = dtSnapshot table
            active = tsFiles snapshot
        (fields, checkedFiles) <- foldChecksumFiles (checkChecksumFile snapshot) (Right active) bytes
        ChecksumMetrics files size metadata protocol <- parseEither checksumMetrics fields
        unless (files == fromIntegral (Map.size active)) (Left "numFiles mismatch")
        let remainingBytes = Map.foldl' subtractAddSize size active
        unless (remainingBytes == 0) (Left "tableSizeBytes mismatch")
        deletionCounts <- parseEither checksumDeletionCounts fields
        checkDeletionMetrics deletionCounts (snapshotFileMetrics snapshot)
        unless (Just metadata == tsMetaData snapshot) (Left "metadata mismatch")
        unless (Just (featureSets protocol) == fmap featureSets (tsProtocol snapshot)) (Left "protocol mismatch")
        checkChecksumMetadataState fields snapshot
        checkChecksumHistograms fields active
        txnId <- parseEither (.:? "txnId") fields
        let info = tsLastCommit snapshot
        unless (optionalFieldsAgree (commitTxnId =<< info) txnId) (Left "txnId mismatch")
        when (inCommitTimestampsEnabled snapshot) $ do
          recorded <- parseEither checksumInCommitTimestamp fields
          unless (optionalFieldsAgree (ciInCommitTimestamp =<< info) recorded) (Left "inCommitTimestamp mismatch")
        case checkedFiles of
          Nothing -> pure ()
          Just checked -> do
            remaining <- checked
            unless (Map.null remaining) (Left "allFiles is missing live files")
        Right True
  where
    -- Every checksum size is a nonnegative signed Long. A larger
    -- sentinel can therefore remember any overflow or overshoot.
    subtractAddSize remaining add
      | remaining == maxBound || addSize add > remaining = maxBound
      | otherwise = remaining - addSize add


-- Spark historically published the same file-size histogram as histogramOpt.
checkChecksumHistograms :: Object -> Map.Map Text AddAction -> Either String ()
checkChecksumHistograms fields active = do
  fileSizes <- parseEither (.:? "fileSizeHistogram") fields
  legacySizes <- parseEither (.:? "histogramOpt") fields
  unless (optionalFieldsAgree fileSizes legacySizes) (Left "fileSizeHistogram disagrees with histogramOpt")
  mapM_ checkFileSizes (fileSizes <|> legacySizes)
  deleted <- parseEither (.:? "deletedRecordCountsHistogramOpt") fields
  mapM_ checkDeletedCounts deleted
  where
    checkFileSizes value = do
      (boundaries, expectedCounts, expectedBytes) <- parseEither (withObject "fileSizeHistogram" $ \histogram ->
        (,,) <$> readHistogramLongs histogram "sortedBinBoundaries"
             <*> readHistogramLongs histogram "fileCounts"
             <*> readHistogramLongs histogram "totalBytes") value
      unless (not (U.null boundaries) && U.head boundaries == 0
        && U.and (U.zipWith (<) boundaries (U.drop 1 boundaries))) (Left "fileSizeHistogram invalid bin boundaries")
      let bins = U.length boundaries
      unless (U.length expectedCounts == bins && U.length expectedBytes == bins) (Left "fileSizeHistogram bin lengths mismatch")
      let (counts, sizes) = runST $ do
            remainingCounts <- U.thaw expectedCounts
            remainingBytes <- U.thaw expectedBytes
            let visit MapInternal.Tip = pure ()
                visit (MapInternal.Bin _ _ add left right) = do
                  let size = addSize add
                      bin = histogramBin boundaries size
                  UM.unsafeModify remainingCounts (`subtractHistogramValue` 1) bin
                  UM.unsafeModify remainingBytes (`subtractHistogramValue` size) bin
                  visit left
                  visit right
            visit active
            (,) <$> U.unsafeFreeze remainingCounts <*> U.unsafeFreeze remainingBytes
      unless (U.all (== 0) counts) (Left "fileSizeHistogram fileCounts mismatch")
      unless (U.all (== 0) sizes) (Left "fileSizeHistogram totalBytes mismatch")
    checkDeletedCounts value = do
      expected <- parseEither (withObject "deletedRecordCountsHistogramOpt" (`readHistogramLongs` "deletedRecordCounts")) value
      unless (U.length expected == U.length deletedCountBoundaries) (Left "deletedRecordCountsHistogramOpt bin length mismatch")
      remaining <- runST $ do
        bins <- U.thaw expected
        let subtractFile bin right = do
              UM.unsafeModify bins (`subtractHistogramValue` 1) bin
              visit right
            visit MapInternal.Tip = pure Nothing
            visit (MapInternal.Bin _ _ add left right) = do
              err <- visit left
              case err of
                Just _ -> pure err
                Nothing -> case addDeletionVector add of
                  Nothing -> subtractFile 0 right
                  Just dv -> case descriptorCardinality dv of
                    Left reason -> pure (Just reason)
                    Right count -> subtractFile (histogramBin deletedCountBoundaries count) right
        err <- visit active
        case err of
          Just reason -> pure (Left reason)
          Nothing -> Right <$> U.unsafeFreeze bins
      unless (U.all (== 0) remaining) (Left "deletedRecordCountsHistogramOpt mismatch")


readHistogramLongs :: Object -> Key -> Parser (U.Vector Word64)
readHistogramLongs histogram key = do
  values <- histogram .: key :: Parser (V.Vector Int64)
  unless (V.all (>= 0) values) (fail "histogram entries must be nonnegative")
  pure (U.convert (V.map fromIntegral values))


-- Each expected bucket is a signed Long, leaving maxBound as an overshoot sentinel.
subtractHistogramValue :: Word64 -> Word64 -> Word64
subtractHistogramValue remaining actual
  | remaining == maxBound || actual > remaining = maxBound
  | otherwise = remaining - actual
{-# INLINE subtractHistogramValue #-}


-- The first boundary is zero. Find the last boundary at or below the value.
histogramBin :: U.Vector Word64 -> Word64 -> Int
histogramBin boundaries value = search 0 (U.length boundaries)
  where
    search !low !high
      | high - low == 1 = low
      | U.unsafeIndex boundaries middle <= value = search middle high
      | otherwise = search low middle
      where
        middle = low + (high - low) `quot` 2
{-# INLINE histogramBin #-}


deletedCountBoundaries :: U.Vector Word64
deletedCountBoundaries = U.fromList [0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 2147483647]


checkChecksumMetadataState :: Object -> TableSnapshot -> Either String ()
checkChecksumMetadataState fields snapshot = do
  transactions <- parseEither checksumTransactions fields
  domains <- parseEither checksumDomains fields
  mapM_ checkTransactions transactions
  mapM_ checkDomains domains
  where
    checkTransactions actions = do
      let recorded = Map.fromList (map (\txn -> (txnAppId txn, txnVersion txn)) (V.toList actions))
          replayed = tsAppIds snapshot
      unless (Map.size recorded == V.length actions) (Left "setTransactions contains a duplicate appId")
      -- A writer can expire transactions using its own clock. The
      -- local snapshot and checksum can originate at different cutoffs.
      let agree = case snapshotProperty "delta.setTransactionRetentionDuration" snapshot of
            Nothing -> recorded == replayed
            Just _ -> Map.foldrWithKey (\app version rest -> optionalFieldsAgree (Map.lookup app replayed) (Just version) && rest) True recorded
      unless agree (Left "setTransactions mismatch")
    checkDomains actions = do
      when (V.any domainRemoved actions) (Left "domainMetadata contains a tombstone")
      let recorded = Map.fromList (map (\domain -> (domainName domain, domainConfiguration domain)) (V.toList actions))
      unless (Map.size recorded == V.length actions) (Left "domainMetadata contains a duplicate domain")
      unless (recorded == tsDomains snapshot) (Left "domainMetadata mismatch")


-- Remove each match from a persistent copy of the snapshot's file map.
checkChecksumFile :: TableSnapshot -> Either String (Map.Map Text AddAction) -> AddAction -> Either String (Map.Map Text AddAction)
checkChecksumFile snapshot checked listed = do
  remaining <- checked
  case Map.updateLookupWithKey (\_ _ -> Nothing) path remaining of
    (Just actual, updated)
      | not (sameChecksumFile actual listed) -> mismatch "descriptor disagrees with the snapshot"
      | not (checksumStatsAgree actual listed) -> mismatch "statistics disagree with the snapshot"
      | otherwise -> updated `seq` Right updated
    _ -> checkChecksumFileAlias snapshot remaining listed
  where
    path = addPath listed
    mismatch reason = Left ("allFiles " ++ reason ++ " at " ++ show path)


checkChecksumFileAlias :: TableSnapshot -> Map.Map Text AddAction -> AddAction -> Either String (Map.Map Text AddAction)
checkChecksumFileAlias snapshot remaining listed
  | Map.member path active = mismatch "contains a duplicate path"
  | otherwise = case resolveSnapshotFilePath path snapshot of
      Left reason -> mismatch reason
      Right key -> case Map.updateLookupWithKey (\_ _ -> Nothing) key remaining of
        (Nothing, _)
          | Map.member key active -> mismatch "contains a duplicate path"
          | otherwise -> mismatch "contains a file absent from the snapshot"
        (Just actual, updated)
          | not (sameChecksumFile actual listed) -> mismatch "descriptor disagrees with the snapshot"
          | not (checksumStatsAgree actual listed) -> mismatch "statistics disagree with the snapshot"
          | otherwise -> updated `seq` Right updated
  where
    path = addPath listed
    active = tsFiles snapshot
    mismatch reason = Left ("allFiles " ++ reason ++ " at " ++ show path)
{-# NOINLINE checkChecksumFileAlias #-}


sameChecksumFile :: AddAction -> AddAction -> Bool
sameChecksumFile actual listed =
  addSize actual == addSize listed
    && addModificationTime actual == addModificationTime listed
    && addPartitionValues actual == addPartitionValues listed
    && addTags actual == addTags listed
    && deletionVectorsAgree
    && addBaseRowId actual == addBaseRowId listed
    && addDefaultRowCommitVersion actual == addDefaultRowCommitVersion listed
  where
    actualVector = addDeletionVector actual
    listedVector = addDeletionVector listed
    -- The replayed descriptor is already validated. Compare parsed fields
    -- only when the two encodings differ.
    deletionVectorsAgree = actualVector == listedVector || sameDeletionVectorDescriptor actualVector listedVector
{-# INLINE sameChecksumFile #-}


-- Spark can omit JSON statistics in a checkpoint even when they exist in
-- the checksum. Compare serialized statistics when both have them, using
-- their contents when the JSON spellings differ.
checksumStatsAgree :: AddAction -> AddAction -> Bool
checksumStatsAgree actual listed = case (addStats actual, addStats listed) of
  (Just before, Just after) ->
    before == after || case (decodeAddStats actual, decodeAddStats listed) of
      (Just (Right left), Just (Right right)) -> left {asTightBounds = Just (effectiveTightBounds left)}
        == right {asTightBounds = Just (effectiveTightBounds right)}
      _ -> False
  _ -> True
{-# INLINE checksumStatsAgree #-}


featureSets :: ProtocolAction -> ProtocolAction
featureSets protocol = protocol
  { pReaderFeatures = sort (pReaderFeatures protocol)
  , pWriterFeatures = sort (pWriterFeatures protocol)
  }


data ReplaySegment = ReplaySegment
  { segmentEnd :: !Word64
  , segmentPath :: !FilePath
  , segmentCompacted :: !Bool
  }


data ReplayStep = ReplayStep {-# UNPACK #-} !Int !ReplaySegment !Bool


data ReplayAnchor = ReplayAnchor {-# UNPACK #-} !Word64 !TableSnapshot


data ReplayFailure = ReplayFailure !String !(Maybe ReplayAnchor)


-- Find a contiguous route to the target before opening any segment.
-- Selecting the farthest endpoint greedily can strand a replay when
-- a shorter range connects to another compaction. Compute the cheapest
-- reachable suffix at each version, counting one read per segment. A
-- failed numbered commit can be bypassed by an overlapping compaction,
-- so exclude failed segments of either kind when replanning. Before
-- the first fork, every reachable route has the same prefix. Keep one
-- snapshot there so retries can skip that prefix without keeping a
-- snapshot for every segment or reusing state from an abandoned route.
replayRanges :: Set.Set Word64 -> Word64 -> Word64 -> TableSnapshot -> [(Word64, FilePath)] -> [Compaction] -> IO (Either ReplayFailure TableSnapshot)
replayRanges checksums target start initial commits compactions = retry start initial Set.empty Nothing
  where
    candidates =
      Map.fromListWith
        (++)
        ( map (\(version, path) -> (version, [ReplaySegment version path False])) commits
            ++ map (\(Compaction first end path) -> (first, [ReplaySegment end path True])) compactions
        )
    retry resume snapshot excluded earlierError =
      let suffix = takeWhile ((>= resume) . fst) descending
          route = foldl' (\rest (version, segments) -> choose excluded version segments rest) Map.empty suffix
      in case Map.lookup resume route of
           Nothing -> pure $ Left $ fromMaybe
             (ReplayFailure ("Delta.IO: no contiguous log history from version " ++ show start ++ " through version " ++ show target) Nothing)
             earlierError
           Just _ -> follow route resume snapshot Nothing excluded earlierError

    descending = Map.toDescList candidates

    choose excluded version segments rest =
      case foldl' best Nothing segments of
        Just step -> Map.insert version step rest
        Nothing -> rest
      where
        best current segment
          | Set.member (segmentPath segment) excluded = current
          | Just cost <- suffixCost segment =
              let next = cost + 1
              in case current of
                   Nothing -> Just (ReplayStep next segment False)
                   Just (ReplayStep old previous _)
                     | (next, Down (segmentEnd segment), segmentPath segment) < (old, Down (segmentEnd previous), segmentPath previous) ->
                         Just (ReplayStep next segment True)
                     | otherwise -> Just (ReplayStep old previous True)
          | otherwise = current
        suffixCost segment
          | segmentEnd segment == target = Just (0 :: Int)
          | otherwise = do
              ReplayStep cost _ _ <- Map.lookup (segmentEnd segment + 1) rest
              pure cost

    follow route version !snapshot anchor excluded earlierError =
      case Map.lookup version route of
        Nothing -> pure (Left (ReplayFailure ("Delta.IO: missing log segment at version " ++ show version) Nothing))
        Just (ReplayStep _ segment fork) -> do
          let path = segmentPath segment
              compacted = segmentCompacted segment
              end = segmentEnd segment
          replayed <- readSegment checksums version compacted snapshot path
          result <- case replayed of
            Right updated | compacted ->
              either (Left . (("Delta.IO: compaction " ++ path ++ ": ") ++)) Right
                <$> checkCoveredSnapshot (takeDirectory path) checksums target end updated
            _ -> pure replayed
          case result of
            Left err ->
              let failed = Set.insert path excluded
                  resumeFrom = if compacted then Nothing else Just (ReplayAnchor version snapshot)
                  firstError = earlierError <|> Just (ReplayFailure err resumeFrom)
              in case anchor of
                   Nothing -> retry version snapshot failed firstError
                   Just (ReplayAnchor resume prefix) -> retry resume prefix failed firstError
            Right updated
              | segmentEnd segment < target ->
                  let nextAnchor = case anchor of
                        Nothing | fork -> Just (ReplayAnchor version snapshot)
                        _ -> anchor
                  in nextAnchor `seq` follow route (segmentEnd segment + 1) updated nextAnchor excluded earlierError
              | otherwise -> pure (Right updated)

replayCommits :: Set.Set Word64 -> Word64 -> Word64 -> TableSnapshot -> [(Word64, FilePath)] -> IO (Either ReplayFailure TableSnapshot)
replayCommits checksums target expected !snapshot commits = case commits of
  (version, path) : rest | version == expected -> do
    result <- readSegment checksums version False snapshot path
    case result of
      Left err -> pure (Left (ReplayFailure err (Just (ReplayAnchor version snapshot))))
      Right updated
        | version == target -> pure (Right updated)
        | otherwise -> replayCommits checksums target (version + 1) updated rest
  _ -> pure (Left (ReplayFailure ("Delta.IO: missing JSON commit at version " ++ show expected) Nothing))


readSegment :: Set.Set Word64 -> Word64 -> Bool -> TableSnapshot -> FilePath -> IO (Either String TableSnapshot)
readSegment checksums version compacted snapshot path = do
  input <- try @IOException (BS.readFile path)
  let result = do
        bytes <- either (Left . show) Right input
        unless compacted (requireCommitBytes bytes)
        reconcileSegment compacted snapshot bytes
  checked <- case (input, result) of
    (_, Left err) -> pure (Left err)
    (Right bytes, Right updated)
      | not compacted
      , Set.member version checksums
      , isJust (tsLastCommit updated >>= commitTxnId) ->
          fmap (fmap (fromMaybe updated)) (recoverLineBoundary (replaceExtension path "crc") bytes updated)
    (_, Right updated) -> pure (Right updated)
  let initialState updated
        | version == 0 = requireTableState "initial log segment" updated
        | otherwise = Right updated
  pure $ case checked >>= initialState of
    Left err -> Left ("Delta.IO: commit " ++ path ++ ": " ++ err)
    Right updated -> Right updated


requireCommitBytes :: BS.ByteString -> Either String ()
requireCommitBytes bytes = when (BS.null bytes) (Left "empty Delta commit")


data SegmentActions = SegmentActions
  !TableSnapshot
  !SegmentAdds
  !(Map.Map Text (Set.Set (Maybe DeletionVectorIdentity)))
  !SegmentMetadata


data SegmentMetadata = SegmentMetadata
  { seenProtocol :: !Bool
  , seenMetadata :: !Bool
  , seenTransactions :: !(Set.Set Text)
  , seenDomains :: !(Set.Set Text)
  }


-- Writers can emit thousands of adds in path order. Keep the reversed
-- list so ordered paths build a map in linear time. For shuffled paths,
-- build once and detect duplicates by comparing the input and map sizes.
data SegmentAdds = SegmentAdds !Bool {-# UNPACK #-} !Int ![(Text, AddAction)]


insertSegmentAdd :: AddAction -> SegmentAdds -> SegmentAdds
insertSegmentAdd add (SegmentAdds ordered count entries) =
  SegmentAdds (ordered && followsPrevious) (count + 1) ((path, add) : entries)
  where
    path = filePathKey (addPath add)
    followsPrevious = case entries of
      [] -> True
      (previous, _) : _ -> previous < path


segmentAddMap :: SegmentAdds -> Either String (Map.Map Text AddAction)
segmentAddMap (SegmentAdds ordered count entries)
  | ordered = Right (Map.fromDistinctDescList entries)
  | Map.size additions == count = Right additions
  | otherwise = Left "duplicate add path"
  where
    additions = Map.fromList entries


-- A segment's file actions are unordered. Check replacements against the
-- entering snapshot and remove old logical files before installing adds.
reconcileSegment :: Bool -> TableSnapshot -> BS.ByteString -> Either String TableSnapshot
reconcileSegment compacted snapshot bytes =
  fst <$> reconcileWith compacted (\() _ -> ()) () snapshot bytes
{-# INLINE reconcileSegment #-}


reconcileWith :: Bool -> (a -> DeltaAction -> a) -> a -> TableSnapshot -> BS.ByteString -> Either String (TableSnapshot, a)
reconcileWith compacted collect collected snapshot bytes = do
  let initial =
        SegmentActions
          (snapshot {tsLastCommit = Nothing})
          (SegmentAdds True 0 [])
          Map.empty
          (SegmentMetadata False False Set.empty Set.empty)
      combined state action = do
        (segment, acc) <- state
        updated <- step (Right segment) action
        let !next = collect acc action
        pure (updated, next)
  (SegmentActions updated adds removals _, changes) <-
    if compacted
      then foldCompactedBytes combined (Right (initial, collected)) bytes >>= id
      else foldCommitBytes combined (Right (initial, collected)) bytes >>= id
  additions <- segmentAddMap adds
  let before = tsFiles snapshot
      checkOverlap path (add, identities) rest = do
        identity <- traverse deletionVectorIdentity (addDeletionVector add)
        when (Set.member identity identities) $
          Left ("add and remove describe the same logical file at " ++ show path)
        rest
      checkReplacement path (add, previous) rest = do
        identity <- traverse deletionVectorIdentity (addDeletionVector add)
        prior <- traverse deletionVectorIdentity (addDeletionVector previous)
        unless (prior == identity || maybe False (Set.member prior) (Map.lookup path removals)) $
          Left ("deletion-vector replacement is missing the prior remove at " ++ show path)
        rest
      retainUnlessRemoved identities add =
        case traverse deletionVectorIdentity (addDeletionVector add) of
          Right identity | Set.member identity identities -> Nothing
          _ -> Just add
  Map.foldrWithKey checkOverlap (Right ()) (Map.intersectionWith (,) additions removals)
  Map.foldrWithKey checkReplacement (Right ()) (Map.intersectionWith (,) additions before)
  let remaining = Map.foldlWithKey' (\files path identities -> Map.update (retainUnlessRemoved identities) path files) before removals
      final = setReplayFiles (Map.union additions remaining) updated
      metrics = Map.foldlWithKey' replaceFile afterRemoves additions
      afterRemoves = Map.foldlWithKey' removeFile (snapshotFileMetrics snapshot) removals
      removeFile totals path identities = case Map.lookup path before of
        Just previous | Nothing <- retainUnlessRemoved identities previous -> updateFileMetrics (-1) totals previous
        _ -> totals
      replaceFile totals path add =
        updateFileMetrics 1 (maybe totals (updateFileMetrics (-1) totals) (Map.lookup path remaining)) add
  case cachedFileMetrics snapshot of
    Just _ -> metrics `seq` pure (cacheFileMetrics metrics final, changes)
    Nothing -> pure (final, changes)
  where
    step state action = do
      SegmentActions updated additions removals metadata <- state
      let applyMetadata metadata' = pure (SegmentActions (applyMetadataAction updated action) additions removals metadata')
      case action of
        ActionAdd add -> do
          case addDeletionVector add of
            Just descriptor -> () <$ deletionVectorIdentity descriptor
            Nothing -> Right ()
          pure (SegmentActions updated (insertSegmentAdd add additions) removals metadata)
        ActionRemove remove -> do
          identity <- traverse deletionVectorIdentity (removeDeletionVector remove)
          let path = filePathKey (removePath remove)
              identities = Map.findWithDefault Set.empty path removals
          when (if compacted then Set.member identity identities else not (Set.null identities)) $
            Left ("duplicate remove at " ++ show path)
          pure (SegmentActions updated additions (Map.insert path (Set.insert identity identities) removals) metadata)
        ActionProtocol _
          | seenProtocol metadata -> Left "duplicate protocol action"
          | otherwise -> applyMetadata (metadata {seenProtocol = True})
        ActionMetaData replacement
          | seenMetadata metadata -> Left "duplicate metadata action"
          | otherwise -> do
              validateMetadataSchema replacement
              applyMetadata (metadata {seenMetadata = True})
        ActionTxn txn
          | Set.member (txnAppId txn) (seenTransactions metadata) ->
              Left ("duplicate transaction appId " ++ show (txnAppId txn))
          | otherwise ->
              applyMetadata (metadata {seenTransactions = Set.insert (txnAppId txn) (seenTransactions metadata)})
        ActionDomainMetadata domain
          | Set.member (domainName domain) (seenDomains metadata) ->
              Left ("duplicate metadata domain " ++ show (domainName domain))
          | otherwise ->
              applyMetadata (metadata {seenDomains = Set.insert (domainName domain) (seenDomains metadata)})
        _ -> applyMetadata metadata
{-# INLINE reconcileWith #-}


readCoveredCommitInfo :: FilePath -> FilePath -> TableSnapshot -> Bool -> IO (Maybe CommitInfoAction)
readCoveredCommitInfo path checksum snapshot recoverClock = do
  input <- try @IOException (BS.readFile path)
  case input of
    Left _ -> pure Nothing
    Right bytes
      | let info = parseCommitInfo bytes
      , recoverClock
      , Nothing <- info >>= ciInCommitTimestamp
      , Just prefix <- truncatedCommitPrefix bytes -> do
          crc <- try @IOException (BS.readFile checksum)
          pure $ either (const info) id $ do
            reconcile <- recoveryCommitPrefix prefix
            decoded@(ChecksumSnapshot recovered _ _ _ fields) <-
              either (Left . show) snapshotFromVersionChecksum crc
            validateCoveredChecksum snapshot recovered fields
            ReconciledChecksum provenance _ <- reconcile decoded
            pure provenance
      | otherwise -> pure (parseCommitInfo bytes)


-- Only borrow a clock from a checksum that describes the selected state.
-- Replay maps are keyed by reconciled paths on both sides.
validateCoveredChecksum :: TableSnapshot -> TableSnapshot -> Object -> Either String ()
validateCoveredChecksum covered recovered fields = do
  unless (tsMetaData covered == tsMetaData recovered
    && fmap featureSets (tsProtocol covered) == fmap featureSets (tsProtocol recovered)) $
    Left "checksum protocol or metadata disagrees with the covered snapshot"
  checkChecksumMetadataState fields covered
  let actual = tsFiles covered
      expected = tsFiles recovered
      agree file listed = sameChecksumFile file listed && checksumStatsAgree file listed
  unless (Map.size actual == Map.size expected && Map.isSubmapOfBy agree actual expected) $
    Left "checksum file list disagrees with the covered snapshot"


-- | A numbered commit and the state on either side. Each entry shares
-- unchanged file-map nodes with its predecessor. Paths in 'tsFiles'
-- use reconciliation keys through the stream.
data DeltaCommit = DeltaCommit
  { dcVersion :: !Word64
  , dcPath :: !FilePath
  , dcBefore :: !TableSnapshot
  , dcAfter :: !TableSnapshot
  , dcSchemaChanged :: !Bool
  , dcFileActions :: ![DeltaAction]
  }


-- | Replay numbered commits over an inclusive range. A checkpoint can
-- seed the state before the range. Changes inside it require original
-- commits because compacted ranges and checkpoints discard events.
streamDeltaCommits :: FilePath -> Word64 -> Word64 -> IO (Either String (IterIO DeltaCommit))
streamDeltaCommits root start end
  | start > end || end > fromIntegral (maxBound :: Int64) =
      pure (Left "Delta.IO: invalid commit version range")
  | otherwise = do
      initial <- if start == 0
        then pure (Right emptySnapshot)
        else fmap (fmap dtSnapshot) (openTableInternal False (SnapshotVersion (start - 1)) root)
      pure (step start <$> initial)
  where
    step version snapshot = IterIO $ do
      let digits = show version
          path = root </> "_delta_log" </> (replicate (20 - length digits) '0' ++ digits ++ ".json")
      input <- try @IOException (BS.readFile path)
      let replay bytes = requireCommitBytes bytes >> reconcileWith False collect (False, []) snapshot bytes
          result = either (Left . show) (\bytes -> fmap (bytes,) (replay bytes)) input
      checked <- case result of
        Right (commitBytes, (updated, changes)) | isJust (tsLastCommit updated >>= commitTxnId) -> do
          let checksum = replaceExtension path "crc"
          header <- readChecksumHeader checksum
          case header of
            Nothing -> pure (Right (updated, changes))
            Just bytes -> do
              let cached = case cachedFileMetrics updated of
                    Just _ -> updated
                    Nothing -> cacheFileMetrics (snapshotFileMetrics updated) updated
              discrepancy <- checkChecksumHeader checksum cached bytes
              pure $ case discrepancy of
                Just detected | checksumMatchesCommit updated detected -> case detected of
                  CompleteChecksum recovered -> do
                    _ <- recoveryCommitPrefix commitBytes >>= ($ recovered)
                    Left "file metrics disagree with the version checksum; original change events are incomplete"
                  IncompleteChecksum _ _ reason -> Left (incompleteChecksumRecovery checksum reason)
                _ -> Right (cached, changes)
        _ -> pure (snd <$> result)
      pure $ either (Left . ((path ++ ": ") ++)) Right $ do
        (updated, (schemaChanged, changes)) <- checked
        validated <- requireTableState "commit" updated >>= validateLocalSnapshot
        let rest = if version == end then IterIO (pure (Right IterIODone)) else step (version + 1) validated
        pure (IterIOYield (DeltaCommit version path snapshot validated schemaChanged (reverse changes)) rest)
    collect acc@(!schemaChanged, !changes) action = case action of
      ActionAdd _ -> (schemaChanged, action : changes)
      ActionRemove _ -> (schemaChanged, action : changes)
      ActionCdc _ -> (schemaChanged, action : changes)
      ActionMetaData _ -> (True, changes)
      ActionProtocol _ -> (True, changes)
      _ -> acc


-- | The clock assigned by the table protocol. Before in-commit
-- timestamps turn on, use the numbered commit file modification time.
deltaCommitMillis :: DeltaCommit -> IO (Either String Int64)
deltaCommitMillis entry = case timestampCutover snapshot of
  Left err -> pure (Left err)
  Right cutover
    | inCommitTimestampsEnabled snapshot
    , maybe True ((<= dcVersion entry) . fst) cutover ->
        pure (maybe (Left "Delta.IO: missing inCommitTimestamp") Right (tsLastCommit snapshot >>= ciInCommitTimestamp))
    | otherwise -> do
        value <- try @IOException (modificationMillis (dcPath entry))
        pure (either (Left . show) Right value)
  where
    snapshot = dcAfter entry


missingLogFile :: FilePath -> IOException -> IO Bool
missingLogFile path err
  | isDoesNotExistError err = pure True
  | otherwise = doesDirectoryExist path


{- | Find the highest-version checkpoint candidate on disk, including
V2 JSON checkpoints and complete multipart sets. This does not decode
the checkpoint.
-}
findCheckpointParquet :: FilePath -> IO (Maybe Word64)
findCheckpointParquet tableRoot = do
  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok
    then pure Nothing
    else do
      names <- listDirectory logDir
      fmap (fmap checkpointVersion . listToMaybe) (availableCheckpoints logDir (checkpointsFromNames names))


data CheckpointFiles
  = CheckpointSingle !Word64 !FilePath
  | CheckpointMultipart !Word64 ![FilePath]


checkpointVersion :: CheckpointFiles -> Word64
checkpointVersion (CheckpointSingle version _) = version
checkpointVersion (CheckpointMultipart version _) = version


-- Check only the leading candidate, deferring older file checks until
-- replay needs a fallback. A directory entry alone cannot complete a set.
availableCheckpoints :: FilePath -> [CheckpointFiles] -> IO [CheckpointFiles]
availableCheckpoints logDir = go
  where
    go [] = pure []
    go candidates@(checkpoint : rest) = do
      present <- case checkpoint of
        CheckpointSingle _ name -> exists name
        CheckpointMultipart _ names -> allPartsExist names
      if present then pure candidates else go rest
    exists name = doesFileExist (logDir </> name)
    allPartsExist [] = pure True
    allPartsExist (name : rest) = do
      present <- exists name
      if present then allPartsExist rest else pure False


checkpointsFromNames :: [FilePath] -> [CheckpointFiles]
checkpointsFromNames names =
  let (singles, parts) = foldl' add ([], []) names
  in orderedCheckpoints singles parts
  where
    add candidates@(!singles, !parts) name
      | Just (version, suffix) <- numberedLogName 20 name =
          case namedCheckpoint version suffix name of
            Just (NamedSingle single) -> (single : singles, parts)
            Just (NamedPart part) -> (singles, part : parts)
            Nothing -> candidates
      | otherwise = candidates


type CheckpointPart = ((Word64, Word64), Word64, FilePath)


data NamedCheckpoint = NamedSingle !CheckpointFiles | NamedPart !CheckpointPart


namedCheckpoint :: Word64 -> String -> FilePath -> Maybe NamedCheckpoint
namedCheckpoint version suffix name = do
  rest <- stripPrefix ".checkpoint." suffix
  fmap NamedSingle (singleCheckpoint version rest name) <|> fmap NamedPart (checkpointPart version rest name)


orderedCheckpoints :: [CheckpointFiles] -> [CheckpointPart] -> [CheckpointFiles]
orderedCheckpoints singles parts = sortBy (comparing (Down . checkpointVersion)) (reverse singles ++ multipart)
  where
    multipart = mapMaybe complete (groupBy (\(key, _, _) (key', _, _) -> key == key') (sort parts))
    complete [] = Nothing
    complete group@(((version, total), _, _) : _)
      | fromIntegral (length group) == total = Just (CheckpointMultipart version (map (\(_, _, name) -> name) group))
      | otherwise = Nothing


singleCheckpoint :: Word64 -> String -> FilePath -> Maybe CheckpointFiles
singleCheckpoint version rest name
  | rest == "parquet" = Just (CheckpointSingle version name)
  | let (uuid, extension) = splitAt 36 rest
  , extension == ".parquet" || extension == ".json"
  , length uuid == 36
  , and (zipWith (\i c -> if i `elem` ([8, 13, 18, 23] :: [Int]) then c == '-' else isHexDigit c) [0 ..] uuid) =
      Just (CheckpointSingle version name)
  | otherwise = Nothing


checkpointPart :: Word64 -> String -> FilePath -> Maybe CheckpointPart
checkpointPart version rest name = case numberedLogName 10 rest of
  Just (ordinal, '.' : tailText)
    | Just (total, ".parquet") <- numberedLogName 10 tailText
    , total > 1 && ordinal > 0 && ordinal <= total -> Just ((version, total), ordinal, name)
  _ -> Nothing


loadCheckpoint :: FilePath -> Bool -> CheckpointFiles -> IO (Either String TableSnapshot)
loadCheckpoint logDir collectMetrics (CheckpointMultipart _ parts) = do
  loaded <- foldM loadPart (Right Checkpoint.emptyCheckpointSnapshot) parts
  pure $ do
    snapshot <- loaded
    checkpointResult logDir $ do
      when (maybe False (elem "v2Checkpoint" . pReaderFeatures) (Checkpoint.checkpointProtocol snapshot)) $
        Left "multipart checkpoint cannot enable v2Checkpoint"
      Checkpoint.finishCheckpointForReplayWithMetrics collectMetrics snapshot
  where
    loadPart (Left err) _ = pure (Left err)
    loadPart (Right snapshot) name = do
      let path = logDir </> name
      checkpointResult path <$> Checkpoint.readCheckpointFoldWithAddBatches Checkpoint.applyCheckpointAddBatch applyPartRow snapshot path
    applyPartRow snapshot action
      | isV2Action action = Left "V2 actions in a multipart checkpoint"
      | otherwise = Checkpoint.applyCheckpointAction snapshot action
loadCheckpoint logDir collectMetrics (CheckpointSingle version name) = do
  let path = logDir </> name
      step = applyCheckpointRow version
      initial = CheckpointRows Checkpoint.emptyCheckpointSnapshot Nothing False []
      stepJson (Left err) _ = Left err
      stepJson (Right rows) action = step rows action
  decoded <-
    if takeExtension name == ".json"
      then do
        input <- try @IOException (BS.readFile path)
        pure $ do
          bytes <- either (Left . show) Right input
          foldCheckpointJson stepJson (Right initial) bytes >>= id
      else Checkpoint.readCheckpointFoldWithAddBatches applyCheckpointAddRows step initial path
  case checkpointResult path decoded of
    Left err -> pure (Left err)
    Right (CheckpointRows checkpoint metadata inline sidecars)
      | v2 metadata sidecars && not (isJust metadata) -> invalid
      | inline && not (null sidecars) -> invalid
      | v2 metadata sidecars && not (maybe False supportsV2Checkpoint (Checkpoint.checkpointProtocol checkpoint)) ->
          pure (Left (path ++ ": V2 checkpoint requires reader version 3, writer version 7, and v2Checkpoint in both feature lists"))
      | otherwise -> case checkpointResult path (checkCheckpointTag metadata "sidecarSizeInBytes" sidecarBytes) of
          Left err -> pure (Left err)
          Right () -> do
            loaded <- loadSidecars logDir checkpoint (reverse sidecars)
            pure $ do
              snapshot <- loaded
              checkpointResult path $ do
                checkCheckpointTag metadata "numOfAddFiles" (toInteger (Checkpoint.checkpointAddCount snapshot))
                let sidecarActions = if null sidecars then 0 else Checkpoint.checkpointFileActionCount snapshot
                checkCheckpointTag metadata "sidecarNumActions" sidecarActions
                Checkpoint.finishCheckpointForReplayWithMetrics collectMetrics snapshot
      where
        sidecarBytes = foldl' (\total sidecar -> total + toInteger (sidecarSize sidecar)) 0 sidecars
  where
    v2 metadata sidecars = isJust metadata || not (null sidecars) || drop 20 name /= ".checkpoint.parquet"
    invalid = pure (Left (logDir </> name ++ ": invalid V2 checkpoint actions"))
    supportsV2Checkpoint protocol =
      pMinReaderVersion protocol == 3
        && pMinWriterVersion protocol == 7
        && "v2Checkpoint" `elem` pReaderFeatures protocol
        && "v2Checkpoint" `elem` pWriterFeatures protocol


-- The manifest totals can detect a missing sidecar reference even when
-- all of the referenced Parquet files decode successfully.
checkCheckpointTag :: Maybe (Map.Map Text Text) -> Text -> Integer -> Either String ()
checkCheckpointTag tags name actual = case tags >>= Map.lookup name of
  Nothing -> Right ()
  Just value
    | not (T.null value)
    , let digits = T.dropWhile (== '0') value
    , T.compareLength digits 19 /= GT
    , Just expected <- if T.null digits then Just 0 else parsePaddedVersion (T.length digits) (T.unpack digits) ->
        unless (toInteger expected == actual) (Left ("checkpointMetadata.tags." ++ T.unpack name ++ " mismatch"))
    | otherwise -> Left ("invalid checkpointMetadata.tags." ++ T.unpack name)


data CheckpointRows = CheckpointRows !Checkpoint.CheckpointSnapshot !(Maybe (Map.Map Text Text)) !Bool ![SidecarAction]


applyCheckpointAddRows :: CheckpointRows -> Checkpoint.CheckpointAddBatch -> Either String CheckpointRows
applyCheckpointAddRows (CheckpointRows snapshot metadata _ sidecars) adds = do
  updated <- Checkpoint.applyCheckpointAddBatch snapshot adds
  Right (CheckpointRows updated metadata True sidecars)


applyCheckpointRow :: Word64 -> CheckpointRows -> DeltaAction -> Either String CheckpointRows
applyCheckpointRow version (CheckpointRows snapshot metadata inline sidecars) action = case action of
  ActionCheckpointMetadataWithTags v tags
    | not (isJust metadata) && v == version -> Right (CheckpointRows snapshot (Just tags) inline sidecars)
    | otherwise -> invalid
  ActionSidecar sidecar -> Right (CheckpointRows snapshot metadata inline (sidecar : sidecars))
  ActionOther name | name == "sidecar" || name == "checkpointMetadata" -> invalid
  _ -> do
    updated <- Checkpoint.applyCheckpointAction snapshot action
    Right (CheckpointRows updated metadata (inline || isFileAction action) sidecars)
  where
    invalid = Left "invalid V2 checkpoint actions"


checkpointResult :: FilePath -> Either String a -> Either String a
checkpointResult path = either (Left . ((path ++ ": ") ++)) Right
{-# INLINE checkpointResult #-}


isV2Action :: DeltaAction -> Bool
isV2Action (ActionCheckpointMetadata _) = True
isV2Action (ActionSidecar _) = True
isV2Action (ActionOther name) = name == "sidecar" || name == "checkpointMetadata"
isV2Action _ = False


isFileAction :: DeltaAction -> Bool
isFileAction (ActionAdd _) = True
isFileAction (ActionRemove _) = True
isFileAction _ = False


loadSidecars :: FilePath -> Checkpoint.CheckpointSnapshot -> [SidecarAction] -> IO (Either String Checkpoint.CheckpointSnapshot)
loadSidecars _ snapshot [] = pure (Right snapshot)
loadSidecars logDir snapshot sidecars = do
  parent <- try @IOException (canonicalizePath logDir)
  case parent of
    Left err -> pure (Left (show err))
    Right base -> go (base </> "_sidecars") Set.empty snapshot sidecars
  where
    go _ _ current [] = pure (Right current)
    go directory seen current (sidecar : rest) =
      case dataFilePath directory (sidecarPath sidecar) of
        Left err -> pure (Left (show (sidecarPath sidecar) ++ ": " ++ err))
        Right path -> do
          input <- try @IOException $ do
            case splitExtension (takeFileName path) of
              (_ : _, ".parquet") -> pure ()
              _ -> ioError (userError "expected a .parquet sidecar filename")
            let parent = takeDirectory path
            -- A bare filename already has the expected parent. Check
            -- the complete path below for symlinks.
            unless (parent == directory) $ do
              resolved <- canonicalizePath parent
              unless (resolved == directory) $
                ioError (userError "sidecar must reside in _delta_log/_sidecars")
            actual <- canonicalizePath path
            unless (takeDirectory actual == directory) $
              ioError (userError "sidecar must reside in _delta_log/_sidecars")
            when (Set.member actual seen) $
              ioError (userError "duplicate sidecar path")
            bytes <- withBinaryFile actual ReadMode $ \handle -> do
              let expected = toInteger (sidecarSize sidecar)
                  wrongSize = ioError (userError "sidecar size mismatch")
              size <- hFileSize handle
              unless (size == expected) wrongSize
              when (size > toInteger (maxBound :: Int)) $
                ioError (userError "sidecar size exceeds addressable memory")
              -- Cap the read even if the file grows after the size check.
              bytes <- BS.hGet handle (fromInteger size)
              end <- BS.hGet handle 1
              unless (toInteger (BS.length bytes) == expected && BS.null end) wrongSize
              pure bytes
            pure (actual, bytes)
          case input of
            Left err -> pure (Left (path ++ ": " ++ show err))
            Right (actual, bytes) ->
              case checkpointResult path (Checkpoint.foldCheckpointFileWithAddBatches Checkpoint.applyCheckpointAddBatch applySidecarRow current bytes) of
                Left err -> pure (Left err)
                Right next -> go directory (Set.insert actual seen) next rest

    applySidecarRow current action
      | isFileAction action = Checkpoint.applyCheckpointAction current action
      | ActionOther _ <- action, not (isV2Action action) = Right current
      | otherwise = Left "sidecar contains non-file actions"


-- ============================================================
-- Snapshot helpers
-- ============================================================

{- | The active file paths as stored in the log, in ascending path order.
These can be URI-encoded relative paths or absolute locations.
Use @Delta.Scan.openDataFile@ to resolve and read a local file.
-}
activeFilePaths :: DeltaTable -> [Text]
activeFilePaths = map addPath . Map.elems . tsFiles . dtSnapshot


{- | Like 'activeFilePaths' but each entry carries the typed
'AddAction' (so callers can pull stats / partition values /
size off the same record).

Distinct from "Delta.Log"'s 'activeFiles' which folds a raw
@[DeltaAction]@ stream — this one consumes the already-folded
'DeltaTable'.
-}
dtActiveFiles :: DeltaTable -> [AddAction]
dtActiveFiles = Map.elems . tsFiles . dtSnapshot


{- | Group the active file set by its partition tuple. Each
key is the partition values map (a column \(\to\) value
assignment); each value is the list of 'AddAction's whose
@partitionValues@ match the key.

This is the entry point a scan planner uses to dispatch
per-partition reads to the right object-store prefixes.
-}
partitionedActiveFiles
  :: DeltaTable
  -> Map.Map (Map.Map Text (Maybe Text)) [AddAction]
partitionedActiveFiles dt =
  Map.foldl' group Map.empty (tsFiles (dtSnapshot dt))
  where
    group partitions file = Map.insertWith (++) (addPartitionValues file) [file] partitions


-- ============================================================
-- History
-- ============================================================

inCommitTimestampsEnabled :: TableSnapshot -> Bool
inCommitTimestampsEnabled snapshot =
  maybe False (\p -> pMinWriterVersion p == 7 && "inCommitTimestamp" `elem` pWriterFeatures p) (tsProtocol snapshot)
    && snapshotProperty "delta.enableInCommitTimestamps" snapshot == Just "true"


-- History and time travel use the same recovered clock as snapshot
-- replay. Reuse the selected snapshot if it already covers this version.
recoveredCommitTimestamp
  :: DeltaTable
  -> (Word64 -> IO (Either String DeltaTable))
  -> Word64
  -> IO (Maybe Int64)
recoveredCommitTimestamp table openAt version
  | dtVersion table == Just version = pure (timestamp table)
  | otherwise = either (const Nothing) timestamp <$> openAt version
  where
    timestamp atVersion
      | inCommitTimestampsEnabled snapshot = tsLastCommit snapshot >>= ciInCommitTimestamp
      | otherwise = Nothing
      where
        snapshot = dtSnapshot atVersion


snapshotProperty :: Text -> TableSnapshot -> Maybe Text
snapshotProperty name snapshot = tsMetaData snapshot >>= Map.lookup name . mdConfiguration


-- A table created with the feature already enabled has no cutover keys.
timestampCutover :: TableSnapshot -> Either String (Maybe (Word64, Int64))
timestampCutover snapshot
  | not (inCommitTimestampsEnabled snapshot) = Right Nothing
  | otherwise = case (snapshotProperty versionKey snapshot, snapshotProperty timestampKey snapshot) of
      (Nothing, Nothing) -> Right Nothing
      (Just version, Just stamp)
        | Just number <- parsePaddedVersion (T.length version) (T.unpack version)
        , Right (millis, rest) <- TR.signed TR.decimal stamp
        , T.null rest
        , millis >= toInteger (minBound :: Int64), millis <= toInteger (maxBound :: Int64) ->
            Right (Just (number, fromInteger millis))
      _ -> Left "Delta.IO: invalid in-commit timestamp enablement version or timestamp"
  where
    versionKey = "delta.inCommitTimestampEnablementVersion"
    timestampKey = "delta.inCommitTimestampEnablementTimestamp"


-- Search only the side of the cutover relevant to the request. Legacy
-- timestamps can be read without opening the JSON, and monotonicity
-- means the scan stops as soon as it brackets the requested time.
versionAtTimestamp
  :: Integer
  -> Word64
  -> DeltaTable
  -> (Word64 -> IO (Either String DeltaTable))
  -> IO (Either String Word64)
versionAtTimestamp requested newest table replayAt = case timestampCutover snapshot of
  Left err -> pure (Left err)
  Right (Just (version, _)) | version > newest -> pure (Left "Delta.IO: in-commit timestamp enablement is beyond the latest version")
  Right (Just (version, timestamp))
    | requested < toInteger timestamp -> go False (Just version) Nothing commits
    | otherwise -> searchInCommit (dropWhile ((< version) . fst) commits)
  Right Nothing
    | inCommitTimestampsEnabled snapshot -> searchInCommit commits
    | otherwise -> go False Nothing Nothing commits
  where
    snapshot = dtSnapshot table
    commits = dtCommits table
    earlier = Left "Delta.IO: requested timestamp is before the available commit history"
    later = Left "Delta.IO: requested timestamp is after the latest commit"
    missing = Left "Delta.IO: missing commit timestamps in the requested range"

    -- In-commit timestamps are already monotonic. Search the numbered
    -- file index, and fall back to the sequential reader if timestamps
    -- at the probed versions cannot establish a sound interval.
    searchInCommit candidates = case V.fromList candidates of
      indexed | V.null indexed -> pure earlier
      indexed -> do
        let lastIndex = V.length indexed - 1
        first <- stampAt indexed 0
        lastStamp <- if lastIndex == 0 then pure first else stampAt indexed lastIndex
        case (first, lastStamp) of
          (Just low@(_, lower), Just high@(version, upper))
            | lastIndex == 0 || lower < upper ->
                if requested < toInteger lower then pure earlier
                else if requested > toInteger upper then pure (if version == newest then later else missing)
                else bisect indexed 0 low lastIndex high
          _ -> sequential
      where
        sequential = go True Nothing Nothing candidates
        stampAt indexed index = do
          let (version, path) = V.unsafeIndex indexed index
          result <- try @IOException (readFirstCommitTimestamp version path)
          pure ((version,) <$> either (const Nothing) id result)
        bisect indexed low lower@(lowVersion, lowStamp) high upper@(highVersion, highStamp)
          | requested == toInteger highStamp = pure (Right highVersion)
          | requested == toInteger lowStamp = pure (Right lowVersion)
          | high - low == 1 = pure (if lowVersion + 1 == highVersion then Right lowVersion else missing)
          | otherwise = do
              let mid = low + (high - low) `quot` 2
              point <- stampAt indexed mid
              case point of
                Just current@(_, stamp)
                  | lowStamp < stamp && stamp < highStamp ->
                      if requested < toInteger stamp
                        then bisect indexed low lower mid current
                        else bisect indexed mid current high upper
                _ -> sequential

    go ict stop previous remaining = case remaining of
      (version, path) : rest | maybe True (version <) stop -> do
        exists <- doesFileExist path
        if not exists then go ict stop previous rest else do
          result <- try @IOException $
            if ict then readFirstCommitTimestamp version path else Just <$> modificationMillis path
          case result of
            Left err -> do
              absent <- missingLogFile path err
              if absent then go ict stop previous rest else pure (Left (show err))
            Right Nothing -> pure (Left ("Delta.IO: missing inCommitTimestamp at version " ++ show version))
            Right (Just raw)
              | ict, Just (_, priorStamp) <- previous, raw <= priorStamp ->
                  pure (Left ("Delta.IO: non-increasing inCommitTimestamp at version " ++ show version))
              | otherwise -> do
                let stamp = if ict then raw else monotonicMillis (snd <$> previous) raw
                case compare requested (toInteger stamp) of
                  EQ -> pure (Right version)
                  LT -> pure $ case previous of
                    Nothing -> earlier
                    Just (prior, _) | prior + 1 == version -> Right prior
                    _ -> missing
                  GT -> go ict stop (Just (version, stamp)) rest
      _ -> pure $ case previous of
        Nothing -> earlier
        Just (version, _)
          | Just firstICT <- stop, version + 1 == firstICT -> Right version
          | version == newest -> later
          | otherwise -> missing

    readFirstCommitTimestamp version path = do
      timestamp <- withBinaryFile path ReadMode $ \handle -> do
        empty <- hIsEOF handle
        if empty then pure Nothing else (>>= ciInCommitTimestamp) . parseCommitInfo <$> BSC.hGetLine handle
      case timestamp of
        Just _ -> pure timestamp
        Nothing
          | version == newest -> recoveredCommitTimestamp table replayAt version
          | otherwise -> do
              bytes <- BS.readFile path
              case truncatedCommitPrefix bytes of
                Nothing -> pure Nothing
                Just _ -> recoveredCommitTimestamp table replayAt version
    {-# INLINE readFirstCommitTimestamp #-}


modificationMillis :: FilePath -> IO Int64
modificationMillis path = floor . (* 1000) . utcTimeToPOSIXSeconds <$> getModificationTime path


monotonicMillis :: Maybe Int64 -> Int64 -> Int64
monotonicMillis previous modified = maybe modified (max modified . afterPrevious) previous
  where
    afterPrevious t = if t == maxBound then t else t + 1


{- | One row of @DESCRIBE HISTORY@. Each commit emits a
@commitInfo@ action — these are the per-row records you'd
see in @deltalake.DeltaTable.history()@.
-}
data HistoryEntry = HistoryEntry
  { heVersion :: !Word64
  , heCommitFile :: !FilePath
  , heTimestamp :: !(Maybe Int64)
  , heOperation :: !(Maybe Text)
  , heIsBlindAppend :: !(Maybe Bool)
  , heIsolationLevel :: !(Maybe Text)
  , heAppId :: !(Maybe Text)
  , heAppVersion :: !(Maybe Int64)
  }
  deriving (Show, Eq)


{- | Walk the table's commit log and emit a 'HistoryEntry' per
version, newest-first (matching delta-rs's
@DeltaTable.history()@ order). Each commit's @commitInfo@
action supplies operation and isolation level. Timestamps come from
@inCommitTimestamp@ when enabled for that version and from the file's
modification time otherwise. Legacy timestamps increase with commit
version even when filesystem times tie or run backwards.

Note: we only walk the on-disk JSON commits; checkpoints
don't carry @commitInfo@ rows. For a Delta table whose
earliest commit was vacuumed below the @_last_checkpoint@
pointer, those early entries are gone — same behaviour as
delta-rs. Commits removed after listing are skipped as well.
-}
historyEntries :: DeltaTable -> IO [HistoryEntry]
historyEntries dt = go Nothing [] (dtCommits dt)
  where
    snapshot = dtSnapshot dt
    timestampFeature = inCommitTimestampsEnabled snapshot
    enablementVersion = fromMaybe 0 $ do
      textVersion <- snapshotProperty "delta.inCommitTimestampEnablementVersion" snapshot
      parsePaddedVersion (T.length textVersion) (T.unpack textVersion)

    go _ entries [] = pure entries
    go previous entries ((v, fp) : rest) = do
      result <- try @IOException (readEntry previous v fp)
      case result of
        Right entry -> go (heTimestamp entry) (entry : entries) rest
        Left err -> do
          absent <- missingLogFile fp err
          if absent then go previous entries rest else throwIO err

    readEntry previous v fp = do
      bytes <- BS.readFile fp
      let CommitHistoryActions mci mtxn = parseCommitHistory bytes
      timestamp <-
        if timestampFeature && v >= enablementVersion
          then case mci >>= ciInCommitTimestamp of
            Just stamp -> pure (Just stamp)
            Nothing -> case truncatedCommitPrefix bytes of
              Nothing -> pure Nothing
              Just _ -> recoveredCommitTimestamp dt (openDeltaTableAt (dtRoot dt)) v
          else Just . monotonicMillis previous <$> modificationMillis fp
      pure $!
        HistoryEntry
          { heVersion = v
          , heCommitFile = fp
          , heTimestamp = timestamp
          , heOperation = ciOperation =<< mci
          , heIsBlindAppend = ciIsBlindAppend =<< mci
          , heIsolationLevel = ciIsolationLevel =<< mci
          , heAppId = txnAppId <$> mtxn
          , heAppVersion = txnVersion <$> mtxn
          }
