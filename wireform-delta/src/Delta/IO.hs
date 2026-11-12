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

  -- * Snapshot helpers
  activeFilePaths,
  dtActiveFiles,
  partitionedActiveFiles,

  -- * History
  HistoryEntry (..),
  historyEntries,

  -- * Re-exports
  module Delta.Log,
) where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.List (sort, sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Word (Word64)
import Delta.Checkpoint qualified as Checkpoint
import Delta.Log
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, takeFileName, (</>))
import Text.Read (readMaybe)


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
findLastCheckpoint tableRoot = do
  let p = tableRoot </> "_delta_log" </> "_last_checkpoint"
  ok <- doesFileExist p
  if not ok
    then pure Nothing
    else do
      res <- try (BS.readFile p) :: IO (Either SomeException BS.ByteString)
      case res of
        Left _ -> pure Nothing
        Right bs -> pure (parseLastCheckpoint bs)


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
      let candidates = mapMaybe (toCommit logDir) entries
      pure (sort candidates)
  where
    toCommit dir e = do
      stem <- stripJson e
      v <- readMaybe stem :: Maybe Word64
      Just (v, dir </> e)
    stripJson e
      | takeExtension e == ".json"
      , length (takeBaseName e) == 20 -- the canonical 20-digit zero-pad
        =
          Just (takeBaseName e)
      | otherwise = Nothing


{- | List every entry in @<root>/_delta_log/@ along with its
classification: @"commit"@, @"checkpoint"@, or
@"unknown"@. Useful for diagnostics.
-}
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
    classify e
      | takeFileName e == "_last_checkpoint" = "last_checkpoint"
      | takeExtension e == ".json" = "commit"
      | ".checkpoint.parquet" `endsWith` e = "checkpoint"
      | otherwise = "unknown"
    endsWith suffix s =
      length suffix <= length s
        && drop (length s - length suffix) s == suffix


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
  {- ^ Version of the most-recent checkpoint Parquet file
  that's actually on disk. May differ from
  'lcVersion' (e.g. when the pointer file was deleted) and
  equal 'Nothing' even when 'dtLastCheckpoint' is 'Just'
  (the parquet file was vacuumed but the pointer wasn't
  updated).
  -}
  , dtCommits :: ![(Word64, FilePath)]
  {- ^ Every numbered @NNNN.json@ commit file found under
  @_delta_log/@, in chronological order. The replay walks
  /every/ commit; checkpoint-aware fast-replay is a
  follow-up.
  -}
  , dtSnapshot :: !TableSnapshot
  -- ^ Folded snapshot at the latest version.
  , dtVersion :: !(Maybe Word64)
  {- ^ Highest commit version observed, or 'Nothing' for an
  empty log.
  -}
  }
  deriving (Show, Eq)


{- | Open a Delta table from a directory.

If a @*.checkpoint.parquet@ file is present at version /K/,
read it via 'Delta.Checkpoint.decodeCheckpointFile' to seed
the snapshot at version /K/, then replay every later
@NNNN.json@ commit (versions > /K/) on top. Otherwise fall
back to a full JSON replay from version 0.

The checkpoint short-circuit turns table-open from O(N) in
commit count to O(N − checkpoint_version), which is the
whole point of the checkpoint format.

The path-aware checkpoint reader reconstructs file actions, metadata,
protocol, and transaction identifiers. Domain metadata and sidecar rows
are surfaced as 'ActionOther'.
-}
openDeltaTable :: FilePath -> IO (Either String DeltaTable)
openDeltaTable tableRoot = do
  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok
    then pure (Left ("Delta.IO: missing " ++ logDir))
    else do
      lc <- findLastCheckpoint tableRoot
      ckptOnDisk <- findCheckpointParquet tableRoot
      commits <- findCommits tableRoot
      let (priorCommits, jsonCommits) = case ckptOnDisk of
            Just v -> partitionAtVersion v commits
            Nothing -> ([], commits)
      ckptActions <- case ckptOnDisk of
        Just v -> readCheckpoint logDir v
        Nothing -> pure []
      jsonActions <- concat <$> mapM readActions jsonCommits
      let !replayedActions = ckptActions ++ jsonActions
          !snap = snapshotFromActions replayedActions
          !ver = case commits of
            [] -> Nothing
            xs -> Just (fst (last xs))
          _ = priorCommits -- kept for potential future use; silence -Wunused
      pure $
        Right
          DeltaTable
            { dtRoot = tableRoot
            , dtLastCheckpoint = lc
            , dtCheckpointAvailable = ckptOnDisk
            , dtCommits = commits
            , dtSnapshot = snap
            , dtVersion = ver
            }


{- | Split commit list into (≤ckptVersion, >ckptVersion). The
former is consumed by the checkpoint reader, the latter is
replayed via JSON on top.
-}
partitionAtVersion
  :: Word64
  -> [(Word64, FilePath)]
  -> ([(Word64, FilePath)], [(Word64, FilePath)])
partitionAtVersion v = span ((<= v) . fst)


{- | Decode the @<logDir>/NNNN.checkpoint.parquet@ file at
version @v@ via 'Delta.Checkpoint.readCheckpointFile'. On
failure we silently fall back to an empty action list so
'openDeltaTable' degrades to a full JSON replay rather than
erroring out.
-}
readCheckpoint :: FilePath -> Word64 -> IO [DeltaAction]
readCheckpoint logDir v = do
  let path = logDir </> formatCheckpointName v
  res <- Checkpoint.readCheckpointFile path
  case res of
    Right acts -> pure acts
    Left _ -> pure []


formatCheckpointName :: Word64 -> FilePath
formatCheckpointName n =
  replicate (20 - length (show n)) '0'
    ++ show n
    ++ ".checkpoint.parquet"


readActions :: (Word64, FilePath) -> IO [DeltaAction]
readActions (_, fp) = do
  bs <- BL.readFile fp
  pure (parseLogFile bs)


{- | Find the highest-version checkpoint Parquet that exists on
disk (regardless of what @_last_checkpoint@ claims). Returns
'Nothing' if no @*.checkpoint.parquet@ files are present.
-}
findCheckpointParquet :: FilePath -> IO (Maybe Word64)
findCheckpointParquet tableRoot = do
  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok
    then pure Nothing
    else do
      entries <- listDirectory logDir
      let versions = mapMaybe parseCheckpointName entries
      pure $ case versions of
        [] -> Nothing
        vs -> Just (maximum vs)
  where
    parseCheckpointName e
      | length stem == 20
      , Just v <- readMaybe stem :: Maybe Word64
      , rest == ".checkpoint.parquet" =
          Just v
      | otherwise = Nothing
      where
        (stem, rest) = splitAt 20 e


-- ============================================================
-- Time travel
-- ============================================================

{- | Open a Delta table /at a specific version/. Identical to
'openDeltaTable' except the JSON-tail replay stops as soon
as it has consumed the requested version, and the resulting
'dtSnapshot' / 'dtVersion' reflect the table state at that
version.

If the requested version is at or below the most recent
checkpoint, the JSON replay is skipped entirely and the
snapshot comes purely from the checkpoint Parquet (or, on
failure, from a JSON-only walk up to that version).

Returns @Left@ when the requested version is greater than
the highest version on disk.
-}
openDeltaTableAt
  :: FilePath
  -> Word64
  -- ^ version to open
  -> IO (Either String DeltaTable)
openDeltaTableAt tableRoot atVersion = do
  let logDir = tableRoot </> "_delta_log"
  ok <- doesDirectoryExist logDir
  if not ok
    then pure (Left ("Delta.IO: missing " ++ logDir))
    else do
      lc <- findLastCheckpoint tableRoot
      ckptOnDisk <- findCheckpointParquet tableRoot
      commits <- findCommits tableRoot
      let highestVersion = case commits of
            [] -> Nothing
            xs -> Just (fst (last xs))
      case highestVersion of
        Just hv
          | atVersion > hv ->
              pure
                ( Left
                    ( "Delta.IO: version "
                        ++ show atVersion
                        ++ " > highest committed version "
                        ++ show hv
                    )
                )
        _ -> do
          let useCkpt = case ckptOnDisk of
                Just v -> v <= atVersion
                Nothing -> False
              jsonCommits =
                filter
                  ( \(v, _) ->
                      v <= atVersion
                        && case ckptOnDisk of
                          Just cv | useCkpt -> v > cv
                          _ -> True
                  )
                  commits
          ckptActions <- case (ckptOnDisk, useCkpt) of
            (Just v, True) -> readCheckpoint logDir v
            _ -> pure []
          jsonActions <- concat <$> mapM readActions jsonCommits
          let !replayedActions = ckptActions ++ jsonActions
              !snap = snapshotFromActions replayedActions
          pure $
            Right
              DeltaTable
                { dtRoot = tableRoot
                , dtLastCheckpoint = lc
                , dtCheckpointAvailable = ckptOnDisk
                , dtCommits = filter (\(v, _) -> v <= atVersion) commits
                , dtSnapshot = snap
                , dtVersion = Just atVersion
                }


-- ============================================================
-- Snapshot helpers
-- ============================================================

{- | Every relative file path the snapshot considers active, in
ascending path order. Production scan planners join these
against the table root to read the underlying Parquet files.
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
  let !files = Map.elems (tsFiles (dtSnapshot dt))
  in Map.fromListWith (++) [(addPartitionValues a, [a]) | a <- files]


-- ============================================================
-- History
-- ============================================================

{- | One row of @DESCRIBE HISTORY@. Each commit emits a
@commitInfo@ action — these are the per-row records you'd
see in @deltalake.DeltaTable.history()@.
-}
data HistoryEntry = HistoryEntry
  { heVersion :: !Word64
  , heCommitFile :: !FilePath
  , heTimestamp :: !(Maybe Word64)
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
action sources the timestamp / operation / isolation level;
if a commit has no @commitInfo@ the entry still appears
with those fields as 'Nothing'.

Note: we only walk the on-disk JSON commits; checkpoints
don't carry @commitInfo@ rows. For a Delta table whose
earliest commit was vacuumed below the @_last_checkpoint@
pointer, those early entries are gone — same behaviour as
delta-rs.
-}
historyEntries :: DeltaTable -> IO [HistoryEntry]
historyEntries dt = do
  rows <- mapM oneCommit (dtCommits dt)
  pure $ sortBy (comparing (Down . heVersion)) rows
  where
    oneCommit (v, fp) = do
      bs <- BL.readFile fp
      let acts = parseLogFile bs
          mci = firstCommitInfo acts
          mtxn = firstTxn acts
      pure
        HistoryEntry
          { heVersion = v
          , heCommitFile = fp
          , heTimestamp = ciTimestamp =<< mci
          , heOperation = ciOperation =<< mci
          , heIsBlindAppend = ciIsBlindAppend =<< mci
          , heIsolationLevel = ciIsolationLevel =<< mci
          , heAppId = txnAppId <$> mtxn
          , heAppVersion = txnVersion <$> mtxn
          }

    firstCommitInfo [] = Nothing
    firstCommitInfo (ActionCommitInfo c : _) = Just c
    firstCommitInfo (_ : xs) = firstCommitInfo xs

    firstTxn [] = Nothing
    firstTxn (ActionTxn t : _) = Just t
    firstTxn (_ : xs) = firstTxn xs


-- The 'Down' import is used inside 'historyEntries'; pin it so
-- the unused-import linter stays quiet on partial builds.
_unused :: Down Int
_unused = Down 0
