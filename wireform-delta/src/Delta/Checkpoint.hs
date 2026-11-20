{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Decoder for Delta Lake @*.checkpoint.parquet@ files.

A Delta checkpoint Parquet file folds the entire active log
of a table into a single columnar snapshot. Each row carries
exactly one of the action variants (@add@ / @remove@ /
@metaData@ / @protocol@ / @txn@ / …) under a top-level
struct column whose name matches the action; all other
action structs on that row are null. By reading these
columns, consumers can reconstruct a 'Delta.Log.TableSnapshot'
without walking the @NNNN.json@ commit files from version 0.

This module decodes @add@, @remove@, @metaData@, @protocol@,
@txn@, and the V2 checkpoint metadata and sidecar references.

/Implementation notes:/

  * The Parquet schema has duplicate leaf names across struct
    parents (@add.path@, @remove.path@, @sidecar.path@, …).
    We resolve each leaf by its full schema path and check that
    each row group's chunk path and physical type agree with the
    leaf at the same ordinal.
  * Per-leaf reading uses Parquet's optional and level-aware
    column readers, which handle
    PLAIN, PLAIN_DICTIONARY and RLE_DICTIONARY in a single
    pass. Delta checkpoints written by delta-rs / Spark are
    dictionary-encoded for the high-cardinality string
    columns and PLAIN for the low-cardinality bool / int
    ones; both shapes thread through the generic reader.
  * For each row we look at the @add.path@ / @remove.path@ /
    @metaData.id@ / @protocol.minReaderVersion@ leaves: if
    the row's value is 'Just _' the corresponding struct is
    present, so we attribute the row to that variant.
-}
module Delta.Checkpoint (
  -- * Decode
  decodeCheckpointFile,
  readCheckpointFile,
  foldCheckpointFile,
  readCheckpointFold,
  foldCheckpointFileWithAdds,
  readCheckpointFoldWithAdds,
  foldCheckpointFileWithAddBatches,
  readCheckpointFoldWithAddBatches,

  -- * Replay
  checkpointToActions,
  snapshotFromCheckpoint,
  CheckpointSnapshot,
  emptyCheckpointSnapshot,
  applyCheckpointAdd,
  applyCheckpointAddBatch,
  applyCheckpointAdds,
  CheckpointAddBatch,
  applyCheckpointAction,
  checkpointProtocol,
  checkpointAddCount,
  checkpointFileActionCount,
  finishCheckpointSnapshot,
  finishCheckpointForReplay,
  finishCheckpointForReplayWithMetrics,
) where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (bit, popCount, shiftL, unsafeShiftL, unsafeShiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Short.Internal qualified as SBSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Internal qualified as MapInternal
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Array qualified as TA
import Data.Text.Internal qualified as TI
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as Intro
import Data.Vector.Algorithms.Radix qualified as Radix
import Data.Vector.Algorithms.Tim qualified as Tim
import Data.Vector.Generic qualified as VG
import Data.Vector.Mutable qualified as VM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Vector.Storable qualified as VS
import Data.Word (Word64)
import Delta.CheckpointStats qualified as Stats
import Delta.DeletionVector (DeletionVectorIdentity, deletionVectorIdentity, validDeletionVectorFields)
import Delta.Log (
  AddAction (..),
  AddStats,
  DeltaAction (..),
  DomainMetadataAction (..),
  MetaDataAction (..),
  ProtocolAction,
  RemoveAction (..),
  SidecarAction (..),
  SnapshotFileMetrics (..),
  TableSnapshot (..),
  TxnAction (..),
  applyMetadataAction,
  cacheFileMetrics,
  emptySnapshot,
  normalizeExtendedFileMetadata,
  normalizePartitionValue,
  protocolFromFeatureLists,
  restoreStoredFiles,
  setReplayFiles,
  updateFileMetrics,
 )
import Delta.ParquetSchema (ColumnLevels (..), schemaLeaves, validateChunks)
import Delta.Path (filePathKey)
import GHC.Exts (Int (I#), Int#)
import Parquet.Levels qualified as PL
import Parquet.Read qualified as PR
import Parquet.Types qualified as P


-- ============================================================
-- Decode
-- ============================================================

-- | Decode the actions in a Delta Parquet checkpoint in file order.
decodeCheckpointFile :: ByteString -> Either String [DeltaAction]
decodeCheckpointFile bs = reverse <$> foldCheckpointFile (\actions action -> Right (action : actions)) [] bs


{- | Decode and consume one row group at a time. A failed row or group
aborts the fold without exposing a partially replayed snapshot.
-}
foldCheckpointFile :: (a -> DeltaAction -> Either String a) -> a -> ByteString -> Either String a
foldCheckpointFile step = foldCheckpointFileWithAdds (\acc add -> step acc (ActionAdd add)) step
{-# INLINE foldCheckpointFile #-}


-- | Fold in file order, supplying a separate callback for 'ActionAdd'
-- so snapshot replay can consume file descriptors directly.
foldCheckpointFileWithAdds
  :: (a -> AddAction -> Either String a)
  -> (a -> DeltaAction -> Either String a)
  -> a
  -> ByteString
  -> Either String a
foldCheckpointFileWithAdds stepAdd step = foldCheckpointGroups consume
  where
    consume initial n _ rowAt = go 0 initial
      where
        go !i !acc
          | i == n = Right acc
          | otherwise = case rowAt (rowIndex i) of
              (# add | | #) -> stepAdd acc add >>= go (i + 1)
              (# | action | #) -> step acc action >>= go (i + 1)
              (# | | err #) -> Left err


{- | Accumulate a row group's additions at once during snapshot replay.
The non-add handler runs in row order and must be independent of the adds.
-}
foldCheckpointFileWithAddBatches
  :: (a -> CheckpointAddBatch -> Either String a)
  -> (a -> DeltaAction -> Either String a)
  -> a
  -> ByteString
  -> Either String a
foldCheckpointFileWithAddBatches stepAdds step = foldCheckpointGroups consume
  where
    consume initial n addCount rowAt = runST $ do
      -- n counts all actions; addCount counts the defined add.path cells.
      -- The row reader yields one add per defined cell or fails the group.
      adds <- VM.unsafeNew addCount
      let go !i !count !acc
            | i == n =
                if count == 0
                  then pure (Right acc)
                  else do
                    batch <- V.unsafeFreeze adds
                    pure (stepAdds acc (CheckpointAddBatch batch))
            | otherwise = case rowAt (rowIndex i) of
                (# add | | #) -> case addDeletionVector add of
                  Nothing -> append add
                  Just dv -> case deletionVectorIdentity dv of
                    Left err -> pure (Left err)
                    Right _ -> append add
                (# | action | #) ->
                  case step acc action of
                    Left err -> pure (Left err)
                    Right next -> go (i + 1) count next
                (# | | err #) -> pure (Left err)
            where
              append add = VM.unsafeWrite adds count add >> go (i + 1) (count + 1) acc
      go 0 0 initial


rowIndex :: Int -> Int#
rowIndex (I# index) = index
{-# INLINE rowIndex #-}


type CheckpointRow = (# AddAction | DeltaAction | String #)


foldCheckpointGroups
  :: (a -> Int -> Int -> (Int# -> CheckpointRow) -> Either String a)
  -> a
  -> ByteString
  -> Either String a
foldCheckpointGroups consume initial bs = do
  pf <- PR.loadParquetFile bs
  let footer = PR.pfFooter pf
      rgs = P.fmRowGroups footer
  unless (V.foldl' (\n group -> n + toInteger (P.rgNumRows group)) 0 rgs == toInteger (P.fmNumRows footer)) $
    Left "Delta.Checkpoint: row-group counts disagree with footer"
  leaves <- either (Left . ("Delta.Checkpoint: " ++)) Right (schemaLeaves (P.fmSchema footer))
  validateActionNames (P.fmSchema footer)
  let columns = Map.fromList (V.toList (V.imap (\i (path, levels) -> (path, CheckpointColumn i levels)) leaves))
  unless (Map.size columns == V.length leaves) $
    Left "Delta.Checkpoint: duplicate column path in schema"
  let ~projection = Stats.statsProjection pf
      ~collections = checkpointCollections pf
      ~statsLeaves = V.ifoldr collectStatsLeaf [] leaves
      ~unfamiliarColumns = unknownActionColumns columns leaves
      checkChunks = validateChunks (P.fmSchema footer) leaves
      go !groupIndex !acc
        | groupIndex == V.length rgs = Right acc
        | otherwise = do
            let group = V.unsafeIndex rgs groupIndex
            either (Left . ("Delta.Checkpoint: " ++)) Right (checkChunks (P.rgColumns group))
            next <- foldRowGroup (consume acc) pf projection collections statsLeaves unfamiliarColumns
              (CheckpointGroup groupIndex group columns)
            go (groupIndex + 1) next
  go 0 initial


collectStatsLeaf :: Int -> (V.Vector Text, ColumnLevels) -> [CheckpointColumn] -> [CheckpointColumn]
collectStatsLeaf i (path, levels) rest
  | V.length path >= 3
  , V.unsafeIndex path 0 == "add"
  , V.unsafeIndex path 1 == "stats_parsed"
  , Stats.isKnownStatistic (V.unsafeIndex path 2) = CheckpointColumn i levels : rest
  | otherwise = rest


-- The file has one top-level field per action. Duplicate group names
-- can otherwise contribute disjoint leaf paths to the same action.
validateActionNames :: V.Vector P.SchemaElement -> Either String ()
validateActionNames schema = go 1 Set.empty
  where
    go !index !names
      | index == V.length schema = Right ()
      | Set.member name names = Left ("Delta.Checkpoint: duplicate checkpoint action name " ++ show name)
      | otherwise = do
          size <- PL.subtreeSize schema index
          go (index + size) (Set.insert name names)
      where
        name = P.seName (V.unsafeIndex schema index)


{- | Convenience: read the checkpoint file off disk and decode
it. Wraps the underlying bytes-loading exception in 'Either'
so callers don't have to install their own handler.
-}
readCheckpointFile :: FilePath -> IO (Either String [DeltaAction])
readCheckpointFile = readCheckpointWith decodeCheckpointFile


readCheckpointFold :: (a -> DeltaAction -> Either String a) -> a -> FilePath -> IO (Either String a)
readCheckpointFold step initial = readCheckpointWith (foldCheckpointFile step initial)


-- | Read a checkpoint file and fold with separate handlers for additions
-- and all other actions.
readCheckpointFoldWithAdds
  :: (a -> AddAction -> Either String a)
  -> (a -> DeltaAction -> Either String a)
  -> a
  -> FilePath
  -> IO (Either String a)
readCheckpointFoldWithAdds stepAdd step initial = readCheckpointWith (foldCheckpointFileWithAdds stepAdd step initial)


readCheckpointFoldWithAddBatches
  :: (a -> CheckpointAddBatch -> Either String a)
  -> (a -> DeltaAction -> Either String a)
  -> a
  -> FilePath
  -> IO (Either String a)
readCheckpointFoldWithAddBatches stepAdds step initial = readCheckpointWith (foldCheckpointFileWithAddBatches stepAdds step initial)


readCheckpointWith :: (ByteString -> Either String a) -> FilePath -> IO (Either String a)
readCheckpointWith decode fp = do
  res <- try @IOException (BS.readFile fp)
  case res of
    Left e -> pure (Left ("Delta.Checkpoint: " ++ show e))
    Right bs -> pure (decode bs)


-- ============================================================
-- Replay
-- ============================================================

{- | Re-emit the action stream in row order. Useful when the
caller wants to feed a checkpoint into the same fold they
use for a JSON commit walk.
-}
checkpointToActions :: ByteString -> Either String [DeltaAction]
checkpointToActions = decodeCheckpointFile


-- | Replay checkpoint rows into a 'TableSnapshot' at the checkpoint version.
snapshotFromCheckpoint :: ByteString -> Either String TableSnapshot
snapshotFromCheckpoint bs = do
  snapshot <- foldCheckpointFileWithAddBatches applyCheckpointAddBatch step emptyCheckpointSnapshot bs
  finishCheckpointSnapshot snapshot
  where
    step _ (ActionSidecar _) = needsSidecar
    step _ (ActionOther "sidecar") = needsSidecar
    step snapshot action = applyCheckpointAction snapshot action
    needsSidecar = Left "Delta.Checkpoint: sidecars must be resolved by the table opener"


-- Checkpoint rows describe a reconciled snapshot. File additions are unique
-- by path, and remove tombstones describe other logical files.
data CheckpointSnapshot
  = CheckpointSnapshot
      !TableSnapshot
      !Int
      ![V.Vector AddAction]
      ![AddAction]
      !(Map.Map Text (Set.Set (Maybe DeletionVectorIdentity)))


emptyCheckpointSnapshot :: CheckpointSnapshot
emptyCheckpointSnapshot = CheckpointSnapshot emptySnapshot 0 [] [] Map.empty


checkpointProtocol :: CheckpointSnapshot -> Maybe ProtocolAction
checkpointProtocol (CheckpointSnapshot snapshot _ _ _ _) = tsProtocol snapshot


checkpointAddCount :: CheckpointSnapshot -> Int
checkpointAddCount (CheckpointSnapshot _ adds _ _ _) = adds


checkpointFileActionCount :: CheckpointSnapshot -> Integer
checkpointFileActionCount (CheckpointSnapshot _ adds _ _ removes) =
  Map.foldl' (\count identities -> count + toInteger (Set.size identities)) (toInteger adds) removes


-- | Accumulate an addition during checkpoint replay.
applyCheckpointAdd :: CheckpointSnapshot -> AddAction -> Either String CheckpointSnapshot
applyCheckpointAdd (CheckpointSnapshot snapshot count batches adds removes) add =
  let next
        | count == maxBound = Left "Delta.Checkpoint: too many add actions"
        | otherwise = Right (CheckpointSnapshot snapshot (count + 1) batches (add : adds) removes)
  in validateAddDescriptor add >> next
{-# INLINE applyCheckpointAdd #-}


-- The decoder constructs batches after validating their file actions.
newtype CheckpointAddBatch = CheckpointAddBatch (V.Vector AddAction)


applyCheckpointAddBatch :: CheckpointSnapshot -> CheckpointAddBatch -> Either String CheckpointSnapshot
applyCheckpointAddBatch (CheckpointSnapshot snapshot count batches adds removes) (CheckpointAddBatch batch)
  | V.length batch > maxBound - count = Left "Delta.Checkpoint: too many add actions"
  | otherwise = Right (CheckpointSnapshot snapshot (count + V.length batch) (batch : batches) adds removes)


-- | Add a decoded file list, checking descriptors before the batch
-- enters snapshot assembly.
applyCheckpointAdds :: CheckpointSnapshot -> V.Vector AddAction -> Either String CheckpointSnapshot
applyCheckpointAdds snapshot batch = do
  V.mapM_ validateAddDescriptor batch
  applyCheckpointAddBatch snapshot (CheckpointAddBatch batch)


validateAddDescriptor :: AddAction -> Either String ()
validateAddDescriptor add = case addDeletionVector add of
  Nothing -> Right ()
  Just descriptor -> () <$ deletionVectorIdentity descriptor
{-# INLINE validateAddDescriptor #-}


applyCheckpointAction :: CheckpointSnapshot -> DeltaAction -> Either String CheckpointSnapshot
applyCheckpointAction current@(CheckpointSnapshot snapshot count batches adds removes) action = case action of
  ActionAdd add -> applyCheckpointAdd current add
  ActionRemove remove -> do
    identity <- traverse deletionVectorIdentity (removeDeletionVector remove)
    let path = filePathKey (removePath remove)
        identities = Map.findWithDefault Set.empty path removes
    when (Set.member identity identities) (duplicate "remove key")
    Right (CheckpointSnapshot snapshot count batches adds (Map.insert path (Set.insert identity identities) removes))
  ActionProtocol _ | isJust (tsProtocol snapshot) -> duplicate "protocol"
  ActionMetaData _ | isJust (tsMetaData snapshot) -> duplicate "metaData"
  ActionTxn txn | Map.member (txnAppId txn) (tsAppIds snapshot) -> duplicate "transaction identifier"
  ActionDomainMetadata domain
    | domainRemoved domain -> Left "Delta.Checkpoint: domain tombstone in checkpoint"
    | Map.member (domainName domain) (tsDomains snapshot) -> duplicate "metadata domain"
  _ -> Right (CheckpointSnapshot (applyMetadataAction snapshot action) count batches adds removes)
  where
    duplicate name = Left ("Delta.Checkpoint: duplicate checkpoint " ++ name)


finishCheckpointSnapshot :: CheckpointSnapshot -> Either String TableSnapshot
finishCheckpointSnapshot checkpoint = restoreStoredFiles <$> finishCheckpointForReplay checkpoint


-- The table opener keeps reconciliation keys until it has applied the log tail.
finishCheckpointForReplay :: CheckpointSnapshot -> Either String TableSnapshot
finishCheckpointForReplay = finishCheckpointForReplayWithMetrics False


-- Accumulate checksum metrics from the contiguous add vector while it is
-- available. The finished file map has the same entries after rekeying.
finishCheckpointForReplayWithMetrics :: Bool -> CheckpointSnapshot -> Either String TableSnapshot
finishCheckpointForReplayWithMetrics collectMetrics (CheckpointSnapshot snapshot count batches adds removes) = do
  let sorted = sortCheckpointAdds (assembleCheckpointAdds count batches adds)
      rawFiles = checkpointFilesMap sorted
      rekey !i !files
        | i == count = Right files
        | i > 0 && addPath (V.unsafeIndex sorted (i - 1)) == path = duplicate
        | key == path = rekey (i + 1) files
        | Map.member key files = duplicate
        | otherwise = rekey (i + 1) (Map.insert key add (Map.delete path files))
        where
          add = V.unsafeIndex sorted i
          path = addPath add
          key = filePathKey path
      duplicate = Left "Delta.Checkpoint: duplicate add path"
  files <- rekey 0 rawFiles
  Map.foldrWithKey (\path identities rest -> checkOverlap files path identities >> rest) (Right ()) removes
  let result = setReplayFiles files snapshot
  pure $ if collectMetrics
    then cacheFileMetrics (V.foldl' (updateFileMetrics 1) (SnapshotFileMetrics 0 0 0 0 0) sorted) result
    else result
  where
    checkOverlap files path identities = case Map.lookup path files of
      Just add -> do
        identity <- traverse deletionVectorIdentity (addDeletionVector add)
        when (Set.member identity identities) $
          Left "Delta.Checkpoint: add and remove describe the same logical file"
      Nothing -> Right ()


-- Keep the common single-row-group case in its original vector.
assembleCheckpointAdds :: Int -> [V.Vector AddAction] -> [AddAction] -> V.Vector AddAction
assembleCheckpointAdds count [] adds = V.fromListN count adds
assembleCheckpointAdds _ [batch] [] = batch
assembleCheckpointAdds count batches adds = V.create $ do
  entries <- VM.unsafeNew count
  let copySingles !index [] = copyBatches index batches
      copySingles !index (add : rest) =
        VM.unsafeWrite entries index add >> copySingles (index + 1) rest
      copyBatches !_ [] = pure ()
      copyBatches !index (batch : rest) = do
        let size = V.length batch
        V.unsafeCopy (VM.unsafeSlice index size entries) batch
        copyBatches (index + size) rest
  copySingles 0 adds
  pure entries


-- Keep vectors already in ascending order. TimSort exploits long runs
-- in either direction; heavily shuffled paths use cached path prefixes.
sortCheckpointAdds :: V.Vector AddAction -> V.Vector AddAction
sortCheckpointAdds entries = examineOrder 0 0 1
  where
    examineOrder :: Int -> Int -> Int -> V.Vector AddAction
    examineOrder !ascBreaks !descBreaks !i
      | ascBreaks >= 32 && descBreaks >= 32 = sortUnorderedAdds entries
      | i >= V.length entries =
          if ascBreaks == 0
            then entries
            else V.modify (Tim.sortBy (comparing addPath)) entries
      | otherwise =
          let previous = addPath (V.unsafeIndex entries (i - 1))
              path = addPath (V.unsafeIndex entries i)
          in case compare previous path of
              LT -> examineOrder ascBreaks (descBreaks + 1) (i + 1)
              GT -> examineOrder (ascBreaks + 1) descBreaks (i + 1)
              EQ -> examineOrder ascBreaks descBreaks (i + 1)


-- UTF-8 byte order agrees with Text ordering. Skip up to 16 common bytes,
-- then sort on the next eight. The full comparison resolves ties, including
-- a short path whose zero padding matches bytes in a longer one.
sortUnorderedAdds :: V.Vector AddAction -> V.Vector AddAction
sortUnorderedAdds entries
  | varying == 0 = V.modify (Intro.sortBy (comparing addPath)) entries
  | otherwise = runST $ V.generateM count $ \i ->
      V.unsafeIndexM entries (VP.unsafeIndex ordered i)
  where
    count = V.length entries
    ordered = VP.modify sortIndices (VP.enumFromN 0 count)
    keys = VP.generate count (pathKey . addPath . V.unsafeIndex entries)
    -- A constant byte cannot affect the order. Visit the varying
    -- positions from least to most significant for the stable radix sort.
    varying = VP.foldl' (\mask key -> mask .|. (key `xor` firstKey)) 0 keys
    firstKey = VP.unsafeHead keys
    shifts = VP.fromList (filter (\n -> varying `unsafeShiftR` n .&. 255 /= 0) [0, 8 .. 56])

    sortIndices :: VPM.MVector s Int -> ST s ()
    sortIndices indices = do
      Radix.sortBy (VP.length shifts) 256 digit indices
      let finish !start
            | start >= VPM.length indices - 1 = pure ()
            | otherwise = do
                firstIndex <- VPM.unsafeRead indices start
                end <- tieEnd (VP.unsafeIndex keys firstIndex) (start + 1)
                when (end > start + 1) $
                  Intro.sortBy comparePath (VPM.unsafeSlice start (end - start) indices)
                finish end
          tieEnd key !i
            | i == VPM.length indices = pure i
            | otherwise = do
                index <- VPM.unsafeRead indices i
                if VP.unsafeIndex keys index == key then tieEnd key (i + 1) else pure i
      finish 0
    digit pass index =
      let offset = VP.unsafeIndex shifts pass
      in fromIntegral ((VP.unsafeIndex keys index `unsafeShiftR` offset) .&. 255)
    comparePath i j = comparing addPath (V.unsafeIndex entries i) (V.unsafeIndex entries j)

    first = addPath (V.unsafeHead entries)
    shared = V.foldl' (\prefix add -> commonBytes prefix first (addPath add)) 16 entries
    pathKey :: Text -> Word64
    pathKey (TI.Text bytes offset size) = go 0 0
      where
        go !i !key
          | i == 8 = key
          | index >= size = key `shiftL` (8 * (8 - i))
          | otherwise =
              go (i + 1) ((key `shiftL` 8) .|. fromIntegral (TA.unsafeIndex bytes (offset + index)))
          where
            index = shared + i
    commonBytes limit (TI.Text a startA lenA) (TI.Text b startB lenB) = go 0
      where
        lastByte = min limit (min lenA lenB)
        go !i
          | i == lastByte = i
          | TA.unsafeIndex a (startA + i) == TA.unsafeIndex b (startB + i) = go (i + 1)
          | otherwise = i


-- Build the balanced tree from the already ordered array, using the
-- subrange sizes directly instead of building a list of key-value pairs.
checkpointFilesMap :: V.Vector AddAction -> Map.Map Text AddAction
checkpointFilesMap entries = build 0 (V.length entries)
  where
    build !start !size
      | size == 0 = MapInternal.Tip
      | otherwise =
          let leftSize = size `quot` 2
              middle = start + leftSize
              add = V.unsafeIndex entries middle
              left = build start leftSize
              right = build (middle + 1) (size - leftSize - 1)
          in MapInternal.Bin size (addPath add) add left right


-- ============================================================
-- Internal: per-row-group walker
-- ============================================================

-- Decode the projected leaves once and let the consumer walk the rows.
foldRowGroup
  :: (Int -> Int -> (Int# -> CheckpointRow) -> Either String a)
  -> PR.ParquetFile
  -> Either String (Maybe Stats.StatsProjection)
  -> CheckpointCollections
  -> [CheckpointColumn]
  -> [(Text, V.Vector CheckpointColumn)]
  -> CheckpointGroup
  -> Either String a
foldRowGroup consume pf ~projection collections ~statsLeaves ~unfamiliarColumns rg@(CheckpointGroup groupIndex group _) = do
  let rowCount = P.rgNumRows group
  when (rowCount < 0 || toInteger rowCount > toInteger (maxBound :: Int)) $
    Left "Delta.Checkpoint: row-group count out of range"
  let n = fromIntegral rowCount
  -- 'add' columns
  addPath_ <- readAddPaths pf rg
  addSize_ <- readInt64Opt pf rg ["add", "size"]
  addModTime_ <- readInt64Opt pf rg ["add", "modificationTime"]
  addDataChg_ <- readBoolOpt pf rg ["add", "dataChange"]
  addStats_ <- readByteArrayOpt pf rg ["add", "stats"]
  let allAdds = case addPath_ of
        DenseValues _ -> True
        OneValue _ _ -> n == 1
        _ -> False
      hasAdd i = hasValue addPath_ i
      needsStats i = hasAdd i && not (hasValue addStats_ i)
  parsedStats <-
    if hasUncoveredRows addPath_ addStats_
      then projection >>= \stats -> Stats.readStats stats groupIndex allAdds (\i -> hasAdd (I# i)) needsStats
      else do
        unless allAdds (validateSkippedStatsRows pf rg n addPath_ statsLeaves)
        pure V.empty
  (addPartK_, addPartV_) <- addPartitions collections rg
  (addTagsK_, addTagsV_) <- addTagColumns collections rg
  addDv <- readDeletionVectorCols pf rg "add"
  addRowTracking <- readRowTrackingCols pf rg "add"
  -- 'remove' columns
  remPath_ <- readByteArrayOpt pf rg ["remove", "path"]
  remDelTs_ <- readInt64Opt pf rg ["remove", "deletionTimestamp"]
  remDataChg_ <- readBoolOpt pf rg ["remove", "dataChange"]
  remSize_ <- readInt64Opt pf rg ["remove", "size"]
  remExt_ <- readBoolOpt pf rg ["remove", "extendedFileMetadata"]
  (remPartK_, remPartV_, hasRemovePartitionMap) <- removePartitions collections rg remExt_
  remDv <- readDeletionVectorCols pf rg "remove"
  remRowTracking <- readRowTrackingCols pf rg "remove"
  -- 'metaData' columns
  metaId_ <- readByteArrayOpt pf rg ["metaData", "id"]
  metaName_ <- readByteArrayOpt pf rg ["metaData", "name"]
  metaDesc_ <- readByteArrayOpt pf rg ["metaData", "description"]
  metaSchema_ <- readByteArrayOpt pf rg ["metaData", "schemaString"]
  formatCols <- metadataFormat collections rg
  metaPart_ <- metadataPartitions collections rg
  (metaConfK_, metaConfV_) <- metadataConfiguration collections rg
  metaCt_ <- readInt64Opt pf rg ["metaData", "createdTime"]
  -- 'protocol' columns
  protMinR_ <- readInt32Opt pf rg ["protocol", "minReaderVersion"]
  protMinW_ <- readInt32Opt pf rg ["protocol", "minWriterVersion"]
  protRdrFt_ <- readerFeatures collections rg
  protWrtFt_ <- writerFeatures collections rg
  txnApp_ <- readByteArrayOpt pf rg ["txn", "appId"]
  txnVersion_ <- readInt64Opt pf rg ["txn", "version"]
  txnLastUpdated_ <- readInt64Opt pf rg ["txn", "lastUpdated"]
  -- 'domainMetadata' and V2 checkpoint actions
  domDom_ <- readByteArrayOpt pf rg ["domainMetadata", "domain"]
  domConfig_ <- readByteArrayOpt pf rg ["domainMetadata", "configuration"]
  domRemoved_ <- readBoolOpt pf rg ["domainMetadata", "removed"]
  sideP_ <- readByteArrayOpt pf rg ["sidecar", "path"]
  sideSize_ <- readInt64Opt pf rg ["sidecar", "sizeInBytes"]
  sideTime_ <- readInt64Opt pf rg ["sidecar", "modificationTime"]
  ckptVersion_ <- readInt64Opt pf rg ["checkpointMetadata", "version"]
  (ckptTagKeys_, ckptTagValues_) <- checkpointTags collections rg
  -- A required sibling must be defined on exactly the marker's rows.
  -- This catches an extra action's fields hidden by an absent marker.
  unless
    ( sameScalarRows addPath_ addSize_
        && sameScalarRows addPath_ addModTime_
        && sameScalarRows addPath_ addDataChg_
        && sameScalarRows remPath_ remDataChg_
        && sameScalarRows metaId_ metaSchema_
        && sameScalarRows protMinR_ protMinW_
        && sameScalarRows txnApp_ txnVersion_
        && sameScalarRows domDom_ domConfig_
        && sameScalarRows domDom_ domRemoved_
        && sameScalarRows sideP_ sideSize_
        && sameScalarRows sideP_ sideTime_
    ) (Left invalidRow)
  unless
    ( coveredBy addPath_ addStats_
        && rowTrackingCoveredBy addPath_ addRowTracking
        && deletionVectorCoveredBy addPath_ addDv
        && repeatedCoveredBy addPath_ addPartK_
        && repeatedCoveredBy addPath_ addTagsK_
        && coveredBy remPath_ remDelTs_
        && coveredBy remPath_ remSize_
        && coveredBy remPath_ remExt_
        && rowTrackingCoveredBy remPath_ remRowTracking
        && deletionVectorCoveredBy remPath_ remDv
        && repeatedCoveredBy remPath_ remPartK_
        && coveredBy metaId_ metaName_
        && coveredBy metaId_ metaDesc_
        && coveredBy metaId_ (formatProviders formatCols)
        && coveredBy metaId_ metaCt_
        && repeatedCoveredBy (formatProviders formatCols) (formatOptionKeys formatCols)
        && repeatedCoveredBy metaId_ metaPart_
        && repeatedCoveredBy metaId_ metaConfK_
        && IntMap.foldrWithKey (\i _ rest -> hasValue protMinR_ i && rest) True protRdrFt_
        && IntMap.foldrWithKey (\i _ rest -> hasValue protMinR_ i && rest) True protWrtFt_
        && coveredBy txnApp_ txnLastUpdated_
        && repeatedCoveredBy ckptVersion_ ckptTagKeys_
    ) (Left invalidRow)
  -- Validate the known action markers before visiting add rows.
  unless
    ( disjointRows addPath_ remPath_
        && disjointRows addPath_ metaId_
        && disjointRows addPath_ protMinR_
        && disjointRows addPath_ txnApp_
        && disjointRows addPath_ domDom_
        && disjointRows addPath_ sideP_
        && disjointRows addPath_ ckptVersion_
    ) (Left invalidRow)
  unfamiliar <- unknownActionRows pf groupIndex group n unfamiliarColumns

  let addCols =
        AddCols
          { addStats_ = addStats_
          , addParsedStats_ = parsedStats
          , addPartK_ = addPartK_
          , addPartV_ = addPartV_
          , addTagsK_ = addTagsK_
          , addTagsV_ = addTagsV_
          , addRowTracking_ = addRowTracking
          }
      remCols =
        RemCols
          { remPath_ = remPath_
          , remDelTs_ = remDelTs_
          , remDataChg_ = remDataChg_
          , remSize_ = remSize_
          , remPartK_ = remPartK_
          , remPartV_ = remPartV_
          , remPartPresent_ = hasRemovePartitionMap
          , remExt_ = remExt_
          , remDv_ = remDv
          , remRowTracking_ = remRowTracking
          }
      metaCols =
        MetaCols
          { metaId_ = metaId_
          , metaName_ = metaName_
          , metaDesc_ = metaDesc_
          , metaSchema_ = metaSchema_
          , metaFormat_ = formatCols
          , metaPart_ = metaPart_
          , metaConfK_ = metaConfK_
          , metaConfV_ = metaConfV_
          , metaCt_ = metaCt_
          }
      protCols =
        ProtCols
          { protMinR_ = protMinR_
          , protMinW_ = protMinW_
          , protRdrFt_ = protRdrFt_
          , protWrtFt_ = protWrtFt_
          }
      otherCols =
        OtherCols
          { txnApp_ = txnApp_
          , txnVersion_ = txnVersion_
          , txnLastUpdated_ = txnLastUpdated_
          , domDom_ = domDom_
          , domConfig_ = domConfig_
          , domRemoved_ = domRemoved_
          , sideP_ = sideP_
          , sideSize_ = sideSize_
          , ckptVersion_ = ckptVersion_
          , ckptTagKeys_ = ckptTagKeys_
          , ckptTagValues_ = ckptTagValues_
          }
      addCount = case addPath_ of
        NoValues -> 0
        OneValue {} -> 1
        DenseValues values -> pathCount values
        SparseValues _ _ values -> pathCount values
      present column i = if hasValue column i then 1 :: Int else 0
      otherActions i =
        present remPath_ i
          + present metaId_ i
          + present protMinR_ i
          + present txnApp_ i
          + present domDom_ i
          + present sideP_ i
          + present ckptVersion_ i
      runRows readDV = consume n addCount rowAt
        where
          rowAt index
            | Just path <- atIdxWith indexPath addPath_ i =
                case optionalAt unfamiliar i of
                  Nothing
                    | Just size <- atIdx addSize_ i
                    , size >= 0
                    , Just modified <- atIdx addModTime_ i
                    , Just changed <- atBoolIdx addDataChg_ i ->
                        case readDV i of
                          Right dv ->
                            let !add = addRow i addCols path (fromIntegral size) modified changed dv
                            in (# add | | #)
                          Left err -> (# | | err #)
                  _ -> (# | | invalidRow #)
            | otherwise = case (otherActions i, optionalAt unfamiliar i) of
                (1, Nothing) ->
                  case otherRow i remCols metaCols protCols otherCols sideTime_ of
                    Right action -> (# | action | #)
                    Left err -> (# | | err #)
                (0, Just name) -> (# | ActionOther name | #)
                _ -> (# | | invalidRow #)
            where
              i = I# index
      {-# INLINE runRows #-}
  -- The column readers have already checked every descriptor leaf. If
  -- none has a value, the row loop can skip constructing a lookup result
  -- for each of the five fields.
  case addDv of
    DeletionVectorCols NoValues NoValues NoValues NoValues NoValues ->
      runRows (\_ -> Right Nothing)
    _ -> runRows (decodeDeletionVector addDv)


invalidRow :: String
invalidRow = "Delta.Checkpoint: invalid action row or missing required fields"


-- JSON stats can cover every add in a mixed group. The structured values
-- can then be skipped, but their ancestor levels must still agree with
-- the action marker wherever another action owns the row.
validateSkippedStatsRows
  :: PR.ParquetFile
  -> CheckpointGroup
  -> Int
  -> ScalarColumn PathBuffers Text
  -> [CheckpointColumn]
  -> Either String ()
validateSkippedStatsRows _ _ _ _ [] = Right ()
validateSkippedStatsRows pf (CheckpointGroup groupIndex group _) rows adds columns = do
  let schema = P.fmSchema (PR.pfFooter pf)
  (actionRep, actionDef) <- PL.maxLevelsForSchemaPath schema (V.singleton "add")
  unless (actionRep == 0) (Left invalidRow)
  mapM_ (validateLeaf actionDef) columns
  where
    validateLeaf actionDef (CheckpointColumn index levels) = do
      let maxRep = repetitionLevel levels
          maxDef = definitionLevel levels
      metadata <- maybe (Left "Delta.Checkpoint: statistics column has no metadata") Right
        (P.ccMetadata (V.unsafeIndex (P.rgColumns group) index))
      chunk <- PR.columnChunkSlice pf groupIndex index
      absent <- checkpointColumnEmpty metadata maxRep maxDef (actionDef - 1) (P.rgNumRows group) chunk
      unless absent $ do
        events <- PR.readColumnLevelsOnly (P.cmCodec metadata) maxRep maxDef chunk
        let reps = PR.clRepetition events
            defs = PR.clDefinition events
            count = VP.length defs
            cutoff = fromIntegral actionDef
            go !event !row
              | event == count = unless (row + 1 == rows) (Left invalidRow)
              | otherwise =
                  let nextRow = if VP.unsafeIndex reps event == 0 then row + 1 else row
                  in if nextRow >= rows || (VP.unsafeIndex defs event >= cutoff && not (hasValue adds nextRow))
                       then Left invalidRow
                       else go (event + 1) nextRow
        unless (fromIntegral count == P.cmNumValues metadata) (Left invalidRow)
        go 0 (-1)


-- The required markers establish the presence of recognized actions.
-- Other actions (including those missing their required marker) are
-- checked using every leaf, without interpreting their payloads.
unknownActionColumns
  :: Map.Map (V.Vector Text) CheckpointColumn
  -> V.Vector (V.Vector Text, ColumnLevels)
  -> [(Text, V.Vector CheckpointColumn)]
unknownActionColumns columns = go 0
  where
    -- The preorder schema keeps each action's leaves contiguous.
    go !index remaining
      | V.null remaining = []
      | hasMarker = next
      | otherwise = (name, V.imap (\i (_, levels) -> CheckpointColumn (index + i) levels) actionLeaves) : next
      where
        actionName = V.unsafeHead . fst
        name = actionName (V.unsafeHead remaining)
        hasMarker = maybe False (\marker -> Map.member (V.fromList [name, marker]) columns) (actionMarker name)
        (actionLeaves, rest) = V.span ((== name) . actionName) remaining
        next = go (index + V.length actionLeaves) rest


-- Delta reserves breaking changes for new reader features or versions.
-- An unfamiliar action can be ignored once its parent is known to be present.
unknownActionRows
  :: PR.ParquetFile
  -> Int
  -> P.RowGroup
  -> Int
  -> [(Text, V.Vector CheckpointColumn)]
  -> Either String (V.Vector (Maybe Text))
unknownActionRows pf groupIndex group rows groups
  | null groups = Right V.empty
  | otherwise = runST $ do
      actions <- VM.replicate rows Nothing
      let visit [] = Right <$> V.unsafeFreeze actions
          visit ((name, columns) : rest) = case PL.maxLevelsForSchemaPath schema (V.singleton name) of
            Left err -> pure (Left err)
            Right (actionRep, actionDef)
              | actionRep /= 0 -> pure (Left "Delta.Checkpoint: top-level action cannot be repeated")
              | otherwise -> checkLeaves 0
              where
                presentAt = fromIntegral actionDef
                action = Just name
                checkLeaves !leafIndex
                  | leafIndex == V.length columns = visit rest
                checkLeaves !leafIndex = case readLevels (V.unsafeIndex columns leafIndex) of
                  Left err -> pure (Left err)
                  Right levels -> mark 0 0
                    where
                      reps = PR.clRepetition levels
                      defs = PR.clDefinition levels
                      mark !event !row
                        | event == VP.length reps =
                            if row == rows then checkLeaves (leafIndex + 1) else wrongRows
                        -- Every repeated value belongs to the same action
                        -- as the preceding value, including across pages.
                        | VP.unsafeIndex reps event /= 0 =
                            if present && VP.unsafeIndex defs (event - 1) >= presentAt
                              then mark (event + 1) row
                              else pure (Left "Delta.Checkpoint: repeated value continues an absent action")
                        | row >= rows = wrongRows
                        | leafIndex /= 0 = do
                            previous <- VM.unsafeRead actions row
                            if present == (previous == action)
                              then mark (event + 1) (row + 1)
                              else pure (Left "Delta.Checkpoint: unfamiliar action leaves disagree on parent rows")
                        | not present = mark (event + 1) (row + 1)
                        | name == "commitInfo" || name == "cdc" =
                            pure (Left "Delta.Checkpoint: commitInfo and cdc are invalid in checkpoints")
                        | isJust (actionMarker name) =
                            pure (Left "Delta.Checkpoint: invalid action row, missing marker column")
                        | otherwise = do
                            previous <- VM.unsafeRead actions row
                            case previous of
                              Just _ -> pure (Left "Delta.Checkpoint: multiple actions in a row")
                              Nothing -> do
                                VM.unsafeWrite actions row action
                                mark (event + 1) (row + 1)
                        where
                          present = VP.unsafeIndex defs event >= presentAt
          wrongRows = pure (Left "Delta.Checkpoint: action row count mismatch")
      visit groups
  where
    schema = P.fmSchema (PR.pfFooter pf)
    readLevels (CheckpointColumn index columnLevels) = do
      metadata <- maybe (Left "Delta.Checkpoint: missing unfamiliar action metadata") Right
        (P.ccMetadata (V.unsafeIndex (P.rgColumns group) index))
      chunk <- PR.columnChunkSlice pf groupIndex index
      levels <- PR.readColumnLevelsOnly (P.cmCodec metadata) (repetitionLevel columnLevels) (definitionLevel columnLevels) chunk
      unless (fromIntegral (VP.length (PR.clDefinition levels)) == P.cmNumValues metadata) $
        Left "Delta.Checkpoint: action column value count mismatch"
      pure levels


actionMarker :: Text -> Maybe Text
actionMarker name
  | name == "add" || name == "remove" || name == "sidecar" = Just "path"
  | name == "metaData" = Just "id"
  | name == "protocol" = Just "minReaderVersion"
  | name == "txn" = Just "appId"
  | name == "domainMetadata" = Just "domain"
  | name == "checkpointMetadata" = Just "version"
  | otherwise = Nothing


-- ============================================================
-- Per-action column bundles (so 'rowAction' takes a manageable
-- number of arguments instead of 30+ positional ones)
-- ============================================================

data AddCols = AddCols
  { addStats_ :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , addParsedStats_ :: !(V.Vector (Maybe AddStats))
  , addPartK_ :: !(RepeatedText)
  , addPartV_ :: !(RepeatedText)
  , addTagsK_ :: !(RepeatedText)
  , addTagsV_ :: !(RepeatedText)
  , addRowTracking_ :: !RowTrackingCols
  }


data RemCols = RemCols
  { remPath_ :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , remDelTs_ :: !(ScalarColumn (VP.Vector Int64) Int64)
  , remDataChg_ :: !(ScalarColumn PR.BoolBuffers Bool)
  , remSize_ :: !(ScalarColumn (VP.Vector Int64) Int64)
  , remPartK_ :: !(RepeatedText)
  , remPartV_ :: !(RepeatedText)
  , remPartPresent_ :: !(Int# -> Bool)
  , remExt_ :: !(ScalarColumn PR.BoolBuffers Bool)
  , remDv_ :: !DeletionVectorCols
  , remRowTracking_ :: !RowTrackingCols
  }


data MetaCols = MetaCols
  { metaId_ :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , metaName_ :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , metaDesc_ :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , metaSchema_ :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , metaFormat_ :: !FormatCols
  , metaPart_ :: !(RepeatedText)
  , metaConfK_ :: !(RepeatedText)
  , metaConfV_ :: !(RepeatedText)
  , metaCt_ :: !(ScalarColumn (VP.Vector Int64) Int64)
  }


data FormatCols = FormatCols
  { formatProviders :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , formatOptionKeys :: !RepeatedText
  , formatOptionValues :: !RepeatedText
  }


data ProtCols = ProtCols
  { protMinR_ :: !(ScalarColumn (VP.Vector Int32) Int32)
  , protMinW_ :: !(ScalarColumn (VP.Vector Int32) Int32)
  , protRdrFt_ :: !FeatureLists
  , protWrtFt_ :: !FeatureLists
  }


data OtherCols = OtherCols
  { txnApp_ :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , txnVersion_ :: !(ScalarColumn (VP.Vector Int64) Int64)
  , txnLastUpdated_ :: !(ScalarColumn (VP.Vector Int64) Int64)
  , domDom_ :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , domConfig_ :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , domRemoved_ :: !(ScalarColumn PR.BoolBuffers Bool)
  , sideP_ :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , sideSize_ :: !(ScalarColumn (VP.Vector Int64) Int64)
  , ckptVersion_ :: !(ScalarColumn (VP.Vector Int64) Int64)
  , ckptTagKeys_ :: !RepeatedText
  , ckptTagValues_ :: !RepeatedText
  }


-- Required fields were already read by the row loop.
addRow :: Int -> AddCols -> Text -> Word64 -> Int64 -> Bool -> Maybe Aeson.Value -> AddAction
addRow i columns path size modified changed dv =
  AddAction
    { addPath = path
    , addSize = size
    , addModificationTime = modified
    , addDataChange = changed
    , addStats = fmap decodeText (atIdx (addStats_ columns) i)
    , addParsedStats = optionalAt (addParsedStats_ columns) i
    , addPartitionValues = decodeOptStringMap (addPartK_ columns) (addPartV_ columns) i
    , addTags = decodeStringMap (addTagsK_ columns) (addTagsV_ columns) i
    , addDeletionVector = dv
    , addBaseRowId = atIdx (baseRowIds (addRowTracking_ columns)) i
    , addDefaultRowCommitVersion = atIdx (defaultRowCommitVersions (addRowTracking_ columns)) i
    }
{-# INLINE addRow #-}


-- The row loop has established exactly one known action is present.
otherRow
  :: Int
  -> RemCols
  -> MetaCols
  -> ProtCols
  -> OtherCols
  -> ScalarColumn (VP.Vector Int64) Int64
  -> Either String DeltaAction
otherRow i rc mc pc oc sideTime
  | Just path <- atIdx (remPath_ rc) i
  , Just changed <- atBoolIdx (remDataChg_ rc) i
  , let size = atIdx (remSize_ rc) i
  , maybe True (>= 0) size = do
      let extended = normalizeExtendedFileMetadata (atBoolIdx (remExt_ rc) i)
            (isJust size && remPartPresent_ rc (rowIndex i))
      dv <- decodeDeletionVector (remDv_ rc) i
      pure . ActionRemove $
        RemoveAction
          { removePath = decodeText path
          , removeDeletionTimestamp = atIdx (remDelTs_ rc) i
          , removeDataChange = changed
          , removeExtendedFileMetadata = extended
          , removeSize = fmap fromIntegral size
          , removePartitionValues = decodeOptStringMap (remPartK_ rc) (remPartV_ rc) i
          , removeDeletionVector = dv
          , removeBaseRowId = atIdx (baseRowIds (remRowTracking_ rc)) i
          , removeDefaultRowCommitVersion = atIdx (defaultRowCommitVersions (remRowTracking_ rc)) i
          }
  | Just metaIdBs <- atIdx (metaId_ mc) i
  , Just schema <- atIdx (metaSchema_ mc) i = do
      partitions <- decodePartitionColumns (metaPart_ mc) i
      format <- decodeMetadataFormat (metaFormat_ mc) i
      configuration <- decodeMetadataStringMap "metaData.configuration" (metaConfK_ mc) (metaConfV_ mc) i
      pure . ActionMetaData $
        MetaDataAction
          { mdId = decodeText metaIdBs
          , mdName = fmap decodeText (atIdx (metaName_ mc) i)
          , mdDescription = fmap decodeText (atIdx (metaDesc_ mc) i)
          , mdFormat = format
          , mdSchemaString = decodeText schema
          , mdPartitionColumns = partitions
          , mdConfiguration = configuration
          , mdCreatedTime = atIdx (metaCt_ mc) i
          }
  | Just minR <- atIdx (protMinR_ pc) i
  , Just minW <- atIdx (protMinW_ pc) i =
      ActionProtocol
        <$> protocolFromFeatureLists
          (fromIntegral minR)
          (fromIntegral minW)
          (IntMap.lookup i (protRdrFt_ pc))
          (IntMap.lookup i (protWrtFt_ pc))
  | Just appId <- atIdx (txnApp_ oc) i
  , Just version <- atIdx (txnVersion_ oc) i =
      Right (ActionTxn (TxnAction (decodeText appId) version (atIdx (txnLastUpdated_ oc) i)))
  | Just domain <- atIdx (domDom_ oc) i
  , Just config <- atIdx (domConfig_ oc) i
  , Just removed <- atBoolIdx (domRemoved_ oc) i =
      Right (ActionDomainMetadata (DomainMetadataAction (decodeText domain) (decodeText config) removed))
  | Just path <- atIdx (sideP_ oc) i
  , Just size <- atIdx (sideSize_ oc) i
  , size >= 0
  , hasValue sideTime i =
      Right (ActionSidecar (SidecarAction (decodeText path) (fromIntegral size)))
  | Just version <- atIdx (ckptVersion_ oc) i
  , version >= 0 =
      ActionCheckpointMetadataWithTags (fromIntegral version)
        <$> decodeMetadataStringMap "checkpointMetadata.tags" (ckptTagKeys_ oc) (ckptTagValues_ oc) i
  | otherwise = Left invalidRow


-- ============================================================
-- Map / list value-shape helpers
-- ============================================================

-- | Decode tags, whose null values are omitted from the result.
decodeStringMap
  :: RepeatedText -- keys per row
  -> RepeatedText -- values per row
  -> Int
  -> Map.Map Text Text
decodeStringMap keysCol valsCol i =
  let !keys = repeatedTextAt keysCol i
      !vals = repeatedTextAt valsCol i
  in V.ifoldl' (insert vals) Map.empty keys
  where
    insert vals entries j (Just key) =
      case optionalAt vals j of
        Just value -> Map.insert (decodeText key) (decodeText value) entries
        Nothing -> Map.delete (decodeText key) entries
    insert _ entries _ Nothing = entries


{- | Like 'decodeStringMap' but the value branch is itself a
nullable (the Delta @partitionValues@ schema lets values be
@null@ for partitions like @region=NULL@). The result map
carries 'Maybe Text' values.
-}
decodeOptStringMap
  :: RepeatedText
  -> RepeatedText
  -> Int
  -> Map.Map Text (Maybe Text)
decodeOptStringMap keysCol valsCol i =
  let !keys = repeatedTextAt keysCol i
      !vals = repeatedTextAt valsCol i
  in V.ifoldl' (insert vals) Map.empty keys
  where
    insert vals entries j (Just key) =
      Map.insert (decodeText key) (normalizePartitionValue (decodeText <$> optionalAt vals j)) entries
    insert _ entries _ Nothing = entries


decodeMetadataFormat :: FormatCols -> Int -> Either String (Maybe (Text, Map.Map Text Text))
decodeMetadataFormat cols i =
  traverse (\provider -> (decodeText provider,) <$> options) (atIdx (formatProviders cols) i)
  where
    options = decodeMetadataStringMap "metaData.format.options" (formatOptionKeys cols) (formatOptionValues cols) i


decodeMetadataStringMap :: String -> RepeatedText -> RepeatedText -> Int -> Either String (Map.Map Text Text)
decodeMetadataStringMap field keysCol valuesCol i =
  V.ifoldM' insert Map.empty (repeatedTextAt keysCol i)
  where
    values = repeatedTextAt valuesCol i
    insert entries index (Just key)
      | Just value <- optionalAt values index =
          Right (Map.insert (decodeText key) (decodeText value) entries)
    insert _ _ _ = Left ("Delta.Checkpoint: null value in " ++ field)


decodePartitionColumns
  :: RepeatedText
  -> Int
  -> Either String [Text]
decodePartitionColumns col i =
  V.foldr decode (Right []) (repeatedTextAt col i)
  where
    decode (Just value) rest = (decodeText value :) <$> rest
    decode Nothing _ = Left "Delta.Checkpoint: null partition column name"


data RowTrackingCols = RowTrackingCols
  { baseRowIds :: !(ScalarColumn (VP.Vector Int64) Int64)
  , defaultRowCommitVersions :: !(ScalarColumn (VP.Vector Int64) Int64)
  }


readRowTrackingCols :: PR.ParquetFile -> CheckpointGroup -> Text -> Either String RowTrackingCols
readRowTrackingCols pf rg action =
  RowTrackingCols
    <$> readInt64Opt pf rg [action, "baseRowId"]
    <*> readInt64Opt pf rg [action, "defaultRowCommitVersion"]


rowTrackingCoveredBy :: ScalarColumn v a -> RowTrackingCols -> Bool
rowTrackingCoveredBy marker (RowTrackingCols ids versions) =
  coveredBy marker ids && coveredBy marker versions


data DeletionVectorCols = DeletionVectorCols
  { dvStorageType :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , dvPathOrInline :: !(ScalarColumn (V.Vector ByteString) ByteString)
  , dvOffset :: !(ScalarColumn (VP.Vector Int32) Int32)
  , dvSize :: !(ScalarColumn (VP.Vector Int32) Int32)
  , dvCardinality :: !(ScalarColumn (VP.Vector Int64) Int64)
  }


readDeletionVectorCols :: PR.ParquetFile -> CheckpointGroup -> Text -> Either String DeletionVectorCols
readDeletionVectorCols pf rg@(CheckpointGroup _ _ chunks) action = do
  storage <- readByteArrayOpt pf rg storagePath
  case storage of
    NoValues | Map.notMember (V.fromList storagePath) chunks ->
      validateMissingDeletionVectorStorage pf rg descriptorPath
    _ -> pure ()
  DeletionVectorCols storage
    <$> readByteArrayOpt pf rg (path "pathOrInlineDv")
    <*> readInt32Opt pf rg (path "offset")
    <*> readInt32Opt pf rg (path "sizeInBytes")
    <*> readInt64Opt pf rg (path "cardinality")
  where
    descriptorPath = [action, "deletionVector"]
    storagePath = path "storageType"
    path leaf = descriptorPath ++ [leaf]


-- A projected checkpoint can omit descriptor columns. If storageType is
-- missing, every remaining leaf must prove the descriptor itself is absent.
validateMissingDeletionVectorStorage :: PR.ParquetFile -> CheckpointGroup -> [Text] -> Either String ()
validateMissingDeletionVectorStorage pf (CheckpointGroup groupIndex group _) path =
  case PL.projectSchemaPath schema projectedPath of
    Left "Parquet.Levels: column path not found in schema" -> Right ()
    Left err -> Left err
    Right (first, projected) -> do
      case projected V.!? (length path) of
        Just node
          | P.seType node == Nothing
          , P.seRepetition node `elem` [Just P.Required, Just P.Optional] -> Right ()
        _ -> Left invalidDescriptor
      (_, definition) <- PL.maxLevelsForSchemaPath projected projectedPath
      leaves <- schemaLeaves projected
      when (V.null leaves) (Left invalidDescriptor)
      V.imapM_ (checkLeaf first definition) leaves
  where
    schema = P.fmSchema (PR.pfFooter pf)
    projectedPath = V.fromList path
    invalidDescriptor = "Delta.Checkpoint: invalid action row, deletion vector missing storageType in " ++ show path
    checkLeaf first definition index (_, ColumnLevels maxRep maxDef _) = do
      let ordinal = first + index
          rows = P.rgNumRows group
      metadata <- maybe (Left invalidDescriptor) Right
        (P.ccMetadata (V.unsafeIndex (P.rgColumns group) ordinal))
      chunk <- PR.columnChunkSlice pf groupIndex ordinal
      absent <- checkpointColumnEmpty metadata maxRep maxDef (definition - 1) rows chunk
      unless absent $ do
        levels <- PR.readColumnLevelsOnly (P.cmCodec metadata) maxRep maxDef chunk
        let definitions = PR.clDefinition levels
            count = fromIntegral (VP.length definitions)
        unless
          ( count == rows && count == P.cmNumValues metadata
              && VP.all (== 0) (PR.clRepetition levels)
              && VP.all (< fromIntegral definition) definitions
          ) (Left invalidDescriptor)


deletionVectorCoveredBy :: ScalarColumn v a -> DeletionVectorCols -> Bool
deletionVectorCoveredBy marker (DeletionVectorCols storage payload offset size count) =
  coveredBy marker storage && coveredBy marker payload && coveredBy marker offset
    && coveredBy marker size && coveredBy marker count


-- Reassemble the same descriptor object as the JSON log reader.
decodeDeletionVector :: DeletionVectorCols -> Int -> Either String (Maybe Aeson.Value)
decodeDeletionVector cols i
  | Nothing <- storageType
  , Nothing <- pathOrInline
  , Nothing <- offset
  , Nothing <- size
  , Nothing <- cardinality =
      Right Nothing
  | Just storage <- storageType
  , Just payload <- pathOrInline
  , Just byteCount <- size
  , Just count <- cardinality
  , validDeletionVectorFields storage (BS.null payload) offset byteCount count =
      let fields = KM.fromList
            [ ("storageType", Aeson.String (decodeText storage))
            , ("pathOrInlineDv", Aeson.String (decodeText payload))
            , ("sizeInBytes", Aeson.Number (fromIntegral byteCount))
            , ("cardinality", Aeson.Number (fromIntegral count))
            ]
          withOffset = maybe fields (\value -> KM.insert "offset" (Aeson.Number (fromIntegral value)) fields) offset
      in Right (Just (Aeson.Object withOffset))
  | otherwise =
      Left "Delta.Checkpoint: invalid deletion vector descriptor"
  where
    storageType = atIdx (dvStorageType cols) i
    pathOrInline = atIdx (dvPathOrInline cols) i
    offset = atIdx (dvOffset cols) i
    size = atIdx (dvSize cols) i
    cardinality = atIdx (dvCardinality cols) i
{-# INLINE decodeDeletionVector #-}


optionalAt :: V.Vector (Maybe a) -> Int -> Maybe a
optionalAt values i = case values V.!? i of
  Just result -> result
  Nothing -> Nothing


-- All scalar readers validate the row count before constructing a column.
-- Row-group callers only use indices from that row group.
data ScalarColumn v a
  = NoValues
  | OneValue {-# UNPACK #-} !Int !a
  | DenseValues !v
  | SparseValues !(VP.Vector Word64) !(VP.Vector Int) !v


sameScalarRows :: ScalarColumn v a -> ScalarColumn w b -> Bool
sameScalarRows NoValues NoValues = True
sameScalarRows (OneValue a _) (OneValue b _) = a == b
sameScalarRows (DenseValues _) (DenseValues _) = True
sameScalarRows (SparseValues a _ _) (SparseValues b _ _) = a == b
sameScalarRows _ _ = False


-- Both columns describe the same row group. Empty and singleton markers
-- need no scan. For two sparse markers, compare 64 rows at a time.
disjointRows :: ScalarColumn v a -> ScalarColumn w b -> Bool
disjointRows NoValues _ = True
disjointRows _ NoValues = True
disjointRows (OneValue i _) other = not (hasValue other i)
disjointRows adds (OneValue i _) = not (hasValue adds i)
disjointRows (SparseValues left _ _) (SparseValues right _ _) = go 0
  where
    go !i
      | i == VP.length left = True
      | otherwise = VP.unsafeIndex left i .&. VP.unsafeIndex right i == 0 && go (i + 1)
disjointRows _ _ = False


-- Both columns describe the same row group. Compare sparse masks in
-- blocks of 64 rows. Dense and empty columns only need a shape check.
hasUncoveredRows :: ScalarColumn v a -> ScalarColumn w b -> Bool
hasUncoveredRows NoValues _ = False
hasUncoveredRows _ (DenseValues _) = False
hasUncoveredRows (OneValue i _) covering = not (hasValue covering i)
hasUncoveredRows (SparseValues rows _ _) (SparseValues covering _ _) = go 0
  where
    go !i
      | i == VP.length rows = False
      | otherwise =
          let required = VP.unsafeIndex rows i
          in required .&. VP.unsafeIndex covering i /= required || go (i + 1)
hasUncoveredRows _ _ = True


coveredBy :: ScalarColumn v a -> ScalarColumn w b -> Bool
coveredBy marker values = not (hasUncoveredRows values marker)
{-# INLINE coveredBy #-}


hasValue :: ScalarColumn v a -> Int -> Bool
hasValue NoValues _ = False
hasValue (OneValue row _) i = i == row
hasValue (DenseValues _) _ = True
hasValue (SparseValues masks _ _) i =
  VP.unsafeIndex masks (i `quot` 64) .&. rowMask i /= 0
{-# INLINE hasValue #-}


-- Mask the offset before shifting so the hot path needs no range check.
rowMask :: Int -> Word64
rowMask i = 1 `unsafeShiftL` (i .&. 63)
{-# INLINE rowMask #-}


atIdx :: VG.Vector v a => ScalarColumn (v a) a -> Int -> Maybe a
atIdx = atIdxWith VG.unsafeIndex
{-# INLINE atIdx #-}


atBoolIdx :: ScalarColumn PR.BoolBuffers Bool -> Int -> Maybe Bool
atBoolIdx = atIdxWith indexBool
{-# INLINE atBoolIdx #-}


indexBool :: PR.BoolBuffers -> Int -> Bool
indexBool values i =
  BSU.unsafeIndex (PR.boolData values) (i `quot` 8) .&. (1 `unsafeShiftL` (i .&. 7)) /= 0
{-# INLINE indexBool #-}


atIdxWith :: (v -> Int -> a) -> ScalarColumn v a -> Int -> Maybe a
atIdxWith indexValue column i = case column of
  NoValues -> Nothing
  OneValue row value -> if i == row then Just value else Nothing
  DenseValues values -> Just $! indexValue values i
  SparseValues masks starts values ->
    let block = i `quot` 64
        mask = VP.unsafeIndex masks block
        rowBit = rowMask i
    in if mask .&. rowBit /= 0
         then
           let index = VP.unsafeIndex starts block + popCount (mask .&. (rowBit - 1))
           in Just $! indexValue values index
         else Nothing
{-# INLINE atIdxWith #-}


-- Both the scalar and repeated text readers validate every defined slice
-- before replay. Copy the validated UTF-8 into an owned byte array that
-- Text can use directly without retaining the Parquet page.
decodeText :: ByteString -> Text
decodeText bytes = case SBS.toShort bytes of
  SBSI.SBS array -> TI.Text (TA.ByteArray array) 0 (BS.length bytes)


-- ============================================================
-- Per-leaf readers (path-based, encoding-agnostic)
-- ============================================================

data CheckpointGroup = CheckpointGroup
  {-# UNPACK #-} !Int
  !P.RowGroup
  !(Map.Map (V.Vector Text) CheckpointColumn)


data CheckpointColumn = CheckpointColumn {-# UNPACK #-} !Int {-# UNPACK #-} !ColumnLevels


withCheckpointChunk
  :: PR.ParquetFile
  -> CheckpointGroup
  -> [Text]
  -> P.ParquetType
  -> a
  -> (P.ColumnMetadata -> ColumnLevels -> ByteString -> Either String a)
  -> Either String a
withCheckpointChunk pf (CheckpointGroup groupIndex group chunks) path expected absent decode =
  case Map.lookup (V.fromList path) chunks of
    Nothing -> Right absent
    Just (CheckpointColumn columnIndex levels) -> do
      cm <-
        maybe (Left "Delta.Checkpoint: column chunk has no metadata") Right $
          P.ccMetadata (V.unsafeIndex (P.rgColumns group) columnIndex)
      unless (P.cmType cm == expected) $
        Left ("Delta.Checkpoint: unexpected physical type in " ++ show path)
      chunk <- PR.columnChunkSlice pf groupIndex columnIndex
      decode cm levels chunk


-- Statistics select the fast path. The page streams still have to prove
-- that every row is absent, because footer counts can be wrong.
checkpointColumnEmpty :: P.ColumnMetadata -> Int -> Int -> Int -> Int64 -> ByteString -> Either String Bool
checkpointColumnEmpty metadata maxRep maxDef maxEmptyDefinition rows bytes
  | allNullEvents metadata rows =
      PR.columnChunkHasNoEntries (P.cmCodec metadata) maxRep maxDef maxEmptyDefinition rows bytes
  | otherwise = Right False


allNullEvents :: P.ColumnMetadata -> Int64 -> Bool
allNullEvents metadata rows =
  P.cmNumValues metadata == rows && (P.cmStatistics metadata >>= P.statNullCount) == Just rows


{- | Read optional leaves using their repetition and definition levels.
Leaves absent from the schema use an empty vector.
-}
readByteArrayOpt
  :: PR.ParquetFile
  -> CheckpointGroup
  -> [Text]
  -> Either String (ScalarColumn (V.Vector ByteString) ByteString)
readByteArrayOpt pf rg path =
  readScalarOpt P.PTByteArray PR.readByteArrayDefinitions (\values -> validateUtf8 path (V.all BS.isValidUtf8 values) ()) pf rg path


-- Checked offsets into one owned UTF-8 array per row group. Paths can share
-- this array without retaining compressed pages or allocating a copy per row.
data PathBuffers = PathBuffers !(VS.Vector Int64) !TA.Array


pathCount :: PathBuffers -> Int
pathCount (PathBuffers offsets _) = VS.length offsets - 1
{-# INLINE pathCount #-}


indexPath :: PathBuffers -> Int -> Text
indexPath (PathBuffers offsets bytes) i =
  let start = fromIntegral (VS.unsafeIndex offsets i)
      end = fromIntegral (VS.unsafeIndex offsets (i + 1))
  in TI.Text bytes start (end - start)
{-# INLINE indexPath #-}


readAddPaths :: PR.ParquetFile -> CheckpointGroup -> Either String (ScalarColumn PathBuffers Text)
readAddPaths pf rg =
  readScalarOptWith pathCount indexPath P.PTByteArray readPaths (const (Right ())) pf rg path
  where
    path = ["add", "path"]
    readPaths codec maxDef chunk = do
      levels <- PR.readLargeBinaryBuffersDefinitions codec maxDef chunk
      let raw = PR.clValues levels
          offsets = PR.binaryOffsets raw
          bytes = PR.binaryData raw
          count = VS.length offsets - 1
          size = BS.length bytes
          -- After validating the whole buffer, a boundary is valid at
          -- the end or before any byte other than a continuation byte.
          -- Checked offsets are monotonic, so trailing empty paths need
          -- no further reads once the offset reaches the end.
          validBoundary !i
            | i >= count = True
            | offset == size = True
            | otherwise =
                BSU.unsafeIndex bytes offset .&. 0xc0 /= 0x80 && validBoundary (i + 1)
            where
              offset = fromIntegral (VS.unsafeIndex offsets i)
      validateUtf8 path (BS.isValidUtf8 bytes && validBoundary 1) ()
      case SBS.toShort bytes of
        SBSI.SBS array -> pure (levels {PR.clValues = PathBuffers offsets (TA.ByteArray array)})


readInt64Opt
  :: PR.ParquetFile
  -> CheckpointGroup
  -> [Text]
  -> Either String (ScalarColumn (VP.Vector Int64) Int64)
readInt64Opt = readScalarOpt P.PTInt64 PR.readInt64Definitions (const (Right ()))


readInt32Opt
  :: PR.ParquetFile
  -> CheckpointGroup
  -> [Text]
  -> Either String (ScalarColumn (VP.Vector Int32) Int32)
readInt32Opt = readScalarOpt P.PTInt32 PR.readInt32Definitions (const (Right ()))


readBoolOpt
  :: PR.ParquetFile
  -> CheckpointGroup
  -> [Text]
  -> Either String (ScalarColumn PR.BoolBuffers Bool)
readBoolOpt = readScalarOptWith PR.boolCount indexBool P.PTBoolean PR.readBoolBuffersDefinitions (const (Right ()))


-- Keep the physical values in their original vector. Sparse columns index
-- only defined entries, and dense and all-null columns need no row index.
readScalarOpt
  :: VG.Vector v a
  => P.ParquetType
  -> (P.Compression -> Int -> ByteString -> Either String (PR.ColumnLevels (v a)))
  -> (v a -> Either String ())
  -> PR.ParquetFile
  -> CheckpointGroup
  -> [Text]
  -> Either String (ScalarColumn (v a) a)
readScalarOpt = readScalarOptWith VG.length VG.unsafeIndex
{-# INLINE readScalarOpt #-}


readScalarOptWith
  :: (v -> Int)
  -> (v -> Int -> a)
  -> P.ParquetType
  -> (P.Compression -> Int -> ByteString -> Either String (PR.ColumnLevels v))
  -> (v -> Either String ())
  -> PR.ParquetFile
  -> CheckpointGroup
  -> [Text]
  -> Either String (ScalarColumn v a)
readScalarOptWith valueCount indexValue expected reader validate pf rg@(CheckpointGroup _ group _) path =
  withCheckpointChunk pf rg path expected NoValues $ \metadata levels chunk -> do
    let maxRep = repetitionLevel levels
        maxDef = definitionLevel levels
    unless (maxRep == 0) $
      Left ("Delta.Checkpoint: expected a scalar column at " ++ show path)
    -- A null marker can mean either a null parent or a present action
    -- missing a required field. Preserve that distinction for mixed rows.
    let presentAt = case path of
          [name, leaf] | actionMarker name == Just leaf -> parentDefinitionLevel levels
          [name, "deletionVector", "storageType"] | name == "add" || name == "remove" -> parentDefinitionLevel levels
          ["metaData", "format", "provider"] -> parentDefinitionLevel levels
          _ -> maxDef
    empty <- checkpointColumnEmpty metadata maxRep maxDef (presentAt - 1) (P.rgNumRows group) chunk
    if empty then Right NoValues else readValues metadata maxDef presentAt chunk
  where
    readValues metadata maxDef presentAt chunk = do
      levels <- reader (P.cmCodec metadata) maxDef chunk
      let definitions = PR.clDefinition levels
          values = PR.clValues levels
          defined = fromIntegral maxDef
          size = VP.length definitions
      unless (fromIntegral size == P.rgNumRows group) $
        Left ("Delta.Checkpoint: row count mismatch in " ++ show path)
      unless (fromIntegral size == P.cmNumValues metadata) $
        Left ("Delta.Checkpoint: column value count mismatch in " ++ show path)
      let count = valueCount values
          missingMarker d = d >= fromIntegral presentAt && d < defined
      when (presentAt < maxDef && count < size && VP.any missingMarker definitions) $
        Left ("Delta.Checkpoint: invalid action row, missing marker at " ++ show path)
      validate values
      case count of
        0 -> Right NoValues
        1 -> case VP.findIndex (== defined) definitions of
          Just row -> Right (OneValue row (indexValue values 0))
          Nothing -> Left ("Delta.Checkpoint: missing definition for value in " ++ show path)
        _
          | count == size -> Right (DenseValues values)
          | otherwise -> Right (indexDefinedRows defined definitions values)
{-# INLINE readScalarOptWith #-}


-- One validity word and one starting value offset per 64 rows.
-- Count set bits before a row to recover its index into the packed values.
indexDefinedRows :: Int32 -> VP.Vector Int32 -> v -> ScalarColumn v a
indexDefinedRows defined definitions values = runST $ do
  masks <- VPM.unsafeNew blocks
  starts <- VPM.unsafeNew blocks
  let fill !row !offset !end !mask
        | row == end = mask
        | VP.unsafeIndex definitions row == defined = fill (row + 1) (offset + 1) end (mask .|. bit offset)
        | otherwise = fill (row + 1) (offset + 1) end mask
      go !block !row !value
        | block == blocks = SparseValues <$> VP.unsafeFreeze masks <*> VP.unsafeFreeze starts <*> pure values
        | otherwise = do
            let !end = row + min 64 (size - row)
                !mask = fill row 0 end 0
            VPM.unsafeWrite masks block mask
            VPM.unsafeWrite starts block value
            go (block + 1) end (value + popCount mask)
  go 0 0 0
  where
    size = VP.length definitions
    blocks = 1 + (size - 1) `quot` 64


data RepeatedText = RepeatedText {-# UNPACK #-} !Int !(V.Vector (V.Vector (Maybe ByteString)))


emptyRepeatedText :: RepeatedText
emptyRepeatedText = RepeatedText 0 V.empty


repeatedTextAt :: RepeatedText -> Int -> V.Vector (Maybe ByteString)
repeatedTextAt (RepeatedText first rows) i = fromMaybe V.empty (rows V.!? (i - first))
{-# INLINE repeatedTextAt #-}


repeatedCoveredBy :: ScalarColumn v a -> RepeatedText -> Bool
repeatedCoveredBy (DenseValues _) _ = True
repeatedCoveredBy marker (RepeatedText first rows) = go 0
  where
    go !i
      | i == V.length rows = True
      | V.null (V.unsafeIndex rows i) || hasValue marker (first + i) = go (i + 1)
      | otherwise = False


type MapColumns = CheckpointGroup -> Either String (RepeatedText, RepeatedText)


type ListColumn a = CheckpointGroup -> Either String a


-- Protocol lists are present only on protocol rows, even in a row group
-- with many file actions. An entry with [] means a defined empty list.
type FeatureLists = IntMap.IntMap [Text]


data CheckpointCollections = CheckpointCollections
  { addPartitions :: !MapColumns
  , addTagColumns :: !MapColumns
  , removePartitions :: !(CheckpointGroup -> ScalarColumn PR.BoolBuffers Bool -> Either String (RepeatedText, RepeatedText, Int# -> Bool))
  , metadataFormat :: !(CheckpointGroup -> Either String FormatCols)
  , metadataPartitions :: !(ListColumn RepeatedText)
  , metadataConfiguration :: !MapColumns
  , readerFeatures :: !(ListColumn FeatureLists)
  , writerFeatures :: !(ListColumn FeatureLists)
  , checkpointTags :: !MapColumns
  }


-- Resolve each collection against the file schema before reading row groups.
-- Each reader then looks up its own chunks in the current row group.
checkpointCollections :: PR.ParquetFile -> CheckpointCollections
checkpointCollections pf =
  CheckpointCollections
    { addPartitions = readTextMap pf ["add", "partitionValues"]
    , addTagColumns = readTextMap pf ["add", "tags"]
    , removePartitions = readRemovePartitions pf
    , metadataFormat = readMetadataFormat pf
    , metadataPartitions = readListColumn pf ["metaData", "partitionColumns"] emptyRepeatedText (readRepeatedText pf)
    , metadataConfiguration = readTextMap pf ["metaData", "configuration"]
    , readerFeatures = readFeatureLists pf ["protocol", "readerFeatures"]
    , writerFeatures = readFeatureLists pf ["protocol", "writerFeatures"]
    , checkpointTags = readTextMap pf ["checkpointMetadata", "tags"]
    }


readMetadataFormat
  :: PR.ParquetFile
  -> CheckpointGroup
  -> Either String FormatCols
readMetadataFormat pf =
  case repeatedSchema pf path of
    Left err -> const (Left err)
    Right [] -> const (Right (FormatCols NoValues emptyRepeatedText emptyRepeatedText))
    Right (format : _)
      | P.seNumChildren format /= Nothing
      , P.seRepetition format `elem` [Just P.Required, Just P.Optional]
      , Right _ <- PL.maxLevelsForColumnPath schema (V.fromList providerPath) ->
          let readOptions = readTextMap pf (path ++ ["options"])
          in \rg -> do
            provider <- readByteArrayOpt pf rg providerPath
            (keys, values) <- readOptions rg
            pure (FormatCols provider keys values)
    _ -> const (Left "Delta.Checkpoint: invalid metaData.format schema, expected a provider field")
  where
    schema = P.fmSchema (PR.pfFooter pf)
    path = ["metaData", "format"]
    providerPath = path ++ ["provider"]


-- Resolve the element by position. Older writers used different names or
-- a repeated BYTE_ARRAY directly, with or without a LIST wrapper.
readListColumn :: PR.ParquetFile -> [Text] -> a -> (CheckpointGroup -> [Text] -> Int -> Either String a) -> ListColumn a
readListColumn pf path absent readElements =
  case repeatedSchema pf path of
    Left err -> const (Left err)
    Right [] -> const (Right absent)
    Right [element] | repeatedString element -> readElement path 1
    Right [container, element]
      | listContainer container && repeatedString element -> readElement (path ++ [P.seName element]) 1
    Right [container, entry, element]
      | listContainer container
      , P.seRepetition entry == Just P.Repeated
      , P.seNumChildren entry == Just 1
      , P.seName entry /= "array"
      , P.seName entry /= P.seName container <> "_tuple"
      , P.seType element == Just P.PTByteArray
      , P.seRepetition element `elem` [Just P.Required, Just P.Optional] ->
          readElement (path ++ [P.seName entry, P.seName element]) (if P.seRepetition element == Just P.Optional then 2 else 1)
    _ -> const (Left ("Delta.Checkpoint: invalid string list schema at " ++ show path))
  where
    repeatedString element =
      P.seType element == Just P.PTByteArray && P.seRepetition element == Just P.Repeated
    listContainer container =
      P.seNumChildren container == Just 1
        && P.seRepetition container `elem` [Just P.Required, Just P.Optional]
        && (P.seLogicalType container == Just P.LTList || P.seConvertedType container == Just P.CTList)
    readElement elementPath levelOffset rg = readElements rg elementPath levelOffset


readFeatureLists :: PR.ParquetFile -> [Text] -> ListColumn FeatureLists
readFeatureLists pf path = readListColumn pf path IntMap.empty readElements
  where
    readElements rg elementPath emptyOffset =
      readRepeatedTextWith pf rg elementPath emptyOffset (emptyOffset + 1) IntMap.empty decode
    decode _ maxDefinition emptyDefinition levels = go 0 (-1) 0 Nothing IntMap.empty
      where
        reps = PR.clRepetition levels
        defs = PR.clDefinition levels
        values = PR.clValues levels
        flush index features lists = case features of
          Nothing -> lists
          Just names -> IntMap.insert index (reverse names) lists
        go !i !row !valueIndex features !lists
          | i == VP.length defs = Right (flush row features lists)
          | otherwise =
              let startsRow = VP.unsafeIndex reps i == 0
                  definition = VP.unsafeIndex defs i
                  index = if startsRow then row + 1 else row
                  next = if startsRow then flush row features lists else lists
                  names = if startsRow then [] else fromMaybe [] features
              in if definition == maxDefinition
                   then go (i + 1) index (valueIndex + 1) (Just (decodeText (V.unsafeIndex values valueIndex) : names)) next
                   else if definition == emptyDefinition
                     then go (i + 1) index valueIndex (Just []) next
                     else if definition < emptyDefinition
                       then go (i + 1) index valueIndex Nothing next
                       else Left ("Delta.Checkpoint: null feature name in " ++ show path)


repeatedSchema :: PR.ParquetFile -> [Text] -> Either String [P.SchemaElement]
repeatedSchema pf path =
  case PL.projectSchemaPath (P.fmSchema (PR.pfFooter pf)) (V.fromList path) of
    Left "Parquet.Levels: column path not found in schema" -> Right []
    Left err -> Left err
    Right (_, projected) -> Right (V.toList (V.drop (length path) projected))


-- Both leaves must agree on their shared ancestors, including the
-- difference between an absent map, an empty map and a null value.
readTextMap :: PR.ParquetFile -> [Text] -> MapColumns
readTextMap pf path = readTextMapWith pf path (\rows keys values -> (mapLeafRows rows keys, mapLeafRows rows values))


readRemovePartitions
  :: PR.ParquetFile
  -> CheckpointGroup
  -> ScalarColumn PR.BoolBuffers Bool
  -> Either String (RepeatedText, RepeatedText, Int# -> Bool)
readRemovePartitions pf =
  let readGroup = readTextMapWith pf ["remove", "partitionValues"] $
        \rows keys values extended ->
          (mapLeafRows rows keys, mapLeafRows rows values, removePartitionPresence rows extended keys)
  in \group extended -> ($ extended) <$> readGroup group


readTextMapWith
  :: PR.ParquetFile
  -> [Text]
  -> (Int -> MapLeaf -> MapLeaf -> a)
  -> CheckpointGroup
  -> Either String a
readTextMapWith pf path finish =
  case repeatedSchema pf path of
    Left err -> const (Left err)
    Right [] -> const (Right (finish 0 missingMapLeaf missingMapLeaf))
    Right [container, entry, key]
      | validLayout container entry key 1 -> readFields entry key Nothing
    Right [container, entry, key, value]
      | validLayout container entry key 2
      , P.seType value == Just P.PTByteArray
      , P.seRepetition value `elem` [Just P.Required, Just P.Optional] ->
          readFields entry key (Just value)
    _ -> const (Left ("Delta.Checkpoint: invalid map schema at " ++ show path))
  where
    validLayout container entry key size =
      P.seNumChildren container == Just 1
        && P.seRepetition container `elem` [Just P.Required, Just P.Optional]
        && (P.seLogicalType container == Just P.LTMap || P.seConvertedType container `elem` [Just P.CTMap, Just P.CTMapKeyValue])
        && P.seRepetition entry == Just P.Repeated
        && P.seNumChildren entry == Just size
        && P.seType key == Just P.PTByteArray
        && P.seRepetition key == Just P.Required
    readFields entry key value rg@(CheckpointGroup _ group _) = do
      let prefix = path ++ [P.seName entry]
          keyPath = prefix ++ [P.seName key]
          rows = fromIntegral (P.rgNumRows group)
      keys <- readMapLeaf pf rg keyPath 1
      case value of
        Nothing -> pure (finish rows keys missingMapLeaf)
        Just valueField -> do
          let valuePath = prefix ++ [P.seName valueField]
              valueOffset = if P.seRepetition valueField == Just P.Optional then 2 else 1
          values <- readMapLeaf pf rg valuePath valueOffset
          unless (mapLeavesAgree keys values) $
            Left ("Delta.Checkpoint: map key and value levels disagree in " ++ show path)
          pure (finish rows keys values)
{-# INLINE readTextMapWith #-}


-- Large unpartitioned checkpoints often have whole chunks of null or empty
-- maps. Retain their proven level without allocating per-row level arrays.
data MapLeaf
  = ConstantEmptyMap {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32
  | MapLeaf {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int32 !(PR.ColumnLevels (V.Vector ByteString))


missingMapLeaf :: MapLeaf
missingMapLeaf = ConstantEmptyMap 0 (-1)


-- The definition level at the empty map still marks it as present.
removePartitionPresence :: Int -> ScalarColumn PR.BoolBuffers Bool -> MapLeaf -> Int# -> Bool
removePartitionPresence _ NoValues _ = \_ -> False
removePartitionPresence _ (OneValue _ False) _ = \_ -> False
removePartitionPresence rows _ keys = case keys of
  ConstantEmptyMap emptyDefinition level -> \_ -> level >= emptyDefinition
  MapLeaf _ emptyDefinition levels ->
    let reps = PR.clRepetition levels
        defs = PR.clDefinition levels
        !masks = runST $ do
          bits <- VPM.replicate (1 + (rows - 1) `quot` 64) (0 :: Word64)
          let go !event !row
                | event == VP.length defs = VP.unsafeFreeze bits
                | VP.unsafeIndex reps event /= 0 = go (event + 1) row
                | otherwise = do
                    when (VP.unsafeIndex defs event >= emptyDefinition) $ do
                      let block = row `quot` 64
                      current <- VPM.unsafeRead bits block
                      VPM.unsafeWrite bits block (current .|. rowMask row)
                    go (event + 1) (row + 1)
          go 0 0
    in \index ->
      let row = I# index
      in VP.unsafeIndex masks (row `quot` 64) .&. rowMask row /= 0


readMapLeaf :: PR.ParquetFile -> CheckpointGroup -> [Text] -> Int -> Either String MapLeaf
readMapLeaf pf rg@(CheckpointGroup _ group _) path emptyOffset =
  withCheckpointChunk pf rg path P.PTByteArray missingMapLeaf $ \metadata (ColumnLevels maxRep maxDef _) chunk -> do
    requireRepeated maxRep path
    let emptyDefinition = maxDef - emptyOffset
        rows = P.rgNumRows group
    constant <-
      if allNullEvents metadata rows
        then PR.columnChunkEmptyDefinition (P.cmCodec metadata) maxRep maxDef emptyDefinition rows chunk
        else Right Nothing
    case constant of
      Just level -> Right (ConstantEmptyMap (fromIntegral emptyDefinition) (fromIntegral level))
      Nothing ->
        MapLeaf (fromIntegral maxDef) (fromIntegral emptyDefinition)
          <$> readRepeatedTextLevels metadata maxRep maxDef emptyDefinition rows path chunk


mapLeavesAgree :: MapLeaf -> MapLeaf -> Bool
mapLeavesAgree (ConstantEmptyMap _ key) (ConstantEmptyMap _ value) = key == value
mapLeavesAgree (MapLeaf _ _ keys) (ConstantEmptyMap _ value) = constantDefinitions value keys
mapLeavesAgree (ConstantEmptyMap _ key) (MapLeaf _ _ values) = constantDefinitions key values
mapLeavesAgree (MapLeaf _ emptyDefinition keys) (MapLeaf _ _ values) =
  VP.length keyReps == VP.length valueReps && go 0
  where
    keyReps = PR.clRepetition keys
    valueReps = PR.clRepetition values
    keyDefs = PR.clDefinition keys
    valueDefs = PR.clDefinition values
    entryDefinition = emptyDefinition + 1
    go !i =
      i == VP.length keyDefs
        || ( VP.unsafeIndex keyReps i == VP.unsafeIndex valueReps i
               && VP.unsafeIndex keyDefs i == min entryDefinition (VP.unsafeIndex valueDefs i)
               && go (i + 1)
           )


constantDefinitions :: Int32 -> PR.ColumnLevels (V.Vector ByteString) -> Bool
constantDefinitions level values =
  VP.all (== level) (PR.clDefinition values) && VP.all (== 0) (PR.clRepetition values)


mapLeafRows :: Int -> MapLeaf -> RepeatedText
mapLeafRows _ (ConstantEmptyMap _ _) = emptyRepeatedText
mapLeafRows rows (MapLeaf maxDef emptyDef levels) = groupRepeatedText rows maxDef emptyDef levels


readRepeatedText
  :: PR.ParquetFile
  -> CheckpointGroup
  -> [Text]
  -> Int
  -> Either String RepeatedText
readRepeatedText pf rg path emptyOffset =
  readRepeatedTextWith pf rg path emptyOffset emptyOffset emptyRepeatedText $ \rowCount maxDefinition emptyDefinition levels ->
    Right (groupRepeatedText rowCount maxDefinition emptyDefinition levels)


readRepeatedTextWith
  :: PR.ParquetFile
  -> CheckpointGroup
  -> [Text]
  -> Int
  -> Int
  -> a
  -> (Int -> Int32 -> Int32 -> PR.ColumnLevels (V.Vector ByteString) -> Either String a)
  -> Either String a
readRepeatedTextWith pf rg@(CheckpointGroup _ group _) path emptyOffset absentOffset absent decode =
  withCheckpointChunk pf rg path P.PTByteArray absent $ \metadata (ColumnLevels maxRep maxDef _) chunk -> do
    requireRepeated maxRep path
    let rows = P.rgNumRows group
        emptyDefinition = maxDef - emptyOffset
    empty <- checkpointColumnEmpty metadata maxRep maxDef (maxDef - absentOffset) rows chunk
    if empty
      then Right absent
      else do
        levels <- readRepeatedTextLevels metadata maxRep maxDef emptyDefinition rows path chunk
        if V.null (PR.clValues levels) && VP.all (<= fromIntegral (maxDef - absentOffset)) (PR.clDefinition levels)
          then Right absent
          else decode (fromIntegral rows) (fromIntegral maxDef) (fromIntegral emptyDefinition) levels
{-# INLINE readRepeatedTextWith #-}


requireRepeated :: Int -> [Text] -> Either String ()
requireRepeated maxRep path =
  unless (maxRep == 1) $
    Left ("Delta.Checkpoint: expected a single repeated level in " ++ show path)


readRepeatedTextLevels
  :: P.ColumnMetadata -> Int -> Int -> Int -> Int64 -> [Text] -> ByteString
  -> Either String (PR.ColumnLevels (V.Vector ByteString))
readRepeatedTextLevels metadata maxRep maxDef emptyDefinition rows path chunk = do
  levels <- PR.readByteArrayLevels (P.cmCodec metadata) maxRep maxDef chunk
  let reps = PR.clRepetition levels
      defs = PR.clDefinition levels
      rowCount = VP.foldl' (\n rep -> if rep == 0 then n + 1 else n) (0 :: Int) reps
  unless (fromIntegral rowCount == rows) $
    Left ("Delta.Checkpoint: row count mismatch in " ++ show path)
  unless (fromIntegral (VP.length defs) == P.cmNumValues metadata) $
    Left ("Delta.Checkpoint: column value count mismatch in " ++ show path)
  unless (rowCount == VP.length reps || validContinuations reps defs (fromIntegral emptyDefinition)) $
    Left ("Delta.Checkpoint: repeated value continues an absent entry in " ++ show path)
  validateUtf8 path (V.all BS.isValidUtf8 (PR.clValues levels)) levels


validContinuations :: VP.Vector Int32 -> VP.Vector Int32 -> Int32 -> Bool
validContinuations reps defs emptyDefinition = go 1
  where
    go !i
      | i >= VP.length reps = True
      | VP.unsafeIndex reps i == 0 = go (i + 1)
      | VP.unsafeIndex defs (i - 1) > emptyDefinition && VP.unsafeIndex defs i > emptyDefinition = go (i + 1)
      | otherwise = False


-- The Parquet level reader validates the repetition stream and decodes
-- exactly as many values as there are maximum-definition entries.
groupRepeatedText
  :: Int
  -> Int32
  -> Int32
  -> PR.ColumnLevels (V.Vector ByteString)
  -> RepeatedText
groupRepeatedText rowCount maxDefinition emptyDefinition levels =
  case (VP.findIndex isEntry defs, VP.findIndexR isEntry defs) of
    (Just first, Just lastEntry) -> runST $ do
      -- Validated empty rows have one event each. Drop the empty prefix
      -- and suffix so a single metadata row costs one vector slot.
      let end = lastEntry + 1
          rowsInRange = rowCount - first - (VP.length defs - end)
      rows <- VM.replicate rowsInRange V.empty
      let nextRow !i
            | i == end || VP.unsafeIndex reps i == 0 = i
            | otherwise = nextRow (i + 1)
          fill entries !firstEvent !limit !i !valueIndex
            | i == limit = pure valueIndex
            | VP.unsafeIndex defs i == maxDefinition = do
                value <- V.unsafeIndexM values valueIndex
                VM.unsafeWrite entries (i - firstEvent) (Just value)
                fill entries firstEvent limit (i + 1) (valueIndex + 1)
            | otherwise = do
                VM.unsafeWrite entries (i - firstEvent) Nothing
                fill entries firstEvent limit (i + 1) valueIndex
          go !event !row !valueIndex
            | event == end = pure ()
            | isEntry (VP.unsafeIndex defs event) = do
                let !limit = nextRow (event + 1)
                entries <- VM.unsafeNew (limit - event)
                nextValue <- fill entries event limit event valueIndex
                frozen <- V.unsafeFreeze entries
                VM.unsafeWrite rows row frozen
                go limit (row + 1) nextValue
            | otherwise = go (event + 1) (row + 1) valueIndex
      go first 0 0
      RepeatedText first <$> V.unsafeFreeze rows
    _ -> emptyRepeatedText
  where
    reps = PR.clRepetition levels
    defs = PR.clDefinition levels
    values = PR.clValues levels
    isEntry = (> emptyDefinition)


validateUtf8 :: [Text] -> Bool -> a -> Either String a
validateUtf8 path valid value
  | valid = Right value
  | otherwise = Left ("Delta.Checkpoint: invalid UTF-8 in " ++ show path)
