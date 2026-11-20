{-# LANGUAGE BangPatterns #-}

-- | Read live rows from the local Parquet file of a Delta add action.
module Delta.Scan (
  DataBatch (..),
  TrackedBatch (..),
  scanChangeDataFeed,
  scanChangeDataFeedWithPartitionOffset,
  scanChangeDataFeedFiltered,
  openDataFile,
  scanTable,
  scanTableWithPartitionOffset,
  scanTableProjected,
  scanTableProjectedWithPartitionOffset,
  scanTableFiltered,
  scanTableFilteredWithPartitionOffset,
  scanTrackedTable,
  scanTrackedTableWithPartitionOffset,
  tableFileStats,
) where

import Arrow.Column qualified as AC
import Arrow.Column.Buffer (unsafeIsValidAt)
import Arrow.Types qualified as AT
import Columnar.Stream (Iter, IterIO (..), IterIOStep (..), IterStep (..), iterEmpty, iterStep, iterUnfold)
import Control.Exception (IOException, try)
import Control.Monad (foldM, unless, when)
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Semigroup (stimes)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (LocalTime)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word64)
import Delta.DeletionVector qualified as DV
import Delta.IO (DeltaCommit (..), DeltaTable (..), deltaCommitMillis, dtActiveFiles, openDeltaTable, openDeltaTableAt, streamDeltaCommits, supportsDeletionVectors, validateLocalSnapshot)
import Delta.Log (AddAction (..), AddStats (asNumRecords), CdcAction (..), DeltaAction (..), MetaDataAction (..), ProtocolAction (..), RemoveAction (..), TableSnapshot (..), decodeAddStats)
import Delta.ParquetSchema (schemaLeaves, validateChunks)
import Delta.SparkCalendar (SparkCalendar, sparkCalendar)
import Delta.Path (dataFilePath, filePathKey)
import Delta.Predicate qualified as Predicate
import Delta.ScanSchema qualified as Schema
import Delta.ScanPredicate qualified as Filter
import Parquet.Arrow qualified as PA
import Parquet.Read qualified as PR
import Parquet.Types qualified as P


data DataBatch = DataBatch
  { batchRowCount :: !Int
  , batchColumns :: !(V.Vector AC.ColumnArray)
  }


-- | Stable row metadata in the same order and length as the data batch.
data TrackedBatch = TrackedBatch
  { trackedData :: !DataBatch
  , rowIds :: !(VS.Vector Int64)
  , rowCommitVersions :: !(VS.Vector Int64)
  }


data LiveRows = LiveRows !Word64 !Int !(Maybe (VS.Vector Int))


physicalIndex :: Maybe (VS.Vector Int) -> Int -> Int
physicalIndex Nothing i = i
physicalIndex (Just live) i = VS.unsafeIndex live i
{-# INLINE physicalIndex #-}


type Project a =
  SparkCalendar -> AT.Schema -> Either String (AT.Schema, LiveRows -> V.Vector AC.ColumnArray -> Either String a)


data RowTracking = RowTracking
  { rowIdColumn :: !Text
  , rowVersionColumn :: !Text
  }


data RowMetadata = Generated | Materialized !(Maybe AC.Validity) !(VS.Vector Int64)


{- | The returned schema is the file's physical Arrow schema. The
iterator yields live rows in file order and skips fully deleted row
groups. An empty physical projection can split a row group into
smaller batches. Partition values, column mapping and schema evolution are
handled by the caller when projecting a table-wide schema.

A deletion vector is applied whenever the add action carries one,
independent of the table's current writer configuration.
-}
openDataFile :: FilePath -> AddAction -> IO (Either String (AT.Schema, Iter DataBatch))
openDataFile root file = openFile False root file (const []) $ \_ native -> pure (native, dataBatch (\_ -> Right))


{- | Stream a local table at the snapshot version. The schema uses Delta
display names. Missing fields are null, partitions come from the log,
and deletion vectors are applied before synthesis and conversion.
-}
scanTable :: DeltaTable -> Either String (AT.Schema, IterIO DataBatch)
scanTable = scanTableWithPartitionOffset (\_ _ -> Just 0)


{- | Scan with a resolver for legacy timestamp partitions that omit their
zone. Return the writer's offset in seconds east of UTC for the file
and local time, or 'Nothing' if it is unknown or ambiguous. Explicit
offsets and timezone-free @timestamp_ntz@ partitions are handled independently.
-}
scanTableWithPartitionOffset
  :: (AddAction -> LocalTime -> Maybe Int)
  -> DeltaTable
  -> Either String (AT.Schema, IterIO DataBatch)
scanTableWithPartitionOffset offset table = scanWithSchema offset table <$> compileTableSchema table


-- | Read only the selected top-level logical fields, in the requested
-- order. An empty projection still emits counts of live rows. Duplicate
-- names and names absent from the snapshot schema fail before iteration.
scanTableProjected :: [Text] -> DeltaTable -> Either String (AT.Schema, IterIO DataBatch)
scanTableProjected = scanTableProjectedWithPartitionOffset (\_ _ -> Just 0)


-- | Like 'scanTableProjected', with the partition clock resolver from
-- 'scanTableWithPartitionOffset'.
scanTableProjectedWithPartitionOffset
  :: (AddAction -> LocalTime -> Maybe Int)
  -> [Text]
  -> DeltaTable
  -> Either String (AT.Schema, IterIO DataBatch)
scanTableProjectedWithPartitionOffset offset names table = do
  schema <- compileTableSchema table >>= Schema.selectFields names
  pure (scanWithSchema offset table schema)


scanWithSchema
  :: (AddAction -> LocalTime -> Maybe Int)
  -> DeltaTable
  -> Schema.ScanSchema
  -> (AT.Schema, IterIO DataBatch)
scanWithSchema offset table schema =
  let project file calendar native = do
        (projected, restore) <- Schema.fileProjection (offset file) schema file calendar native
        pure (projected, dataBatch restore)
  in (Schema.outputSchema schema, scanFiles False table schema project)


-- | Stream change rows over an inclusive version range. An omitted end
-- fixes the range at the latest observed version. The output uses the
-- end snapshot's schema followed by the three Delta CDF columns.
scanChangeDataFeed :: FilePath -> Word64 -> Maybe Word64 -> IO (Either String (AT.Schema, IterIO DataBatch))
scanChangeDataFeed = scanChangeDataFeedWithPartitionOffset (\_ _ -> Just 0)


-- | Like 'scanChangeDataFeed', with a resolver for legacy timestamp partitions.
scanChangeDataFeedWithPartitionOffset
  :: (AddAction -> LocalTime -> Maybe Int)
  -> FilePath -> Word64 -> Maybe Word64
  -> IO (Either String (AT.Schema, IterIO DataBatch))
scanChangeDataFeedWithPartitionOffset offset root start end = do
  loaded <- maybe (openDeltaTable root) (openDeltaTableAt root) end
  case loaded >>= feedSchema of
    Left err -> pure (Left err)
    Right (schema, target) -> do
      commits <- streamDeltaCommits root start target
      let output = Schema.outputSchema schema
      pure ((output {AT.arrowFields = AT.arrowFields output V.++ markers},) . stepCommits schema <$> commits)
  where
    markers = V.fromList
      [ AT.defaultLeafField "_change_type" False AT.AUtf8
      , AT.defaultLeafField "_commit_version" False (AT.AInt 64 True)
      , AT.defaultLeafField "_commit_timestamp" False (AT.ATimestamp AT.Microsecond (Just "UTC"))
      ]
    feedSchema table = do
      target <- maybe (Left "Delta.Scan: change data feed requires a committed version") Right (dtVersion table)
      schema <- compileTableSchema table
      let names = Set.fromList (map AT.fieldName (V.toList markers))
      unless (V.all (\field -> Set.notMember (T.toCaseFold (AT.fieldName field)) names) (AT.arrowFields (Schema.outputSchema schema))) $
        Left "Delta.Scan: table field collides with change data feed metadata"
      pure (schema, target)
    stepCommits schema commits = IterIO $ iterIOStep commits >>= \case
      Left err -> pure (Left err)
      Right IterIODone -> pure (Right IterIODone)
      Right (IterIOYield entry rest) ->
        let continue = stepCommits schema rest
        in case changesFor schema entry of
          Left err -> pure (Left (dcPath entry ++ ": " ++ err))
          Right [] -> iterIOStep continue
          Right files -> do
            timestamp <- deltaCommitMillis entry
            case timestamp >>= millisToMicros of
              Left err -> pure (Left (dcPath entry ++ ": " ++ err))
              Right micros -> iterIOStep (scanChanges schema entry micros files continue)
    changesFor schema entry = do
      files <- fileChanges entry
      let before = dcBefore entry
          after = dcAfter entry
          crossesChange = dcVersion entry > start || any readsBefore files
      let previous = if crossesChange && dcSchemaChanged entry then Just before else Nothing
      Schema.validateChangeSchema schema after previous
      pure files
    readsBefore (ChangeFile _ Deletion _) = True
    readsBefore _ = False
    scanChanges _ _ _ [] rest = rest
    scanChanges schema entry micros (ChangeFile file changeKind exclude : files) rest = IterIO $ do
      let go = scanChanges schema entry micros files rest
          project calendar native = do
            (projected, restore) <- case changeKind of
              FromCdc -> Schema.changeFileProjection (offset file) schema file calendar native
              _ -> Schema.fileProjection (offset file) schema file calendar native
            pure (projected, \live physical -> do
              batch <- dataBatch restore live physical
              appendChangeColumns (fromIntegral (dcVersion entry)) micros changeKind batch)
      case validateFileDeletionVector (supportsDeletionVectors (dcBefore entry) || supportsDeletionVectors (dcAfter entry)) file of
        Left err -> pure (Left (fileError file err))
        Right () -> do
          opened <- openFileSelection False root file (Schema.legacyVariantPaths schema) project exclude
          case opened of
            Left err -> pure (Left (fileError file err))
            Right (_, batches) -> iterIOStep (drainFile file batches go)


-- | Filter change rows by logical fields or CDF metadata columns.
scanChangeDataFeedFiltered
  :: Predicate.Predicate
  -> FilePath -> Word64 -> Maybe Word64
  -> IO (Either String (AT.Schema, IterIO DataBatch))
scanChangeDataFeedFiltered predicate root start end = do
  loaded <- scanChangeDataFeed root start end
  pure $ do
    (schema, batches) <- loaded
    prepare <- Filter.preparePredicate schema predicate
    pure (schema, filterBatches (V.length (AT.arrowFields schema)) prepare batches)


millisToMicros :: Int64 -> Either String Int64
millisToMicros millis
  | millis < minBound `quot` 1000 || millis > maxBound `quot` 1000 = Left "Delta.Scan: commit timestamp overflows Arrow microseconds"
  | otherwise = Right (millis * 1000)


data ChangeKind = FromCdc | Insertion | Deletion


data ChangeFile = ChangeFile !AddAction !ChangeKind !(Maybe AddAction)


fileChanges :: DeltaCommit -> Either String [ChangeFile]
fileChanges entry = do
  let after = dcAfter entry
      actions = dcFileActions entry
      cdcs = foldr (\action rest -> case action of ActionCdc cdc -> cdc : rest; _ -> rest) [] actions
      config = tsMetaData after >>= Map.lookup "delta.enableChangeDataFeed" . mdConfiguration
      enabled protocol = case pMinWriterVersion protocol of
        7 -> "changeDataFeed" `elem` pWriterFeatures protocol
        version -> version >= 4 && version <= 6
  unless (config == Just "true" && maybe False enabled (tsProtocol after)) $
    Left "Delta.Scan: change data feed was disabled at this version"
  if null cdcs then reverse <$> foldM inferred [] actions else traverse cdcFile cdcs
  where
    prior = tsFiles (dcBefore entry)
    current = tsFiles (dcAfter entry)
    cdcFile cdc = do
      when (cdcDataChange cdc) (Left "Delta.Scan: a cdc action must have dataChange=false")
      let added = AddAction (cdcPath cdc) (cdcSize cdc) 0 False Nothing Nothing (cdcPartitionValues cdc) (cdcTags cdc) Nothing Nothing Nothing
      pure (ChangeFile added FromCdc Nothing)
    inferred rest (ActionAdd add) | addDataChange add =
      pure (ChangeFile add Insertion (Map.lookup (filePathKey (addPath add)) prior) : rest)
    inferred rest (ActionRemove remove) | removeDataChange remove = do
      let key = filePathKey (removePath remove)
      previous <- maybe (Left ("Delta.Scan: cannot reconstruct removed file " ++ show (removePath remove))) Right (Map.lookup key prior)
      actual <- traverse DV.deletionVectorIdentity (addDeletionVector previous)
      expected <- traverse DV.deletionVectorIdentity (removeDeletionVector remove)
      unless (actual == expected) (Left "Delta.Scan: remove does not match the prior logical file")
      pure (ChangeFile previous Deletion (Map.lookup key current) : rest)
    inferred rest _ = pure rest


appendChangeColumns :: Int64 -> Int64 -> ChangeKind -> DataBatch -> Either String DataBatch
appendChangeColumns version micros kind (DataBatch n columns) = do
  typed <- case kind of
    FromCdc -> do
      let marker = V.last columns
      unless (validMarkers marker 0) (Left "Delta.Scan: invalid _change_type value")
      Right columns
    Insertion -> withLabel "insert"
    Deletion -> withLabel "delete"
  versions <- AC.mkPrim AC.PInt64 Nothing (VS.replicate n version)
  timestamps <- AC.mkPrim AC.PTimestamp Nothing (VS.replicate n micros)
  pure (DataBatch n (V.snoc (V.snoc typed versions) timestamps))
  where
    withLabel label = do
      unless (n <= fromIntegral (maxBound :: Int32) `quot` 6) (Left "Delta.Scan: CDC marker offsets exceed Int32")
      marker <- AC.mkUtf8 Nothing (VS.generate (n + 1) (fromIntegral . (* 6))) (if n == 0 then mempty else stimes n label)
      pure (V.snoc columns marker)
    validMarkers marker !i = i == n || case AC.anyBytesAt marker i of
      Just label | label `elem` ["insert", "delete", "update_preimage", "update_postimage"] -> validMarkers marker (i + 1)
      _ -> False


-- | Filter live rows with a predicate over logical display names. The
-- optional projection orders output columns independently of the columns
-- tested by the predicate. Only rows for which the predicate is true are
-- emitted. A null comparison can remain unknown under negation.
scanTableFiltered :: Maybe [Text] -> Predicate.Predicate -> DeltaTable -> Either String (AT.Schema, IterIO DataBatch)
scanTableFiltered = scanTableFilteredWithPartitionOffset (\_ _ -> Just 0)


-- | Like 'scanTableFiltered', with a resolver for legacy timestamp partitions.
scanTableFilteredWithPartitionOffset
  :: (AddAction -> LocalTime -> Maybe Int)
  -> Maybe [Text]
  -> Predicate.Predicate
  -> DeltaTable
  -> Either String (AT.Schema, IterIO DataBatch)
scanTableFilteredWithPartitionOffset offset outputNames predicate table = do
  schema <- compileTableSchema table
  output <- maybe (Right schema) (`Schema.selectFields` schema) outputNames
  let outputSchema = Schema.outputSchema output
      outputCount = V.length (AT.arrowFields outputSchema)
      names = map AT.fieldName (V.toList (AT.arrowFields outputSchema))
      predicateNames = Set.fromList (Filter.predicateColumns predicate)
      extraNames = Set.toList (predicateNames `Set.difference` Set.fromList names)
  input <- if null extraNames then Right output else Schema.selectFields (names ++ extraNames) schema
  prepare <- Filter.preparePredicate (Schema.outputSchema input) predicate
  statsSchema <- Schema.selectFields (Set.toList predicateNames) schema
  let project file calendar native = do
        (projected, restore) <- Schema.fileProjection (offset file) input file calendar native
        pure (projected, dataBatch restore)
      keepFile = Filter.prepareFilePruning offset statsSchema predicate
  pure (outputSchema, filterBatches outputCount prepare (scanFilesMatching keepFile False table input project))


filterBatches :: Int -> Filter.Prepared -> IterIO DataBatch -> IterIO DataBatch
filterBatches outputCount prepare = go
  where
    go iterator = IterIO $ iterIOStep iterator >>= \case
      Left err -> pure (Left err)
      Right IterIODone -> pure (Right IterIODone)
      Right (IterIOYield (DataBatch count columns) next) ->
        let picked = V.take outputCount columns
            advance = iterIOStep (go next)
            emit n cols = Right (IterIOYield (DataBatch n cols) (go next))
        in case prepare columns of
          Left err -> pure (Left err)
          Right test
            | outputCount == 0 -> case Filter.countRows test count of
                0 -> advance
                n -> pure (emit n V.empty)
            | otherwise -> case Filter.selectRows test count of
                Filter.None -> advance
                Filter.All -> pure (emit count picked)
                Filter.Contiguous start n -> pure (emit n (V.map (AC.sliceColumnArray start n) picked))
                Filter.Selected rows -> pure (V.mapM (AC.takeColumnArray rows) picked >>= emit (VS.length rows))


{- | Like 'scanTable', with stable row IDs and commit versions. Hidden
materialized values take precedence over the file defaults.
-}
scanTrackedTable :: DeltaTable -> Either String (AT.Schema, IterIO TrackedBatch)
scanTrackedTable = scanTrackedTableWithPartitionOffset (\_ _ -> Just 0)


-- | Scan with row tracking and a resolver for legacy partition timestamps.
-- See 'scanTableWithPartitionOffset' for the resolver contract.
scanTrackedTableWithPartitionOffset
  :: (AddAction -> LocalTime -> Maybe Int)
  -> DeltaTable
  -> Either String (AT.Schema, IterIO TrackedBatch)
scanTrackedTableWithPartitionOffset offset table = do
  schema <- compileTableSchema table
  tracking <- rowTracking (dtSnapshot table)
  Schema.validateHiddenColumns schema (rowIdColumn tracking) (rowVersionColumn tracking)
  pure (Schema.outputSchema schema, scanFiles True table schema (trackedProjection offset schema tracking))


scanFiles :: Bool -> DeltaTable -> Schema.ScanSchema -> (AddAction -> Project a) -> IterIO a
scanFiles = scanFilesMatching (const True)
{-# INLINE scanFiles #-}


scanFilesMatching :: (AddAction -> Bool) -> Bool -> DeltaTable -> Schema.ScanSchema -> (AddAction -> Project a) -> IterIO a
scanFilesMatching keepFile wantsPositions table schema project = nextFile (dtActiveFiles table)
  where
    nextFile [] = IterIO (pure (Right IterIODone))
    nextFile (file : rest) = IterIO $
      case checkDeletionVector file of
        Left err -> pure (Left (fileError file err))
        Right () | not (keepFile file) -> iterIOStep (nextFile rest)
        Right () -> do
          result <- openFile wantsPositions (dtRoot table) file (Schema.legacyVariantPaths schema) (project file)
          case result of
            Left err -> pure (Left (fileError file err))
            Right (_, batches) -> iterIOStep (drainFile file batches (nextFile rest))
    checkDeletionVector = validateFileDeletionVector (supportsDeletionVectors (dtSnapshot table))


drainFile :: AddAction -> Iter a -> IterIO a -> IterIO a
drainFile file batches rest = IterIO $ case iterStep batches of
  Left err -> pure (Left (fileError file err))
  Right IterDone -> iterIOStep rest
  Right (IterYield batch next) -> pure (Right (IterIOYield batch (drainFile file next rest)))


fileError :: AddAction -> String -> String
fileError file err = "Delta.Scan: " ++ show (addPath file) ++ ": " ++ err


dataBatch
  :: (Int -> V.Vector AC.ColumnArray -> Either String (V.Vector AC.ColumnArray))
  -> LiveRows
  -> V.Vector AC.ColumnArray
  -> Either String DataBatch
dataBatch restore (LiveRows _ n selection) physical = do
  columns <- case selection of
    Nothing -> Right physical
    Just live -> V.mapM (AC.takeColumnArray live) physical
  DataBatch n <$> restore n columns


rowTracking :: TableSnapshot -> Either String RowTracking
rowTracking snapshot = case (tsProtocol snapshot, tsMetaData snapshot) of
  (Just protocol, Just meta)
    | pMinWriterVersion protocol == 7
    , all (`elem` pWriterFeatures protocol) ["rowTracking", "domainMetadata"]
    , let config = mdConfiguration meta
    , Map.lookup "delta.enableRowTracking" config == Just "true"
    , Map.lookup "delta.rowTrackingSuspended" config /= Just "true" -> do
        idName <- columnName config "delta.rowTracking.materializedRowIdColumnName"
        versionName <- columnName config "delta.rowTracking.materializedRowCommitVersionColumnName"
        unless (idName /= versionName) (Left "Delta.Scan: row tracking column names must be distinct")
        pure (RowTracking idName versionName)
  _ -> Left "Delta.Scan: row tracking is not enabled on this snapshot"
  where
    columnName config key = case Map.lookup key config of
      Just name | name /= "" -> Right name
      _ -> Left ("Delta.Scan: missing row tracking property " ++ show key)


trackedProjection :: (AddAction -> LocalTime -> Maybe Int) -> Schema.ScanSchema -> RowTracking -> AddAction -> Project TrackedBatch
trackedProjection offset logical tracking file calendar native = do
  base <- maybe (Left "Delta.Scan: missing baseRowId") Right (addBaseRowId file)
  version <- maybe (Left "Delta.Scan: missing defaultRowCommitVersion") Right (addDefaultRowCommitVersion file)
  (projected, restore) <- Schema.fileProjection (offset file) logical file calendar native
  idField <- hiddenField (rowIdColumn tracking)
  versionField <- hiddenField (rowVersionColumn tracking)
  let userFields = AT.arrowFields projected
      hidden = V.fromList (maybe [] pure idField ++ maybe [] pure versionField)
      userCount = V.length userFields
      versionIndex = userCount + maybe 0 (const 1) idField
      physical = projected {AT.arrowFields = userFields V.++ hidden}
      assemble rows@(LiveRows start n indices) cols = do
        values <- dataBatch restore rows (V.take userCount cols)
        storedIds <- hiddenColumn idField userCount cols
        storedVersions <- hiddenColumn versionField versionIndex cols
        ids <- stableRowIds n base start indices storedIds
        let versions = fillRowMetadata n (const version) indices storedVersions
        pure (TrackedBatch values ids versions)
  unless (V.all (\field -> AT.fieldName field /= rowIdColumn tracking && AT.fieldName field /= rowVersionColumn tracking) userFields) $
    Left "Delta.Scan: hidden row tracking column is also a data column"
  pure (physical, assemble)
  where
    hiddenField name = case V.find ((== name) . AT.fieldName) (AT.arrowFields native) of
      Nothing -> Right Nothing
      Just f | AT.fieldType f == AT.AInt 64 True -> Right (Just f)
      _ -> Left ("Delta.Scan: row tracking column must be INT64: " ++ show name)


hiddenColumn
  :: Maybe AT.Field
  -> Int
  -> V.Vector AC.ColumnArray
  -> Either String RowMetadata
hiddenColumn field index columns = case field of
  Nothing -> Right Generated
  Just _ -> case columns V.!? index of
    Just (AC.ColInt64 valid values) -> Right (Materialized valid values)
    _ -> Left "Delta.Scan: missing projected row tracking column"


stableRowIds :: Int -> Int64 -> Word64 -> Maybe (VS.Vector Int) -> RowMetadata -> Either String (VS.Vector Int64)
stableRowIds n base start selection materialized = do
  -- File positions are nonnegative and bounded by the signed footer row count.
  -- If the last position would overflow, only null slots need the fallback.
  when (n > 0 && overflows (physicalIndex selection (n - 1))) $
    case materialized of
      Materialized Nothing _ -> Right ()
      Materialized valid _ -> checkFrom valid (n - 1)
      Generated -> overflow
  pure (fillRowMetadata n (\i -> base + position i) selection materialized)
  where
    position i = fromIntegral start + fromIntegral i
    overflows i = base > maxBound - position i
    overflow = Left "Delta.Scan: row ID exceeds signed 64-bit range"
    checkFrom valid i
      | i < 0 || not (overflows source) = Right ()
      | unsafeIsValidAt valid source = checkFrom valid (i - 1)
      | otherwise = overflow
      where
        source = physicalIndex selection i
{-# INLINE stableRowIds #-}


-- Gather stored metadata and fill defaults in one pass over the live rows.
fillRowMetadata :: Int -> (Int -> Int64) -> Maybe (VS.Vector Int) -> RowMetadata -> VS.Vector Int64
fillRowMetadata n fallback selection = \case
  Generated -> generated
  Materialized valid values
    | Just mask <- valid, AC.validityNullCount mask == VS.length values ->
        generated
    | otherwise -> case selection of
        Nothing -> case valid of
          Nothing -> values
          Just _ -> VS.imap (\i value -> if unsafeIsValidAt valid i then value else fallback i) values
        Just live -> VS.generate n $ \i ->
          let source = VS.unsafeIndex live i
          in if unsafeIsValidAt valid source then VS.unsafeIndex values source else fallback source
  where
    generated = VS.generate n (fallback . physicalIndex selection)
{-# INLINE fillRowMetadata #-}


-- | Resolve file statistics to the snapshot's logical field names.
-- Null counts must match the column shape and cannot exceed the applicable
-- row count. For wide statistics, only all-null and no-null counts are useful.
-- Bounds must match the column type and fit its value range. Bounds for
-- columns known to contain only nulls are omitted.
-- Partition columns use the values recorded in the Add action and are
-- excluded from the returned statistics.
-- Files with deletion vectors require a physical record count at least as
-- large as the vector's cardinality.
-- Floating-point bounds and millisecond-aligned timestamp maxima may be
-- expanded for safe file pruning.
-- Zoned timestamp bounds without an explicit offset are omitted.
tableFileStats :: DeltaTable -> Either String (AddAction -> Either String (Maybe AddStats))
tableFileStats table = do
  schema <- compileTableSchema table
  let checkDeletionVector = validateFileDeletionVector (supportsDeletionVectors (dtSnapshot table))
  pure $ \file -> do
    checkDeletionVector file
    Schema.logicalFileStats schema file


validateFileDeletionVector :: Bool -> AddAction -> Either String ()
validateFileDeletionVector supported file
  | not supported && isJust (addDeletionVector file) =
      Left "Delta.Scan: deletion vector requires the deletionVectors reader and writer feature (protocol 3/7)"
  | otherwise = Right ()
{-# INLINE validateFileDeletionVector #-}


compileTableSchema :: DeltaTable -> Either String Schema.ScanSchema
compileTableSchema table = do
  snapshot <- validateLocalSnapshot (dtSnapshot table)
  case tsMetaData snapshot of
    Nothing -> Left "Delta.Scan: snapshot has no table metadata"
    Just metadata -> Schema.scanSchema (tsProtocol snapshot) metadata


openFile
  :: Bool
  -> FilePath
  -> AddAction
  -> (AT.Schema -> [[Text]])
  -> Project a
  -> IO (Either String (AT.Schema, Iter a))
openFile wantsPositions root file legacyVariants project = openFileSelection wantsPositions root file legacyVariants project Nothing


openFileSelection
  :: Bool
  -> FilePath
  -> AddAction
  -> (AT.Schema -> [[Text]])
  -> Project a
  -> Maybe AddAction
  -> IO (Either String (AT.Schema, Iter a))
openFileSelection wantsPositions root file legacyVariants project exclude = case dataFilePath root (addPath file) of
  Left err -> pure (Left err)
  Right path -> do
    loaded <- try @IOException (PR.loadParquetFilePath path)
    case either (Left . show) id loaded of
      Left err -> pure (Left err)
      Right parquet -> do
        vector <- traverse (DV.readDeletionVector root) (addDeletionVector file)
        other <- traverse (DV.readDeletionVector root) (exclude >>= addDeletionVector)
        pure $ do
          bitmap <- sequence vector
          otherBitmap <- sequence other
          when (isJust bitmap) $
            validateDeletionVectorRowCount file parquet
          case exclude of
            Just previous | isJust otherBitmap -> validateDeletionVectorRowCount previous parquet
            _ -> pure ()
          scanFile wantsPositions legacyVariants project parquet bitmap (otherBitmap <$ exclude)


validateDeletionVectorRowCount :: AddAction -> PR.ParquetFile -> Either String ()
validateDeletionVectorRowCount file parquet = do
  stats <- sequence (decodeAddStats file)
  let physicalRows = P.fmNumRows (PR.pfFooter parquet)
  case stats >>= asNumRecords of
    Nothing -> Left "Delta.Scan: a file with a deletion vector requires numRecords"
    Just rows -> unless (physicalRows >= 0 && rows == fromIntegral physicalRows) $
      Left "Delta.Scan: numRecords disagrees with the Parquet physical row count"


scanFile
  :: Bool
  -> (AT.Schema -> [[Text]])
  -> Project a
  -> PR.ParquetFile
  -> Maybe DV.DeletionVector
  -> Maybe (Maybe DV.DeletionVector)
  -> Either String (AT.Schema, Iter a)
scanFile wantsPositions legacyVariants project parquet bitmap exclude = do
  let footer = PR.pfFooter parquet
      groups = P.fmRowGroups footer
      rowCount = P.fmNumRows footer
      schemaError = either (Left . ("Delta.Scan: " ++)) Right
      validate checkChunks !total group = do
        let n = P.rgNumRows group
        unless (n >= 0 && n <= rowCount - total && toInteger n <= toInteger (maxBound :: Int)) $
          Left "Delta.Scan: invalid Parquet row-group size"
        () <- schemaError (checkChunks (P.rgColumns group))
        pure (total + n)
  when (rowCount < 0) (Left "Delta.Scan: negative Parquet row count")
  leaves <- schemaError (schemaLeaves (P.fmSchema footer))
  total <- V.foldM' (validate (validateChunks (P.fmSchema footer) leaves)) 0 groups
  unless (total == rowCount) (Left "Delta.Scan: Parquet row counts disagree")
  case bitmap >>= DV.lastDeletedRow of
    Just i | i >= fromIntegral rowCount -> Left "Delta.Scan: deleted row outside the Parquet file"
    _ -> pure ()
  case exclude >>= id >>= DV.lastDeletedRow of
    Just i | i >= fromIntegral rowCount -> Left "Delta.Scan: deleted row outside the Parquet file"
    _ -> pure ()
  (physical, inferred) <- PA.parquetFileArrowSchemaWithLegacyVariants legacyVariants parquet
  (schema, restore) <- project (sparkCalendar footer) inferred
  let readGroup = PA.prepareParquetRowGroupReader schema physical
      step (!groupIndex, !start, !offset)
        | Just group <- groups V.!? groupIndex = do
            let remaining = fromIntegral (P.rgNumRows group) - offset
                n = if noColumns then min 65_536 remaining else remaining
                next =
                  if n == remaining
                    then (groupIndex + 1, start + fromIntegral n, 0)
                    else (groupIndex, start + fromIntegral n, offset + n)
                columns =
                  if noColumns
                    then Right V.empty
                    else either (Left . show) Right (readGroup >>= ($ groupIndex))
            if n == 0
              then step next
              else if countOnly
                then do
                  live <- maybe (Right n) (\dv -> DV.countLiveRows dv start n) bitmap
                  if live == 0
                    then step next
                    else do
                      batch <- restore (LiveRows start live Nothing) V.empty
                      pure (Just (batch, next))
                else do
                  selection <- case exclude of
                    Nothing -> maybe (Right DV.AllRows) (\dv -> DV.selectLiveRows dv start n) bitmap
                    Just Nothing -> Right DV.NoRows
                    Just (Just other) -> do
                      changed <- DV.selectDeletedRows other start n
                      case bitmap of
                        Nothing -> Right changed
                        Just invalid -> DV.subtractRowSelection n changed <$> DV.selectDeletedRows invalid start n
                  case selection of
                    DV.NoRows -> step next
                    DV.AllRows -> do
                      batch <- columns >>= restore (LiveRows start n Nothing)
                      pure (Just (batch, next))
                    DV.SelectedRows live -> do
                      batch <- columns >>= restore (LiveRows start (VS.length live) (Just live))
                      pure (Just (batch, next))
        | otherwise = Right Nothing
      -- A tiny columnless file can declare a large row group. Bound the
      -- work in each pull when all output comes from row counts and the log.
      noColumns = V.null (AT.arrowFields schema)
      countOnly = noColumns && not wantsPositions && not (isJust exclude)
      -- Cardinality is checked against the decoded bitmap, and every
      -- deleted position is within the file after the last-row check.
      rows = case bitmap of
        Just dv | DV.deletionCardinality dv == fromIntegral rowCount -> iterEmpty
        _ -> iterUnfold (0, 0, 0) step
  pure (schema, rows)
