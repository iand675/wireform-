module Main (main) where

import Arrow.Column qualified as AC
import Arrow.Types qualified as AT
import Columnar.Stream (IterIO, iterFoldM, iterIOFold)
import Control.DeepSeq (NFData (rnf))
import Criterion.Main
import Data.Aeson qualified as JSON
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Int (Int64)
import Data.Map.Internal qualified as MapInternal
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific, base10Exponent, coefficient, toBoundedInteger)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word64)
import Delta.Checkpoint (snapshotFromCheckpoint)
import Delta.IO
import Delta.Predicate qualified as Filter
import Delta.Scan qualified as Scan
import System.Environment (getEnv)
import System.FilePath (addTrailingPathSeparator, (</>))


main :: IO ()
main = do
  fixtures <- getEnv "WIREFORM_DELTA_BENCH_FIXTURES"
  let checkpoint = fixtures </> "checkpoint" </> "_delta_log" </> "00000000000000000020.checkpoint.parquet"
      statistics name count = bench name $ nfIO (checkStatistics count (fixtures </> name))
  defaultMain
    [ bench "json" $ nfIO (openSnapshotPaths (fixtures </> "json") 10_000)
    , bench "json-row-tracking" $ nfIO (openSnapshotPaths (fixtures </> "json-row-tracking") 10_000)
    , bench "compacted" $ nfIO (openSnapshotPaths (fixtures </> "compacted") 10_000)
    , bench "shuffled-compacted" $ nfIO (openSnapshotPaths (fixtures </> "shuffled-compacted") 10_000)
    , bench "json-history" $ nfIO (readHistory (fixtures </> "json"))
    , env (prepareChecksumFixture (fixtures </> "json")) $ \fixture ->
        bench "version-checksum-validation" $ nfIO (checkChecksum fixture)
    , bench "version-checksum-recovery" $ nfIO $ do
        table <- openDeltaTable (fixtures </> "version-checksum-recovery") >>= either fail pure
        pure (fileCount 10_000 (dtSnapshot table))
    , bench "version-checksum-line-boundary" $ nfIO $ do
        table <- openDeltaTable (fixtures </> "version-checksum-line-boundary") >>= either fail pure
        pure (fileCount 10_000 (dtSnapshot table))
    , bench "incremental-file-actions" $ nf countIncrementalFiles (map incrementalAdd [1 .. 10_000])
    , bench "checkpoint" $ nfIO (openSnapshotPaths (fixtures </> "checkpoint") 10_000)
    , bench "checkpoint-tail" $ nfIO (openSnapshotPaths (fixtures </> "checkpoint-tail") 10_100)
    , bench "checkpoint-long-history" $ nfIO (openSnapshotPaths (fixtures </> "checkpoint-long-history") 10_000)
    , bench "in-commit-timestamp-lookup" $ nfIO (checkTimestampLookup (fixtures </> "in-commit-timestamp-lookup"))
    , bench "checkpoint-sidecars" $ nfIO (openSnapshotPaths (fixtures </> "checkpoint-sidecars") 10_000)
    , statistics "checkpoint-ordered-repeated-stats" 10_000
    , statistics "checkpoint-shuffled-repeated-stats" 10_000
    , statistics "checkpoint-ordered-varied-stats" 49_995_000
    , statistics "checkpoint-shuffled-varied-stats" 49_995_000
    , bench "checkpoint-logical-stats-with-vectors" $
        nfIO (checkLogicalStatistics (fixtures </> "checkpoint-ordered-varied-stats"))
    , env (BS.readFile checkpoint) $ \bytes ->
        bench "checkpoint-in-memory" $ nf (either error (fileCount 10_000) . snapshotFromCheckpoint) bytes
    , bench "deletion-vector-scan" $ nfIO (scanIDs (fixtures </> "deletion-vector-scan"))
    , bench "deletion-vector-small-row-groups" $ nfIO (scanIDs (fixtures </> "deletion-vector-small-row-groups"))
    , bench "deletion-vector-fragmented-runs" $ nfIO (scanIDs (fixtures </> "deletion-vector-fragmented-runs"))
    , bench "deletion-vector-fully-deleted" $ nfIO (checkFullyDeleted (fixtures </> "deletion-vector-fully-deleted"))
    , bench "deletion-vector-count-only" $ nfIO (countPartitionRows (fixtures </> "deletion-vector-count-only"))
    , bench "mapped-table-scan" $ nfIO (scanMappedIDs (fixtures </> "mapped-table-scan"))
    , bench "change-feed" $ nfIO (countChangeFeed (fixtures </> "change-feed"))
    , bgroup "projection"
        [ bench "all-columns" $ nfIO (scanProjectionFixture Scan.scanTable (fixtures </> "projection"))
        , bench "id-only" $ nfIO (scanProjectionFixture (Scan.scanTableProjected ["id"]) (fixtures </> "projection"))
        ]
    , bgroup "predicate"
        [ bench "scan" $ nfIO (countPredicateFixture False (fixtures </> "projection"))
        , bench "prune" $ nfIO (countPredicateFixture True (fixtures </> "projection"))
        , bench "mapped-partition-prune" $ nfIO (countNonMatchingPartitions (fixtures </> "mapped-table-scan"))
        ]
    , bench "partition-text-scan" $ nfIO (scanPartitionText (fixtures </> "partition-text-scan"))
    , bench "row-tracking-scan" $ nfIO (scanTracked 4_360_437_760 (fixtures </> "row-tracking-scan"))
    , bench "row-tracking-mixed-scan" $ nfIO (scanTracked 7_059_510_613 (fixtures </> "row-tracking-mixed-scan"))
    , bench "timestamp-scan" $ nfIO (scanTimestamps (fixtures </> "timestamp-scan"))
    , bench "decimal-widening-scan" $ nfIO (scanWidenedDecimals (fixtures </> "decimal-widening-scan"))
    ]


-- Keep setup out of the measure and expose only the validated operation
-- to Criterion's environment forcing.
newtype ChecksumFixture = ChecksumFixture (IO Bool)


instance NFData ChecksumFixture where
  rnf (ChecksumFixture check) = check `seq` ()


prepareChecksumFixture :: FilePath -> IO ChecksumFixture
prepareChecksumFixture root = do
  table <- openDeltaTable root >>= either fail pure
  fileCount 10_000 (dtSnapshot table) `seq` pure (ChecksumFixture (validateVersionChecksum table >>= either fail pure))


checkChecksum :: ChecksumFixture -> IO Bool
checkChecksum (ChecksumFixture check) = do
  present <- check
  if present then pure True else fail "missing benchmark version checksum"


incrementalAdd :: Int -> DeltaAction
incrementalAdd n =
  let path = "part-" <> T.show n <> ".parquet"
  in ActionAdd (AddAction path 1 0 True Nothing Nothing Map.empty Map.empty Nothing Nothing Nothing)


countIncrementalFiles :: [DeltaAction] -> Int
countIncrementalFiles = fileCount 10_000 . foldl' applyAction emptySnapshot


readHistory :: FilePath -> IO Int
readHistory root = do
  commits <- findCommits root
  entries <- historyEntries (DeltaTable root Nothing Nothing commits emptySnapshot (Just 20))
  let versions = map heVersion entries
  if versions == [20, 19 .. 0] && all (maybe False (const True) . heTimestamp) entries
    then pure (length entries)
    else fail "unexpected recovered history"


openSnapshotPaths :: FilePath -> Int -> IO [T.Text]
openSnapshotPaths root expected = do
  result <- openDeltaTable root
  table <- either fail pure result
  let prefix = T.pack (addTrailingPathSeparator root)
  fileCount expected (dtSnapshot table) `seq`
    pure (map (prefix <>) (activeFilePaths table))


checkTimestampLookup :: FilePath -> IO Word64
checkTimestampLookup root = do
  table <- openDeltaTableAtTimestamp root (posixSecondsToUTCTime 11) >>= either fail pure
  case dtVersion table of
    Just 1000 -> pure 1000
    _ -> fail "unexpected commit selected by timestamp"


fileCount :: Int -> TableSnapshot -> Int
fileCount expected snapshot
  | actual == expected = actual
  | otherwise = error ("unexpected active file count: " ++ show actual)
  where
    actual = Map.size (tsFiles snapshot)


data StatisticsTotals = StatisticsTotals !Word64 !Word64 !Word64 !Word64
  deriving (Eq, Show)


checkStatistics :: Word64 -> FilePath -> IO Word64
checkStatistics expectedMinimums root = do
  table <- openDeltaTable root >>= either fail pure
  summarizeStatistics (maybe (Left "missing statistics") id . decodeAddStats) expectedMinimums table


checkLogicalStatistics :: FilePath -> IO Word64
checkLogicalStatistics root = do
  table <- openDeltaTable root >>= either fail pure
  let snapshot = dtSnapshot table
      -- The fixture has 16 physical rows per file. Exercise logical stats
      -- with a valid one-row deletion vector descriptor on every file.
      vector = JSON.object
        [ "storageType" JSON..= ("p" :: T.Text)
        , "pathOrInlineDv" JSON..= ("file:///vector.bin" :: T.Text)
        , "sizeInBytes" JSON..= (32 :: Int)
        , "cardinality" JSON..= (1 :: Int)
        ]
      protocol = ProtocolAction 3 7 ["deletionVectors"] ["deletionVectors"]
      withProtocol = table {dtSnapshot = snapshot {tsProtocol = Just protocol}}
  readStats <- either fail pure (Scan.tableFileStats withProtocol)
  let readFileStats file =
        readStats (file {addDeletionVector = Just vector}) >>= maybe (Left "missing statistics") Right
  summarizeStatistics readFileStats 49_995_000 withProtocol


summarizeStatistics :: (AddAction -> Either String AddStats) -> Word64 -> DeltaTable -> IO Word64
summarizeStatistics readStats expectedMinimums table = do
  let files = tsFiles (dtSnapshot table)
      expected = StatisticsTotals 160_000 expectedMinimums (expectedMinimums + 150_000) 0
  totals <- either fail pure $ summarize (StatisticsTotals 0 0 0 0) files
  if Map.size files == 10_000 && totals == expected
    then pure 160_000
    else fail ("unexpected checkpoint file count or statistics totals " ++ show (Map.size files) ++ " " ++ show totals)
  where
    -- Traverse in path order with a strict accumulator and stop on error.
    summarize !acc MapInternal.Tip = Right acc
    summarize !acc (MapInternal.Bin _ _ file left right) = do
      StatisticsTotals records minimums maximums nulls <- summarize acc left
      stats <- readStats file
      rows <- maybe (Left "missing numRecords") Right (asNumRecords stats)
      minimumValue <- bound "minValues.id" (asMinValues stats)
      maximumValue <- bound "maxValues.id" (asMaxValues stats)
      nullCount <- case HM.lookup "id" (asNullCounts stats) of
        Just (NullCount count) -> Right count
        _ -> Left "missing scalar nullCount.id"
      summarize (StatisticsTotals (records + rows) (minimums + minimumValue) (maximums + maximumValue) (nulls + nullCount)) right
    bound name values = case HM.lookup "id" values of
      Just (JSON.Number value) | Just n <- toWord64 value -> Right n
      _ -> Left ("missing or invalid " ++ name)
    toWord64 :: Scientific -> Maybe Word64
    toWord64 value
      | base10Exponent value == 0
      , let n = coefficient value
      , n >= 0 && n <= toInteger (maxBound :: Word64) = Just (fromInteger n)
      | otherwise = toBoundedInteger value


scanIDs :: FilePath -> IO Int64
scanIDs root = do
  table <- openDeltaTable root >>= either fail pure
  case dtActiveFiles table of
    [file] -> do
      (_, batches) <- Scan.openDataFile root file >>= either fail pure
      total <- either fail pure (iterFoldM (\n b -> (n +) <$> sumBatch b) 0 batches)
      if total == 1_879_048_192
        then pure total
        else fail ("unexpected scan sum " ++ show total)
    _ -> fail "expected one Parquet data file"


checkFullyDeleted :: FilePath -> IO Int
checkFullyDeleted root = do
  table <- openDeltaTable root >>= either fail pure
  case dtActiveFiles table of
    [file] -> do
      (_, batches) <- Scan.openDataFile root file >>= either fail pure
      total <- either fail pure (iterFoldM (\n batch -> Right $! n + Scan.batchRowCount batch) 0 batches)
      if total == 0 then pure total else fail "fully deleted file produced rows"
    _ -> fail "expected one Parquet data file"


countPartitionRows :: FilePath -> IO Int
countPartitionRows root = do
  table <- openDeltaTable root >>= either fail pure
  (_, batches) <- either fail pure (Scan.scanTable table)
  total <- iterIOFold (\n batch -> n + Scan.batchRowCount batch) 0 batches >>= either fail pure
  if total == 57_344 then pure total else fail "unexpected live partition row count"


scanProjectionFixture
  :: (DeltaTable -> Either String (AT.Schema, IterIO Scan.DataBatch))
  -> FilePath
  -> IO Int64
scanProjectionFixture scan root = do
  table <- openDeltaTable root >>= either fail pure
  (_, batches) <- either fail pure (scan table)
  total <- iterIOFold sumIDs 0 batches >>= either fail pure
  if total == 2_147_450_880 then pure total else fail "unexpected projection sum"
  where
    sumIDs total batch = case Scan.batchColumns batch V.! 0 of
      AC.ColInt64 Nothing ids -> total + VS.sum ids
      _ -> error "expected a non-null Int64 ID column"


countPredicateFixture :: Bool -> FilePath -> IO Int
countPredicateFixture prune root = do
  table <- openDeltaTable root >>= either fail pure
  let predicate = Filter.Column "value_16" [] (Filter.Compare Filter.Greater (Filter.PVInt64 limit))
      limit = if prune then 65_552 else 32_783
      expected = if prune then 0 else 32_768
  (_, batches) <- either fail pure (Scan.scanTableFiltered (Just []) predicate table)
  total <- iterIOFold (\n batch -> n + Scan.batchRowCount batch) 0 batches >>= either fail pure
  if total == expected then pure total else fail "unexpected predicate row count"


countNonMatchingPartitions :: FilePath -> IO Int
countNonMatchingPartitions root = do
  table <- openDeltaTable root >>= either fail pure
  let predicate = Filter.Column "region" [] (Filter.Compare Filter.Equal (Filter.PVText "asia"))
  (_, batches) <- either fail pure (Scan.scanTableFiltered (Just []) predicate table)
  total <- iterIOFold (\n batch -> n + Scan.batchRowCount batch) 0 batches >>= either fail pure
  if total == 0 then pure total else fail "partition predicate unexpectedly matched rows"


countChangeFeed :: FilePath -> IO Int
countChangeFeed root = do
  (_, batches) <- Scan.scanChangeDataFeed root 0 (Just 1) >>= either fail pure
  count <- iterIOFold (\n batch -> n + Scan.batchRowCount batch) 0 batches >>= either fail pure
  if count == 131_072 then pure count else fail "unexpected number of change rows"


scanMappedIDs :: FilePath -> IO Int64
scanMappedIDs root = do
  table <- openDeltaTable root >>= either fail pure
  (_, batches) <- either fail pure (Scan.scanTable table)
  result <- iterIOFold (\acc b -> (+) <$> acc <*> sumBatch b) (Right 0) batches
  total <- either fail pure (result >>= id)
  if total == 2_147_450_880
    then pure total
    else fail ("unexpected mapped scan sum " ++ show total)


sumBatch :: Scan.DataBatch -> Either String Int64
sumBatch batch = case Scan.batchColumns batch V.!? 0 of
  Just (AC.ColInt64 _ ids) -> Right (VS.sum ids)
  _ -> Left "expected an Int64 id column"


scanPartitionText :: FilePath -> IO Int
scanPartitionText root = do
  table <- openDeltaTable root >>= either fail pure
  (_, batches) <- either fail pure (Scan.scanTable table)
  let measure acc batch = case Scan.batchColumns batch V.!? 1 of
        Just column@(AC.ColUtf8 _ _ bytes)
          | AC.anyTextAt column (Scan.batchRowCount batch - 1) == Just "région-europe" ->
              (BS.length bytes +) <$> acc
        _ -> Left "unexpected partition column"
  total <- iterIOFold measure (Right 0) batches >>= either fail pure . (>>= id)
  if total == 65_536 * 14
    then pure total
    else fail ("unexpected partition byte count " ++ show total)


scanTimestamps :: FilePath -> IO Int64
scanTimestamps root = do
  table <- openDeltaTable root >>= either fail pure
  (_, batches) <- either fail pure (Scan.scanTable table)
  let addBatch acc batch = case Scan.batchColumns batch V.!? 0 of
        Just (AC.ColTimestamp _ values) -> case acc of
          Right n -> Right $! n + VS.sum values
          Left err -> Left err
        _ -> Left "expected a timestamp column"
  total <- iterIOFold addBatch (Right 0) batches >>= either fail pure . (>>= id)
  if total == 2_147_450_880_000
    then pure total
    else fail ("unexpected timestamp sum " ++ show total)


scanWidenedDecimals :: FilePath -> IO Int
scanWidenedDecimals root = do
  table <- openDeltaTable root >>= either fail pure
  (_, batches) <- either fail pure (Scan.scanTable table)
  let checkBatch acc batch = do
        start <- acc
        case V.toList (Scan.batchColumns batch) of
          [AC.ColDecimal128 38 2 _ same, AC.ColDecimal128 38 20 _ scaled] -> do
            let end = start + VS.length same - 1
                value = toInteger end - 32768
            if AC.decimal128ToInteger (VS.last same) == value
                && AC.decimal128ToInteger (VS.last scaled) == value * 10 ^ (18 :: Int)
              then Right $! end + 1
              else Left "unexpected widened decimal values"
          _ -> Left "expected two widened Decimal128 columns"
  count <- iterIOFold checkBatch (Right 0) batches >>= either fail pure . (>>= id)
  if count == 65_536
    then pure count
    else fail ("unexpected decimal row count " ++ show count)


scanTracked :: Int64 -> FilePath -> IO Int64
scanTracked expected root = do
  table <- openDeltaTable root >>= either fail pure
  (_, batches) <- either fail pure (Scan.scanTrackedTable table)
  let sumTracked acc b =
        (+ (VS.sum (Scan.rowIds b) + VS.sum (Scan.rowCommitVersions b))) <$> ((+) <$> acc <*> sumBatch (Scan.trackedData b))
  total <- iterIOFold sumTracked (Right 0) batches >>= either fail pure . (>>= id)
  if total == expected
    then pure total
    else fail ("unexpected tracked scan sum " ++ show total)
