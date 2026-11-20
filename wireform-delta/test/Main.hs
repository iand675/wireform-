{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for Delta log decoding and snapshot replay.

These cover the public surface — action variants, the
'TableSnapshot' replay, the schema-string decoder, and the
@_last_checkpoint@ parser — without trying to test things
inherent to aeson (e.g. \"ints round-trip through JSON\").
-}
module Main (main) where

import Arrow.Column qualified as AC
import Arrow.Types qualified as AT
import Columnar.Stream (iterIOTake, iterIOToList, iterToList)
import Control.Monad (foldM, forM_)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.HashMap.Strict qualified as HM
import Data.Int (Int64)
import Data.List (isInfixOf, sort, zip4)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Vector.Storable qualified as VS
import Data.Word (Word64)
import Delta.Checkpoint qualified as Checkpoint
import Delta.DeletionVector qualified as DV
import Delta.IO qualified as DIO
import Delta.Log qualified as D
import Delta.Predicate qualified as Predicate
import Delta.Scan qualified as Scan
import GHC.Records (getField)
import Parquet.Footer qualified as PF
import Parquet.LevelsEncode qualified as LE
import Parquet.Nested qualified as PN
import Parquet.Page qualified as PP
import Parquet.Read qualified as PR
import Parquet.Timestamp qualified as TS
import Parquet.Types qualified as PT
import Parquet.Variant qualified as Variant
import Parquet.Variant.Arrow qualified as VA
import Parquet.Write qualified as PW
import System.Directory (canonicalizePath, createDirectory, createFileLink, removeDirectory, removeFile, setModificationTime)
import System.FilePath (pathSeparator, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-delta" $
      sequence_
        [ it "parseLogLine: add" addLineTest
        , it "checked add replay agrees with Aeson across JSON representations" addReplayDecoderTest
        , it "parseLogLine: remove" removeLineTest
        , it "parseLogLine: protocol" protocolLineTest
        , it "parseLogLine: metaData" metaDataLineTest
        , it "parseLogLine: commitInfo" commitInfoLineTest
        , it "replays commits with free-form provenance using conventional field names" freeFormCommitInfoTest
        , it "parseLogLine: txn" txnLineTest
        , it "parseLogLine: cdc" cdcLineTest
        , it "normalizes empty partition values across file actions" emptyPartitionValueTest
        , it "preserves signed timestamps in log actions" signedTimestampsTest
        , it "enforces Delta Long bounds for JSON file sizes, counts, and checkpoint versions" jsonLongBoundsTest
        , it "rejects exponent overflow before decoding Delta JSON numbers" jsonExponentOverflowTest
        , it "rejects ambiguous JSON keys across Delta readers" duplicateJsonKeysTest
        , it "snapshot fold collapses add+remove" snapshotFoldTest
        , it "replays deletion-vector replacements in either action order" deletionVectorReplayTest
        , it "checks deletion-vector replacements in numbered commits and compacted ranges" segmentFileActionsTest
        , it "rejects duplicate metadata actions within a log segment" segmentMetadataActionsTest
        , it "validates table schemas during JSON replay and bypasses an invalid compaction" segmentSchemaRecoveryTest
        , it "loads the protocol's inline deletion vector" inlineDeletionVectorTest
        , it "accepts Spark's empty inline deletion vector in logs and checkpoints" emptyDeletionVectorTest
        , it "selects live rows across Roaring containers and 32-bit buckets" deletionVectorSelectionTest
        , it "rejects malformed deletion vectors" invalidDeletionVectorTest
        , it "uses the last signed transaction version for each app" transactionReplayTest
        , it "snapshot reflects metadata + protocol" snapshotProtoMetaTest
        , it "schemaString decodes nested types" schemaTest
        , it "preserves nested schema metadata and decodes void and variant" nestedSchemaMetadataTest
        , it "rejects incomplete fields and out-of-range decimals" malformedSchemaTest
        , it "checks case-insensitive field names independently in each struct" schemaFieldNamesTest
        , it "_last_checkpoint parser" lastCheckpointTest
        , it "rejects duplicate keys in _last_checkpoint, including nested and escaped keys" lastCheckpointDuplicateKeysTest
        , it "validates Delta's optional _last_checkpoint checksum before exposing the pointer" lastCheckpointChecksumTest
        , it "replays complete JSON history when the checkpoint is corrupt" corruptCheckpointRecoveryTest
        , it "recovers when the checkpoint pointer counts more adds than the Parquet checkpoint" checkpointPointerAddCountTest
        , it "checks V2 add-file counts against the UUID-named manifest in the pointer" v2CheckpointPointerTest
        , it "ignores checkpoint-shaped directories and incomplete multipart sets" checkpointDiscoveryFilesTest
        , it "ignores directories and dangling links named like log files" logDiscoveryFilesTest
        , it "ignores on-disk log versions above the signed Long limit" logVersionBoundsTest
        , it "recovers from a corrupt checkpoint using another format at the same version" checkpointSameVersionRecoveryTest
        , it "checks optional V2 manifest totals against inline file actions" checkpointManifestTotalsTest
        , it "reports a corrupt checkpoint when a JSON commit is missing" corruptCheckpointGapTest
        , it "reports a corrupt checkpoint when a JSON commit is malformed" corruptCheckpointJsonTest
        , it "recovers a corrupt commit from its complete version checksum" versionChecksumRecoveryTest
        , it "retains corroborated statistics from before a damaged commit" checksumPriorStatsTest
        , it "recovers a missing deletion-vector change with the same file count and size" checksumDeletionMetricsTest
        , it "uses a validated checksum to recover a truncated in-commit timestamp" versionChecksumTimestampRecoveryTest
        , it "verifies required version checksum metrics on request" versionChecksumValidationTest
        , it "verifies optional transaction and domain state in version checksums" checksumMetadataStateTest
        , it "streams explicit and inferred change rows with commit metadata and deletion vectors" changeDataFeedTest
        , it "allows additive CDF schema evolution and rejects mapped renames, drops and type changes" changeFeedSchemaTest
        , it "rejects checkpoint-only actions in a numbered JSON commit" checkpointActionInCommitTest
        , it "replays compacted ranges across gaps and preserves exact-target provenance" compactedReplayTest
        , it "falls back to numbered commits when compaction is invalid" compactedFallbackTest
        , it "bypasses compacted ranges that disagree with a matching checksum" compactedChecksumTest
        , it "carries checksum totals through change-feed commits with and without checksums" changeFeedChecksumTotalsTest
        , it "uses an overlapping range when a numbered commit is corrupt" overlappingCompactionRecoveryTest
        , it "rejects a log without a committed version and opens a committed empty table" emptyLogTest
        , it "rejects numbered and compacted history missing the initial table state" missingInitialStateTest
        , it "preserves domain tombstones through compaction and rejects ambiguous ranges" compactedDomainsTest
        , it "rejects duplicate checkpoint protocol rows and replays the log" duplicateCheckpointTest
        , it "recovers from a V2 checkpoint with an invalid protocol" checkpointProtocolRecoveryTest
        , it "falls back when a checkpoint has an invalid table schema" checkpointSchemaRecoveryTest
        , it "checks the required V2 sidecar size and modification time in both encodings" checkpointSidecarFieldsTest
        , it "reconciles checkpoint file actions independently of row order" checkpointFileActionsTest
        , it "reconciles literal-space and URI-encoded file paths across recovery sources" snapshotPathAliasesTest
        , it "reconciles equivalent URI escapes without collapsing reserved delimiters" snapshotUriEscapesTest
        , it "indexes shuffled checkpoint paths in Text order and rejects duplicates" checkpointPathOrderTest
        , it "caches checkpoint metrics across size and deletion-count overflow" checkpointFileMetricsTest
        , it "only retains commitInfo at the snapshot version" snapshotCommitInfoTest
        , it "opens a covered snapshot when its provenance cannot be read" missingSnapshotProvenanceTest
        , it "uses in-commit timestamps after the protocol's enablement version" historyTimestampTest
        , it "opens snapshots by timestamp on either side of the in-commit timestamp cutover" timeTravelTimestampTest
        , it "uses monotonic file modification times before in-commit timestamps are enabled" legacyHistoryTimestampTest
        , it "skips commits removed after listing when reading history" historyCleanupRaceTest
        , it "finds commitInfo after unrelated and escaped action names" commitInfoProjectionTest
        , it "rejects a catalog-managed log opened without a catalog" catalogManagedTest
        , it "rejects unknown reader features without hiding known descriptor features" readerFeaturesTest
        , it "rejects invalid reader-affecting table configuration at snapshot open" snapshotConfigurationTest
        , it "enforces reader requirements when scanning an assembled snapshot" scanReaderRequirementsTest
        , it "validates metadata format descriptors and preserves providers" metadataFormatTest
        , it "validates Parquet checkpoint format descriptors" checkpointMetadataFormatTest
        , it "validates checkpoint metadata configuration values like JSON replay" checkpointMetadataConfigurationTest
        , it "validates feature-list presence in JSON logs and Parquet checkpoints" protocolFeatureListsTest
        , it "validates the signed Int range and positivity of protocol versions" protocolVersionRangeTest
        , it "AddStats decoder" addStatsTest
        , it "preserves nested null counts and defaults to tight bounds" nestedStatsTest
        , it "decodeAddStats helper" decodeAddStatsHelperTest
        , it "reads structured Variant bounds in a Parquet checkpoint" checkpointVariantStatsTest
        , it "scans annotated and legacy shredded Variants with no fallback value column" scanTypedOnlyVariantTest
        , it "resolves unannotated Variants in mapped lists and maps" scanMappedLegacyVariantsTest
        , it "preserves legacy and extended timestamp bounds in checkpoints" checkpointTimestampStatsTest
        , it "reads Spark ANSI intervals from Parquet and partition values" sparkIntervalTest
        , it "rebases Spark legacy DATE values in scans and checkpoints" sparkLegacyDateTest
        , it "rebases Spark legacy zoned timestamps with conservative checkpoint bounds" sparkLegacyTimestampTest
        , it "expands timestamp upper bounds with historical offsets and saturates at the Delta Long limit" timestampUpperBoundTest
        , it "omits zoned timestamp bounds whose offset is unknown" ambiguousTimestampBoundsTest
        , it "returns conservative decimal bounds for Float32 and Float64" floatingStatsBoundsTest
        , it "omits unavailable logical bounds while preserving their stored representation" unavailableStatsBoundsTest
        , it "omits logical bounds outside the column's value domain" invalidStatsBoundsTest
        , it "omits malformed date bounds and impossible null counts" invalidStatsShapeTest
        , it "omits bounds for columns known to be all null" allNullStatsBoundsTest
        , it "excludes partition columns from logical file statistics" partitionFileStatsTest
        , it "scans extended Parquet timestamps with exact Delta microsecond conversion" timestamp96ScanTest
        , it "requires the timestampNtz feature for nested timezone-free timestamps" timestampNtzFeatureTest
        , it "rejects reordered same-type data chunks in Delta scans" scanChunkOrderTest
        , it "resolves URI-escaped delimiters within local Parquet paths" scanEscapedFilePathTest
        , it "streams partition values and evolved null columns from a columnless Parquet file" scanColumnlessFileTest
        , it "projects logical columns in order and leaves unselected Parquet pages unread" scanProjectionTest
        , it "filters live rows by projected-away columns, nulls, and file bounds" scanPredicateTest
        , it "prunes by date and timestamp bounds from JSON and Parquet checkpoints" scanTemporalPruningTest
        , it "prunes mapped partitions before opening data files and preserves SQL unknown" scanPartitionPruningTest
        , it "prunes nested null predicates using file-level counts" scanNestedNullPruningTest
        , it "uses a caller's per-file offset for legacy timestamp partitions" legacyPartitionOffsetTest
        , it "restores nullable values for Parquet maps stored with only keys" scanKeyOnlyMapTest
        , it "reads structured checkpoint stats alongside deep unfamiliar fields" checkpointStatsProjectionTest
        , it "uses structured stats only for add rows without JSON stats" checkpointStatsCoverageTest
        , it "rejects a checkpoint action with a mismatched Parquet physical type" checkpointPhysicalTypeTest
        , it "checks scalar and repeated checkpoint event counts against the footer" checkpointEventCountTest
        , it "rejects required fields for an action whose marker is absent" checkpointMismatchedActionRowsTest
        , it "rejects optional checkpoint fields outside their action rows" checkpointOrphanFieldsTest
        , it "rejects overlapping add and protocol markers in sparse checkpoints" checkpointOverlappingActionsTest
        , it "rejects duplicate column chunks in a checkpoint row group" checkpointDuplicateChunkTest
        , it "rejects sibling action groups that reuse a name with different leaves" checkpointDuplicateActionGroupTest
        , it "rejects swapped bounds of the same physical type" checkpointChunkOrderTest
        , it "rejects inconsistent checkpoint chunk types and external chunk locations" checkpointChunkMetadataTest
        , it "rejects negative remove sizes and checkpoint versions" checkpointUnsignedFieldsTest
        , it "normalizes incomplete extended remove metadata and preserves the tombstone" extendedRemoveMetadataTest
        , it "rejects incomplete checkpoint deletion-vector descriptors" checkpointDeletionVectorFieldsTest
        , it "reads sparse checkpoint fields across bitmap word boundaries" checkpointSparseColumnsTest
        , it "validates UTF-8 within each checkpoint add path" checkpointAddPathsTest
        , it "keeps checkpoint map values aligned with their keys" checkpointMapValuesTest
        , it "distinguishes absent and empty maps when checking checkpoint leaves" checkpointMapPresenceTest
        , it "reads legacy checkpoint string lists by position" checkpointListLayoutsTest
        , it "rejects null partition names in checkpoint metadata" checkpointNullPartitionNameTest
        , it "reads a legacy Parquet dictionary chunk in a checkpoint" checkpointLegacyDictionaryTest
        , it "ignores unfamiliar checkpoint actions and rejects empty rows" checkpointUnknownActionTest
        , it "checks that unfamiliar checkpoint action leaves agree on parent rows" checkpointUnknownActionSiblingsTest
        , it "rejects repetition that crosses an absent unfamiliar action" checkpointUnknownActionRepetitionTest
        ]


inlineDeletionVectorTest :: IO ()
inlineDeletionVectorTest = do
  let descriptor =
        Aeson.object
          [ "storageType" Aeson..= ("i" :: String)
          , "pathOrInlineDv" Aeson..= ("wi5b=000010000siXQKl0rr91000f55c8Xg0@@D72lkbi5=-{L" :: String)
          , "sizeInBytes" Aeson..= (40 :: Int)
          , "cardinality" Aeson..= (6 :: Int)
          ]
  vector <- DV.readDeletionVector "." descriptor >>= either fail pure
  DV.deletionCardinality vector `shouldBe` 6
  DV.lastDeletedRow vector `shouldBe` Just 29
  DV.selectLiveRows vector 0 30
    `shouldBe` Right (DV.SelectedRows (VS.fromList (filter (`notElem` [3, 4, 7, 11, 18, 29]) [0 .. 29])))
  DV.selectLiveRows vector 3 2 `shouldBe` Right DV.NoRows
  DV.selectLiveRows vector 30 5 `shouldBe` Right DV.AllRows


emptyDeletionVectorTest :: IO ()
emptyDeletionVectorTest = withSystemTempDirectory "wireform-delta-empty-vector" $ \root -> do
  let logDir = root </> "_delta_log"
      commit = logDir </> "00000000000000000000.json"
      descriptor = Aeson.object
        [ "storageType" Aeson..= ("i" :: T.Text)
        , "pathOrInlineDv" Aeson..= ("" :: T.Text)
        , "sizeInBytes" Aeson..= (0 :: Int)
        , "cardinality" Aeson..= (0 :: Int)
        ]
      json =
        "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"deletionVectors\"],\"writerFeatures\":[\"deletionVectors\"]}}\n"
          <> "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
          <> "{\"add\":{\"path\":\"part.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"deletionVector\":"
          <> Aeson.encode descriptor <> "}}\n"
      required = PN.NSRequired . PN.NSPrimitive
      record = PN.NSStruct . V.fromList
      row = PN.NRStruct . V.fromList
      fields = V.singleton ("add", record
        [ ("path", required PN.LtString)
        , ("size", required PN.LtInt64)
        , ("modificationTime", required PN.LtInt64)
        , ("dataChange", required PN.LtBool)
        , ("deletionVector", record
            [ ("storageType", required PN.LtString)
            , ("pathOrInlineDv", required PN.LtString)
            , ("sizeInBytes", required PN.LtInt32)
            , ("cardinality", required PN.LtInt64)
            ])
        ])
      parquet count = PN.buildNestedFile fields (V.singleton (V.singleton (row
        [ PN.NRLeaf (PN.LvString "part.parquet")
        , PN.NRLeaf (PN.LvInt64 1)
        , PN.NRLeaf (PN.LvInt64 0)
        , PN.NRLeaf (PN.LvBool True)
        , row (map PN.NRLeaf [PN.LvString "i", PN.LvString "", PN.LvInt32 0, PN.LvInt64 count])
        ])))
  createDirectory logDir
  BL.writeFile commit json
  table <- DIO.openDeltaTable root >>= either fail pure
  map D.addDeletionVector (DIO.dtActiveFiles table) `shouldBe` [Just descriptor]
  vector <- DV.readDeletionVector root descriptor >>= either fail pure
  DV.deletionCardinality vector `shouldBe` 0
  DV.lastDeletedRow vector `shouldBe` Nothing
  DV.selectLiveRows vector 0 65536 `shouldBe` Right DV.AllRows
  bytes <- either fail pure (parquet 0)
  case Checkpoint.decodeCheckpointFile bytes of
    Right [D.ActionAdd add] -> D.addDeletionVector add `shouldBe` Just descriptor
    result -> expectationFailure ("expected empty checkpoint descriptor, got " ++ show result)
  bad <- either fail pure (parquet 1)
  case Checkpoint.decodeCheckpointFile bad of
    Left err -> err `shouldSatisfy` isInfixOf "deletion vector"
    Right _ -> expectationFailure "accepted an empty descriptor with nonzero cardinality"


deletionVectorSelectionTest :: IO ()
deletionVectorSelectionTest = do
  let runBucket =
        BB.word32LE 0x0001303b
          <> BB.word8 2
          <> BB.word16LE 0
          <> BB.word16LE 2
          <> BB.word16LE 2
          <> BB.word16LE 99
          <> foldMap BB.word16LE [5, 10, 1000, 1, 0, 99]
      denseBucket =
        BB.word32LE 12346
          <> BB.word32LE 1
          <> BB.word16LE 0
          <> BB.word16LE 32767
          <> BB.word32LE 16
          <> mconcat (replicate 1024 (BB.word64LE 0x5555555555555555))
      vectorBytes = BB.word32LE 1681511377 <> BB.word64LE 2 <> BB.word32LE 0 <> runBucket <> BB.word32LE 1 <> denseBucket
      high = 4294967296 :: Word64
  let unaligned = BS.drop 1 (BL.toStrict (BB.toLazyByteString (BB.word8 0 <> vectorBytes)))
  vector <- either fail pure (DV.decodeDeletionVector unaligned)
  forM_ [1 .. 8] $ \missing ->
    case DV.decodeDeletionVector (BS.take (BS.length unaligned - missing) unaligned) of
      Left err -> err `shouldSatisfy` isInfixOf "truncated bitmap"
      Right _ -> expectationFailure "accepted a truncated Roaring bitmap word"
  DV.deletionCardinality vector `shouldBe` 32871
  DV.lastDeletedRow vector `shouldBe` Just (high + 65534)
  DV.selectLiveRows vector 131070 6 `shouldBe` Right (DV.SelectedRows (VS.fromList [0, 1]))
  DV.selectLiveRows vector 131071 103 `shouldBe` Right (DV.SelectedRows (VS.fromList [0, 101, 102]))
  DV.selectLiveRows vector 131074 98 `shouldBe` Right DV.NoRows
  DV.selectLiveRows vector (high - 3) 10 `shouldBe` Right (DV.SelectedRows (VS.fromList [0, 1, 2, 4, 6, 8]))
  DV.selectLiveRows vector (high + 65530) 10 `shouldBe` Right (DV.SelectedRows (VS.fromList [1, 3, 5, 6, 7, 8, 9]))
  DV.selectLiveRows vector high 65536 `shouldBe` Right (DV.SelectedRows (VS.generate 32768 (\i -> 2 * i + 1)))
  let deleted position =
        position `elem` [5, 10, 1000]
          || (position >= 131072 && position < 131172)
          || (position >= high && position < high + 65536 && even position)
      checkRange start n = do
        let live = filter (not . deleted . (start +) . fromIntegral) [0 .. n - 1]
            expected
              | length live == n = DV.AllRows
              | null live = DV.NoRows
              | otherwise = DV.SelectedRows (VS.fromList live)
        DV.selectLiveRows vector start n `shouldBe` Right expected
        DV.countLiveRows vector start n `shouldBe` Right (length live)
  mapM_
    (\base -> mapM_ (\start -> mapM_ (checkRange start) [0, 1, 3, 64, 127]) [base - 2 .. base + 2])
    [5, 10, 1000, 131072, 131172, high, high + 64, high + 65536]
  let runs = [0, 8 .. 65528]
      encodeRun row = BB.word16LE row <> BB.word16LE 2
      runBytes = BL.toStrict . BB.toLazyByteString $
        BB.word32LE 1681511377 <> BB.word64LE 1 <> BB.word32LE 0
          <> BB.word32LE 12347 <> BB.word8 1 <> BB.word16LE 0
          <> BB.word16LE (fromIntegral (3 * length runs - 1))
          <> BB.word16LE (fromIntegral (length runs)) <> foldMap encodeRun runs
  fragmented <- either fail pure (DV.decodeDeletionVector runBytes)
  DV.deletionCardinality fragmented `shouldBe` 24_576
  let checkRuns start n = do
        let live = filter (\i -> let row = start + fromIntegral i in row >= 65536 || row `rem` 8 >= 3) [0 .. n - 1]
            expected
              | null live = DV.NoRows
              | length live == n = DV.AllRows
              | otherwise = DV.SelectedRows (VS.fromList live)
        DV.selectLiveRows fragmented start n `shouldBe` Right expected
        DV.countLiveRows fragmented start n `shouldBe` Right (length live)
  mapM_ (\start -> mapM_ (checkRuns start) [1, 3, 5, 13, 512]) [0, 2, 4, 32768, 65530, 65531, 65536]
  let roaringArray low =
        BB.word32LE 12346 <> BB.word32LE 1 <> BB.word16LE 0 <> BB.word16LE 0
          <> BB.word32LE 16 <> BB.word16LE low
      sparseNative =
        BB.word32LE 1681511376 <> BB.word32LE 3
          <> BB.word32LE 18 <> roaringArray 2
          <> BB.word32LE 8 <> BB.word32LE 12346 <> BB.word32LE 0
          <> BB.word32LE 18 <> roaringArray 3
      sparsePortable =
        BB.word32LE 1681511377 <> BB.word64LE 2
          <> BB.word32LE 0 <> roaringArray 2 <> BB.word32LE 2 <> roaringArray 3
      signedLimit =
        BB.word32LE 1681511377 <> BB.word64LE 1 <> BB.word32LE 0x7fffffff
          <> BB.word32LE 12346 <> BB.word32LE 1 <> BB.word16LE 0xffff <> BB.word16LE 0
          <> BB.word32LE 16 <> BB.word16LE 0xffff
      decodeBuilder = DV.decodeDeletionVector . BL.toStrict . BB.toLazyByteString
  forM_ [sparseNative, sparsePortable] $ \fixture -> do
    sparse <- either fail pure (decodeBuilder fixture)
    DV.deletionCardinality sparse `shouldBe` 2
    DV.lastDeletedRow sparse `shouldBe` Just (2 * high + 3)
    DV.selectLiveRows sparse high 32768 `shouldBe` Right DV.AllRows
    DV.selectLiveRows sparse (2 * high + 1) 5 `shouldBe` Right (DV.SelectedRows (VS.fromList [0, 1, 3, 4]))
  nearLimit <- either fail pure (decodeBuilder signedLimit)
  DV.lastDeletedRow nearLimit `shouldBe` Just 0x7fffffffffffffff
  DV.selectLiveRows nearLimit 0x7ffffffffffffffe 4 `shouldBe` Right (DV.SelectedRows (VS.fromList [0, 2, 3]))
  DV.countLiveRows nearLimit 0x7ffffffffffffffe 4 `shouldBe` Right 3
  DV.countLiveRows nearLimit maxBound 1 `shouldBe` Left "Delta.DeletionVector: row interval out of range"
  DV.countLiveRows nearLimit 0 (-1) `shouldBe` Left "Delta.DeletionVector: row interval out of range"


invalidDeletionVectorTest :: IO ()
invalidDeletionVectorTest = do
  let bytes =
        BL.toStrict . BB.toLazyByteString $
          BB.word32LE 1681511377
            <> BB.word64LE 1
            <> BB.word32LE 0
            <> BB.word32LE 12346
            <> BB.word32LE 1
            <> BB.word16LE 0
            <> BB.word16LE 1
            <> BB.word32LE 16
            <> BB.word16LE 1
            <> BB.word16LE 3
      patch offset value = BS.take offset bytes <> BS.singleton value <> BS.drop (offset + 1) bytes
      invalid input = case DV.decodeDeletionVector input of
        Left _ -> pure ()
        Right _ -> expectationFailure "invalid deletion vector was accepted"
  mapM_ (invalid . (`BS.take` bytes)) [0 .. BS.length bytes - 1]
  mapM_ invalid [bytes <> "trailing", patch 0 0, patch 15 128, patch 28 20, patch 34 1]


withCorruptCheckpoint :: (FilePath -> FilePath -> IO ()) -> IO ()
withCorruptCheckpoint action =
  withSystemTempDirectory "wireform-delta-checkpoint" $ \root -> do
    let logDir = root </> "_delta_log"
    createDirectory logDir
    BL.writeFile (logDir </> "00000000000000000000.json") $
      "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2}}\n"
        <> "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
        <> "{\"add\":{\"path\":\"live.parquet\",\"size\":42,\"modificationTime\":0,\"dataChange\":true}}\n"
    BL.writeFile (logDir </> "00000000000000000001.json") "{\"txn\":{\"appId\":\"client\",\"version\":7}}\n"
    BL.writeFile (logDir </> "00000000000000000001.checkpoint.parquet") "truncated checkpoint"
    action root logDir


checkpointDiscoveryFilesTest :: IO ()
checkpointDiscoveryFilesTest =
  withCorruptCheckpoint $ \root logDir -> do
    let classicDir = logDir </> "00000000000000000004.checkpoint.parquet"
        partDir = logDir </> "00000000000000000003.checkpoint.0000000001.0000000002.parquet"
        partFile = logDir </> "00000000000000000003.checkpoint.0000000002.0000000002.parquet"
        alternateDir = logDir </> "00000000000000000001.checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json"
    mapM_ createDirectory [classicDir, partDir, alternateDir]
    BL.writeFile partFile "part file"
    DIO.findCheckpointParquet root >>= (\version -> version `shouldBe` Just 1)
    table <- DIO.openDeltaTable root >>= either fail pure
    DIO.dtVersion table `shouldBe` Just 1
    DIO.dtCheckpointAvailable table `shouldBe` Just 1
    DIO.activeFilePaths table `shouldBe` ["live.parquet"]
    DIO.openDeltaTableAt root 3 >>= \case
      Left err -> err `shouldSatisfy` isInfixOf "beyond the available history"
      Right _ -> expectationFailure "accepted a version represented only by an incomplete multipart set"


logDiscoveryFilesTest :: IO ()
logDiscoveryFilesTest = withCorruptCheckpoint $ \root logDir -> do
  let first = "00000000000000000000"
      second = "00000000000000000001"
      third = "00000000000000000002"
      fourth = "00000000000000000003"
      secondCommit = logDir </> second ++ ".json"
      secondCompaction = logDir </> first ++ "." ++ second ++ ".compacted.json"
      thirdCommit = logDir </> third ++ ".json"
      fourthCompaction = logDir </> first ++ "." ++ fourth ++ ".compacted.json"
      compactedContent =
        "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2}}\n"
          <> "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
      check opener commits paths = do
        table <- opener >>= either fail pure
        DIO.dtVersion table `shouldBe` Just 1
        DIO.activeFilePaths table `shouldBe` paths
        DIO.findCommits root >>= (\files -> map fst files `shouldBe` commits)
        DIO.historyEntries table >>= (\history -> map DIO.heVersion history `shouldBe` reverse commits)
  createDirectory fourthCompaction
  createFileLink (logDir </> "deleted-commit") thirdCommit
  check (DIO.openDeltaTable root) [0, 1] ["live.parquet"]
  DIO.openDeltaTableAt root 2 >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "beyond the available history"
    Right _ -> expectationFailure "opened a version represented only by a dangling link"
  removeFile secondCommit
  createDirectory secondCommit
  BL.writeFile secondCompaction compactedContent
  check (DIO.openDeltaTable root) [0] []
  check (DIO.openDeltaTableAt root 1) [0] []


logVersionBoundsTest :: IO ()
logVersionBoundsTest =
  withCorruptCheckpoint $ \root logDir -> do
    let overflow = "09223372036854775808"
        commit = overflow ++ ".json"
        checkpoint = overflow ++ ".checkpoint.parquet"
        compaction = "00000000000000000001." ++ overflow ++ ".compacted.json"
    mapM_ (\name -> BL.writeFile (logDir </> name) "{}") [commit, checkpoint, compaction]
    BL.writeFile (logDir </> "_last_checkpoint") "{\"version\":9223372036854775808,\"size\":3}"
    table <- DIO.openDeltaTable root >>= either fail pure
    DIO.dtVersion table `shouldBe` Just 1
    DIO.dtLastCheckpoint table `shouldBe` Nothing
    DIO.dtCheckpointAvailable table `shouldBe` Just 1
    DIO.activeFilePaths table `shouldBe` ["live.parquet"]
    entries <- DIO.listLogEntries root
    mapM_ (\name -> lookup name entries `shouldBe` Just "unknown") [commit, checkpoint, compaction]
    let largest = "09223372036854775807.checkpoint.parquet"
    BL.writeFile (logDir </> largest) "checkpoint"
    DIO.findCheckpointParquet root >>= (\version -> version `shouldBe` Just 9223372036854775807)


checkpointSameVersionRecoveryTest :: IO ()
checkpointSameVersionRecoveryTest =
  withCorruptCheckpoint $ \root logDir -> do
    let checkpoint = logDir </> "00000000000000000001.checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json"
    BL.writeFile checkpoint $
      "{\"checkpointMetadata\":{\"version\":1}}\n"
        <> "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\"]}}\n"
        <> "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
        <> "{\"add\":{\"path\":\"live.parquet\",\"size\":42,\"modificationTime\":0,\"dataChange\":true}}\n"
        <> "{\"txn\":{\"appId\":\"client\",\"version\":7}}\n"
    removeFile (logDir </> "00000000000000000000.json")
    table <- DIO.openDeltaTableAt root 1 >>= either fail pure
    DIO.dtVersion table `shouldBe` Just 1
    DIO.activeFilePaths table `shouldBe` ["live.parquet"]
    D.tsAppIds (DIO.dtSnapshot table) `shouldBe` Map.singleton "client" 7


checkpointManifestTotalsTest :: IO ()
checkpointManifestTotalsTest = withCorruptCheckpoint $ \root logDir -> do
  let checkpoint = logDir </> "00000000000000000001.checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json"
      write tags = BL.writeFile checkpoint $
        "{\"checkpointMetadata\":{\"version\":1,\"tags\":" <> tags <> "}}\n"
          <> "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\"]}}\n"
          <> "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
          <> "{\"add\":{\"path\":\"checkpoint.parquet\",\"size\":42,\"modificationTime\":0,\"dataChange\":true}}\n"
      expectFile path = do
        table <- DIO.openDeltaTable root >>= either fail pure
        DIO.activeFilePaths table `shouldBe` [path]
  write "{\"numOfAddFiles\":\"01\",\"sidecarSizeInBytes\":\"0\",\"sidecarNumActions\":\"0\",\"unknown\":\"kept\"}"
  expectFile "checkpoint.parquet"
  write "{\"numOfAddFiles\":\"2\"}"
  expectFile "live.parquet"
  removeFile (logDir </> "00000000000000000000.json")
  removeFile (logDir </> "00000000000000000001.checkpoint.parquet")
  let reject problem = DIO.openDeltaTable root >>= \case
        Left err -> err `shouldSatisfy` isInfixOf problem
        Right _ -> expectationFailure "accepted an inconsistent manifest"
  reject "checkpointMetadata.tags.numOfAddFiles mismatch"
  write "{\"numOfAddFiles\":\"9223372036854775808\"}"
  reject "invalid checkpointMetadata.tags.numOfAddFiles"


compactedReplayTest :: IO ()
compactedReplayTest =
  withSystemTempDirectory "wireform-delta-compaction" $ \root -> do
    let logDir = root </> "_delta_log"
        first = "00000000000000000000"
        second = "00000000000000000001"
        third = "00000000000000000002"
        fourth = "00000000000000000003"
        fifth = "00000000000000000004"
        writeRange a b = BL.writeFile (logDir </> a ++ "." ++ b ++ ".compacted.json")
        writeCommit v = BL.writeFile (logDir </> v ++ ".json")
    createDirectory logDir
    writeCommit first $
      "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2}}\n"
        <> "{\"metaData\":{\"id\":\"compaction\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
        <> "{\"add\":{\"path\":\"old.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
    writeRange second third $
      "{\"remove\":{\"path\":\"old.parquet\",\"dataChange\":true}}\n"
        <> "{\"txn\":{\"appId\":\"job\",\"version\":8}}\n"
    -- Taking [1,3] would strand the replay at version 4. The complete
    -- route uses [1,2] followed by [3,4], with no numbered file at 3.
    writeRange second fourth "{\"txn\":{\"appId\":\"stranded\",\"version\":99}}\n"
    writeRange fourth fifth "{\"add\":{\"path\":\"live.parquet\",\"size\":2,\"modificationTime\":0,\"dataChange\":true}}\n"
    latest <- DIO.openDeltaTable root >>= either fail pure
    DIO.dtVersion latest `shouldBe` Just 4
    DIO.activeFilePaths latest `shouldBe` ["live.parquet"]
    D.tsAppIds (DIO.dtSnapshot latest) `shouldBe` Map.singleton "job" 8
    D.tsLastCommit (DIO.dtSnapshot latest) `shouldBe` Nothing
    interior <- DIO.openDeltaTableAt root 1
    case interior of
      Left _ -> pure ()
      Right _ -> expectationFailure "cannot reconstruct a version inside an indivisible range"
    writeCommit fifth "{\"commitInfo\":{\"timestamp\":42}}\n"
    atEnd <- DIO.openDeltaTableAt root 4 >>= either fail pure
    fmap D.ciTimestamp (D.tsLastCommit (DIO.dtSnapshot atEnd)) `shouldBe` Just (Just 42)
    entries <- DIO.listLogEntries root
    lookup (fourth ++ "." ++ fifth ++ ".compacted.json") entries `shouldBe` Just "compaction"


compactedDomainsTest :: IO ()
compactedDomainsTest =
  withSystemTempDirectory "wireform-delta-compact-domains" $ \root -> do
    let logDir = root </> "_delta_log"
        first = logDir </> "00000000000000000000.json"
        next = logDir </> "00000000000000000001.json"
        lastCommit = logDir </> "00000000000000000002.json"
        compacted = logDir </> "00000000000000000001.00000000000000000002.compacted.json"
        protocol = "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":7,\"writerFeatures\":[\"domainMetadata\"]}}\n"
        metadata = "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
        domain name configuration removed =
          "{\"domainMetadata\":{\"domain\":\"" <> name <> "\",\"configuration\":\"" <> configuration
            <> "\",\"removed\":" <> removed <> "}}\n"
        original = domain "example.expiring" "initial" "false"
        changed = domain "example.expiring" "updated" "false"
        tombstone = domain "example.expiring" "updated" "true"
        kept = domain "example.kept" "yes" "false"
        expectDomains expected = do
          table <- DIO.openDeltaTable root >>= either fail pure
          DIO.dtVersion table `shouldBe` Just 2
          D.tsDomains (DIO.dtSnapshot table) `shouldBe` expected
        live = Map.singleton "example.kept" "yes"
    createDirectory logDir
    BL.writeFile first (protocol <> metadata <> original)
    BL.writeFile next (changed <> kept)
    BL.writeFile lastCommit tombstone
    expectDomains live
    -- If this malformed compacted range were accepted, the domain would
    -- be resurrected. Numbered replay must be used instead.
    BL.writeFile compacted (tombstone <> kept <> changed)
    expectDomains live
    BL.writeFile compacted (kept <> tombstone)
    removeFile next
    removeFile lastCommit
    expectDomains live
    BL.writeFile compacted (tombstone <> changed)
    DIO.openDeltaTable root >>= \case
      Left err -> err `shouldSatisfy` isInfixOf "duplicate metadata domain"
      Right _ -> expectationFailure "ambiguous compacted metadata domain was accepted"
    removeFile compacted
    BL.writeFile next (tombstone <> changed)
    DIO.openDeltaTable root >>= \case
      Left err -> err `shouldSatisfy` isInfixOf "duplicate metadata domain"
      Right _ -> expectationFailure "ambiguous numbered metadata domain was accepted"


compactedFallbackTest :: IO ()
compactedFallbackTest =
  withCorruptCheckpoint $ \root logDir -> do
    BL.writeFile (logDir </> "00000000000000000000.00000000000000000001.compacted.json") $
      "{\"add\":{\"path\":\"phantom.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
        <> "{\"commitInfo\":{\"timestamp\":42}}\n"
    table <- DIO.openDeltaTable root >>= either fail pure
    DIO.activeFilePaths table `shouldBe` ["live.parquet"]
    D.tsAppIds (DIO.dtSnapshot table) `shouldBe` Map.singleton "client" 7


compactedChecksumTest :: IO ()
compactedChecksumTest = withCorruptCheckpoint $ \root logDir -> do
  let second = logDir </> "00000000000000000001.json"
      third = logDir </> "00000000000000000002.json"
      compacted = logDir </> "00000000000000000001.00000000000000000002.compacted.json"
      checksum = logDir </> "00000000000000000002.crc"
      fourth = logDir </> "00000000000000000003.json"
      protocol = "{\"minReaderVersion\":1,\"minWriterVersion\":2}"
      metadata = "{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}"
      addFile = "{\"add\":{\"path\":\"next.parquet\",\"size\":1,\"modificationTime\":0,\"partitionValues\":{},\"dataChange\":true}}\n"
      txn = "{\"txn\":{\"appId\":\"client\",\"version\":7}}\n"
      writeChecksum txnId = BL.writeFile checksum $
        "{\"numFiles\":2,\"tableSizeBytes\":43,\"numProtocol\":1,\"numMetadata\":1,\"protocol\":"
          <> protocol <> ",\"metadata\":" <> metadata <> ",\"txnId\":\"" <> txnId <> "\"}"
      open version paths = do
        table <- DIO.openDeltaTableAt root version >>= either fail pure
        DIO.activeFilePaths table `shouldBe` paths
        pure (D.tsLastCommit (DIO.dtSnapshot table) >>= D.ciOperation)
  BL.writeFile third ("{\"commitInfo\":{\"txnId\":\"third\",\"operation\":\"WRITE\"}}\n" <> addFile)
  BL.writeFile fourth "{\"commitInfo\":{\"operation\":\"TAIL\"}}\n"
  BL.writeFile compacted txn
  writeChecksum "stale"
  open 2 ["live.parquet"] `shouldReturn` Just "WRITE"
  writeChecksum "third"
  open 2 ["live.parquet", "next.parquet"] `shouldReturn` Just "WRITE"
  open 3 ["live.parquet", "next.parquet"] `shouldReturn` Just "TAIL"
  removeFile second
  DIO.openDeltaTableAt root 2 >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "replayed file metrics disagree with the version checksum"
    Right _ -> expectationFailure "accepted an incomplete compacted range"
  BL.writeFile compacted (txn <> addFile)
  open 2 ["live.parquet", "next.parquet"] `shouldReturn` Just "WRITE"


overlappingCompactionRecoveryTest :: IO ()
overlappingCompactionRecoveryTest =
  withSystemTempDirectory "wireform-delta-overlapping-compaction" $ \root -> do
    let logDir = root </> "_delta_log"
        first = "00000000000000000000"
        second = "00000000000000000001"
        third = "00000000000000000002"
        fourth = "00000000000000000003"
        fifth = "00000000000000000004"
        range a b = logDir </> a ++ "." ++ b ++ ".compacted.json"
        commit v = logDir </> v ++ ".json"
        added = "{\"add\":{\"path\":\"live.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
        transaction = "{\"txn\":{\"appId\":\"client\",\"version\":7}}\n"
    createDirectory logDir
    BL.writeFile (commit first) $
      "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2}}\n"
        <> "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
    BL.writeFile (commit second) added
    -- This range is usable on its own but leads to a corrupt numbered
    -- commit. Its transaction must not leak into the alternate replay.
    BL.writeFile (range second third) $
      added <> "{\"txn\":{\"appId\":\"abandoned\",\"version\":88}}\n"
    BL.writeFile (commit fourth) "{\"txn\":\n"
    BL.writeFile (range third fourth) transaction
    BL.writeFile (commit fifth) "{\"commitInfo\":{\"timestamp\":42}}\n"

    table <- DIO.openDeltaTable root >>= either fail pure
    DIO.dtVersion table `shouldBe` Just 4
    DIO.activeFilePaths table `shouldBe` ["live.parquet"]
    D.tsAppIds (DIO.dtSnapshot table) `shouldBe` Map.singleton "client" 7
    fmap D.ciTimestamp (D.tsLastCommit (DIO.dtSnapshot table)) `shouldBe` Just (Just 42)

    removeFile (range third fourth)
    DIO.openDeltaTable root >>= \case
      Left err -> err `shouldSatisfy` isInfixOf (commit fourth)
      Right _ -> expectationFailure "accepted a log with no readable route to the target"


emptyLogTest :: IO ()
emptyLogTest =
  withSystemTempDirectory "wireform-delta-empty-log" $ \root -> do
    let logDir = root </> "_delta_log"
        initial = logDir </> "00000000000000000000.json"
        open = DIO.openDeltaTable root
        expectMissingVersion = open >>= \case
          Left err -> err `shouldSatisfy` isInfixOf "no committed Delta version"
          Right _ -> expectationFailure "opened a table from an empty log"
    createDirectory logDir
    expectMissingVersion
    BL.writeFile (logDir </> "_last_checkpoint") "{\"version\":0,\"size\":2}"
    BL.writeFile (logDir </> "00000000000000000000.crc") "{}"
    createDirectory initial
    expectMissingVersion
    removeDirectory initial
    BL.writeFile initial $
      "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2}}\n"
        <> "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
    table <- open >>= either fail pure
    DIO.dtVersion table `shouldBe` Just 0
    DIO.activeFilePaths table `shouldBe` []


missingInitialStateTest :: IO ()
missingInitialStateTest =
  withSystemTempDirectory "wireform-delta-missing-initial-state" $ \root -> do
    let logDir = root </> "_delta_log"
        initial = logDir </> "00000000000000000000.json"
        next = logDir </> "00000000000000000001.json"
        compacted = logDir </> "00000000000000000000.00000000000000000001.compacted.json"
        protocol = "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2}}\n"
        metadata = "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
        check path = DIO.openDeltaTable root >>= \case
          Left err -> do
            err `shouldSatisfy` isInfixOf path
            err `shouldSatisfy` isInfixOf "missing protocol or table metadata"
          Right _ -> expectationFailure "accepted history with incomplete initial table state"
    createDirectory logDir
    BL.writeFile initial protocol
    BL.writeFile next metadata
    check initial
    BL.writeFile initial metadata
    BL.writeFile next protocol
    check initial
    removeFile initial
    removeFile next
    BL.writeFile compacted protocol
    check compacted
    BL.writeFile compacted (protocol <> metadata)
    table <- DIO.openDeltaTable root >>= either fail pure
    DIO.dtVersion table `shouldBe` Just 1


duplicateCheckpointTest :: IO ()
duplicateCheckpointTest =
  withCorruptCheckpoint $ \root logDir -> do
    let checkpoint = logDir </> "00000000000000000001.checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json"
        metadata = "{\"metaData\":{\"id\":\"checkpoint\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
        protocol = "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\"]}}\n"
    BL.writeFile checkpoint $
      "{\"checkpointMetadata\":{\"version\":1}}\n" <> metadata <> protocol <> protocol
    table <- DIO.openDeltaTable root >>= either fail pure
    DIO.activeFilePaths table `shouldBe` ["live.parquet"]
    D.tsAppIds (DIO.dtSnapshot table) `shouldBe` Map.singleton "client" 7


checkpointProtocolRecoveryTest :: IO ()
checkpointProtocolRecoveryTest =
  withCorruptCheckpoint $ \root logDir -> do
    let checkpoint = logDir </> "00000000000000000001.checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json"
        metadata = "{\"metaData\":{\"id\":\"checkpoint\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
        add = "{\"add\":{\"path\":\"checkpoint.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
        recover protocol = do
          BL.writeFile checkpoint ("{\"checkpointMetadata\":{\"version\":1}}\n" <> metadata <> protocol <> add)
          table <- DIO.openDeltaTable root >>= either fail pure
          DIO.activeFilePaths table `shouldBe` ["live.parquet"]
          D.tsAppIds (DIO.dtSnapshot table) `shouldBe` Map.singleton "client" 7
    recover "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[]}}\n"
    recover "{\"protocol\":{\"minReaderVersion\":2,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\"]}}\n"
    recover "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\",\"catalogManaged\"]}}\n"
    BL.writeFile checkpoint $
      "{\"checkpointMetadata\":{\"version\":1}}\n" <> metadata <> add
        <> "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\",\"futureReaderFeature\"],\"writerFeatures\":[\"v2Checkpoint\",\"futureReaderFeature\"]}}\n"
    DIO.openDeltaTable root >>= \case
      Left err -> err `shouldSatisfy` isInfixOf "unsupported snapshot reader feature futureReaderFeature"
      Right _ -> expectationFailure "fallback hid a required reader feature"


checkpointSchemaRecoveryTest :: IO ()
checkpointSchemaRecoveryTest =
  withCorruptCheckpoint $ \root logDir -> do
    let version = "00000000000000000001"
        jsonCheckpoint = logDir </> version ++ ".checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json"
        classicCheckpoint = logDir </> version ++ ".checkpoint.parquet"
        metadata = "{\"checkpointMetadata\":{\"version\":1}}\n"
        protocol = "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\"]}}\n"
        writeJson schema =
          BL.writeFile jsonCheckpoint $
            metadata <> protocol
              <> Aeson.encode
                (Aeson.object ["metaData" Aeson..= Aeson.object ["id" Aeson..= ("checkpoint" :: T.Text), "schemaString" Aeson..= schema]])
        leaf = PN.NSRequired . PN.NSPrimitive
        record = PN.NSOptional . PN.NSStruct . V.fromList
        fields = V.fromList
          [ ("metaData", record
              [ ("id", leaf PN.LtString)
              , ("schemaString", leaf PN.LtString)
              , ("partitionColumns", PN.NSList (PN.NSOptional (PN.NSPrimitive PN.LtString)))
              , ("format", PN.NSOptional (PN.NSStruct (V.singleton ("provider", PN.NSOptional (PN.NSPrimitive PN.LtString)))))
              , ("configuration", PN.NSMap (leaf PN.LtString) (PN.NSOptional (PN.NSPrimitive PN.LtString)))
              ])
          , ("protocol", record [("minReaderVersion", leaf PN.LtInt32), ("minWriterVersion", leaf PN.LtInt32)])
          ]
        row = PN.NRStruct . V.fromList . map PN.NRLeaf
        writeClassic schema partitions format config = do
          let metadataRow = PN.NRStruct (V.fromList
                [PN.NRLeaf (PN.LvString "checkpoint"), PN.NRLeaf (PN.LvString schema), PN.NRList partitions, format, PN.NRMapEntries config])
              metadataRows = V.fromList [metadataRow, PN.NRNull]
              protocolRows = V.fromList [PN.NRNull, row [PN.LvInt32 1, PN.LvInt32 2]]
          bytes <- either fail pure (PN.buildNestedFile fields (V.fromList [metadataRows, protocolRows]))
          BS.writeFile classicCheckpoint bytes
        good = "{\"type\":\"struct\",\"fields\":[]}" :: T.Text
        bad = "{\"type\":\"struct\"}" :: T.Text
        files = fmap DIO.activeFilePaths <$> DIO.openDeltaTable root
    writeJson good
    files `shouldReturn` Right []
    writeJson bad
    files `shouldReturn` Right ["live.parquet"]
    removeFile jsonCheckpoint
    writeClassic good V.empty (row [PN.LvString "parquet"]) V.empty
    files `shouldReturn` Right []
    writeClassic good V.empty (PN.NRStruct (V.singleton PN.NRNull)) V.empty
    files `shouldReturn` Right ["live.parquet"]
    writeClassic good (V.singleton PN.NRNull) PN.NRNull V.empty
    files `shouldReturn` Right ["live.parquet"]
    writeClassic good V.empty PN.NRNull (V.singleton (PN.NRLeaf (PN.LvString "delta.columnMapping.mode"), PN.NRNull))
    files `shouldReturn` Right ["live.parquet"]
    writeClassic bad V.empty PN.NRNull V.empty
    files `shouldReturn` Right ["live.parquet"]
    removeFile (logDir </> "00000000000000000000.json")
    DIO.openDeltaTable root >>= \case
      Left err -> err `shouldSatisfy` isInfixOf "invalid table schema"
      Right _ -> expectationFailure "an unreadable checkpoint schema was accepted"


checkpointSidecarFieldsTest :: IO ()
checkpointSidecarFieldsTest = do
  let decodeJson = fmap reverse . D.foldCheckpointJson (flip (:)) []
      json fields = "{\"sidecar\":{\"path\":\"files.parquet\"," <> fields <> "}}"
      expected = [D.ActionSidecar (D.SidecarAction "files.parquet" 17)]
  decodeJson (json "\"sizeInBytes\":17,\"modificationTime\":-1") `shouldBe` Right expected
  mapM_
    ( \fields -> case decodeJson (json fields) of
        Left _ -> pure ()
        Right _ -> expectationFailure "invalid JSON sidecar accepted"
    )
    [ "\"sizeInBytes\":17"
    , "\"sizeInBytes\":17,\"modificationTime\":null"
    , "\"sizeInBytes\":17,\"modificationTime\":9223372036854775808"
    , "\"sizeInBytes\":-1,\"modificationTime\":0"
    , "\"sizeInBytes\":9223372036854775808,\"modificationTime\":0"
    ]
  let leaf = PN.NSRequired . PN.NSPrimitive
      field name = (name, leaf PN.LtInt64)
      row size fields =
        let schema = PN.NSStruct (V.fromList ([("path", leaf PN.LtString), field "sizeInBytes"] ++ map fst fields))
            values = V.fromList ([PN.NRLeaf (PN.LvString "files.parquet"), PN.NRLeaf (PN.LvInt64 size)] ++ map snd fields)
        in either fail pure (PN.buildNestedFile (V.singleton ("sidecar", schema)) (V.singleton (V.singleton (PN.NRStruct values))))
      modified = [(field "modificationTime", PN.NRLeaf (PN.LvInt64 (-1)))]
      reject bytes = case Checkpoint.decodeCheckpointFile bytes of
        Left _ -> pure ()
        Right _ -> expectationFailure "invalid Parquet sidecar accepted"
  good <- row 17 modified
  Checkpoint.decodeCheckpointFile good `shouldBe` Right expected
  row 17 [] >>= reject
  row (-1) modified >>= reject


checkpointFileActionsTest :: IO ()
checkpointFileActionsTest =
  withSystemTempDirectory "wireform-delta-checkpoint-files" $ \root -> do
    let logDir = root </> "_delta_log"
        path = logDir </> "00000000000000000001.checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json"
        header =
          "{\"checkpointMetadata\":{\"version\":1}}\n"
            <> "{\"metaData\":{\"id\":\"checkpoint\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
            <> "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\",\"deletionVectors\"],\"writerFeatures\":[\"v2Checkpoint\",\"deletionVectors\"]}}\n"
        add = "{\"add\":{\"path\":\"live.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
        removeSame = "{\"remove\":{\"path\":\"live.parquet\",\"dataChange\":true}}\n"
        removeOld = "{\"remove\":{\"path\":\"live.parquet\",\"dataChange\":true,\"deletionVector\":{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///old.bin\",\"offset\":1,\"sizeInBytes\":32,\"cardinality\":1}}}\n"
        removeOlder = "{\"remove\":{\"path\":\"live.parquet\",\"dataChange\":true,\"deletionVector\":{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///old.bin\",\"offset\":2,\"sizeInBytes\":32,\"cardinality\":1}}}\n"
        removeSameVectorDifferentCardinality = "{\"remove\":{\"path\":\"live.parquet\",\"dataChange\":true,\"deletionVector\":{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///old.bin\",\"offset\":1,\"sizeInBytes\":32,\"cardinality\":2}}}\n"
        removeMissingVectorFields = "{\"remove\":{\"path\":\"live.parquet\",\"dataChange\":true,\"deletionVector\":{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///old.bin\"}}}\n"
        addMissingVectorFields = "{\"add\":{\"path\":\"live.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"deletionVector\":{}}}\n"
        checkLive rows = do
          BL.writeFile path (header <> rows)
          table <- DIO.openDeltaTable root >>= either fail pure
          DIO.activeFilePaths table `shouldBe` ["live.parquet"]
        checkRejected problem rows = do
          BL.writeFile path (header <> rows)
          DIO.openDeltaTable root >>= \case
            Left err -> err `shouldSatisfy` isInfixOf problem
            Right _ -> expectationFailure "invalid checkpoint was accepted"
    createDirectory logDir
    mapM_ checkLive [add <> removeOld, removeOld <> add, removeOlder <> add <> removeOld]
    mapM_ (checkRejected "same logical file") [add <> removeSame, removeSame <> add]
    checkRejected "duplicate add path" (add <> add)
    mapM_ (checkRejected "duplicate checkpoint remove key")
      [removeSame <> removeSame, removeOld <> removeOld, removeOld <> removeSameVectorDifferentCardinality]
    checkRejected "sizeInBytes" (add <> removeMissingVectorFields)
    checkRejected "storageType" addMissingVectorFields
    let addPath pathName = "{\"add\":{\"path\":\"" <> pathName <> "\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
        shuffled = addPath "z.parquet" <> add <> addPath "\\u00e9.parquet" <> addPath "a.parquet"
    BL.writeFile path (header <> shuffled)
    table <- DIO.openDeltaTable root >>= either fail pure
    DIO.activeFilePaths table `shouldBe` ["a.parquet", "live.parquet", "z.parquet", "é.parquet"]
    checkRejected "duplicate add path" (shuffled <> add)


snapshotPathAliasesTest :: IO ()
snapshotPathAliasesTest =
  withSystemTempDirectory "wireform-delta-path-alias" $ \root -> do
    let logDir = root </> "_delta_log"
        first = logDir </> "00000000000000000000.json"
        second = logDir </> "00000000000000000001.json"
        compacted = logDir </> "00000000000000000001.00000000000000000002.compacted.json"
        checkpoint = logDir </> "00000000000000000000.checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json"
        protocol = "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\",\"deletionVectors\"],\"writerFeatures\":[\"v2Checkpoint\",\"deletionVectors\"]}}\n"
        metadata = "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
        header = protocol <> metadata
        literal = "one two%20three.parquet"
        escaped = "one%20two%20three.parquet"
        distinct = "one%20two%2520three.parquet"
        add path = "{\"add\":{\"path\":\"" <> path <> "\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
        remove path = "{\"remove\":{\"path\":\"" <> path <> "\",\"dataChange\":true}}\n"
        descriptor = "\"deletionVector\":{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///bitmap.bin\",\"offset\":1,\"sizeInBytes\":32,\"cardinality\":1}"
        withVector action = BL.take (BL.length action - 3) action <> "," <> descriptor <> "}}\n"
        commit = header <> add literal <> add distinct
        snapshot = D.snapshotFromActions . D.parseLogFile
        files = fmap (Map.keys . D.tsFiles . DIO.dtSnapshot) <$> DIO.openDeltaTable root
    createDirectory logDir
    BL.writeFile first commit
    BL.writeFile second (remove escaped)
    files `shouldReturn` Right ["one%20two%2520three.parquet"]
    oldest <- DIO.openDeltaTableAt root 0 >>= either fail pure
    Map.keys (D.tsFiles (DIO.dtSnapshot oldest)) `shouldBe` ["one two%20three.parquet", "one%20two%2520three.parquet"]
    BL.writeFile second (withVector (remove escaped))
    files `shouldReturn` Right ["one two%20three.parquet", "one%20two%2520three.parquet"]
    BL.writeFile second (withVector (add escaped) <> remove literal)
    files `shouldReturn` Right ["one%20two%20three.parquet", "one%20two%2520three.parquet"]
    let applied = foldl' D.applyAction D.emptySnapshot (D.parseLogFile (commit <> remove escaped))
    applied `shouldBe` snapshot (commit <> remove escaped)
    Map.keys (D.tsFiles applied) `shouldBe` ["one%20two%2520three.parquet"]
    let replacement = snapshot (commit <> add escaped)
    Map.keys (D.tsFiles replacement) `shouldBe` ["one%20two%20three.parquet", "one%20two%2520three.parquet"]
    BL.writeFile second (remove literal)
    BL.writeFile first (header <> add escaped <> add distinct)
    files `shouldReturn` Right ["one%20two%2520three.parquet"]
    BL.writeFile first commit
    removeFile second
    BL.writeFile compacted (remove escaped)
    files `shouldReturn` Right ["one%20two%2520three.parquet"]
    BL.writeFile checkpoint ("{\"checkpointMetadata\":{\"version\":0}}\n" <> commit)
    removeFile first
    files `shouldReturn` Right ["one%20two%2520three.parquet"]
    let reject problem rows = do
          BL.writeFile checkpoint ("{\"checkpointMetadata\":{\"version\":0}}\n" <> header <> rows)
          DIO.openDeltaTableAt root 0 >>= \case
            Left err -> err `shouldSatisfy` isInfixOf problem
            Right _ -> expectationFailure "checkpoint accepted duplicate logical paths"
    reject "duplicate add path" (add literal <> add escaped)
    reject "same logical file" (add literal <> remove escaped)
    reject "duplicate checkpoint remove key" (remove literal <> remove escaped)


checkpointFileMetricsTest :: IO ()
checkpointFileMetricsTest = do
  let descriptor = ",\"deletionVector\":{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///vectors\",\"offset\":1,\"sizeInBytes\":32,\"cardinality\":9223372036854775807}"
      file path = "{\"add\":{\"path\":\"" <> path <> "\",\"size\":9223372036854775807,\"modificationTime\":0,\"dataChange\":true" <> descriptor <> "}}\n"
  actions <- either fail pure (D.parseLogFileChecked (file "b.parquet" <> file "a%20a.parquet" <> file "c.parquet"))
  checkpoint <- either fail pure (foldM Checkpoint.applyCheckpointAction Checkpoint.emptyCheckpointSnapshot actions)
  uncached <- either fail pure (Checkpoint.finishCheckpointForReplay checkpoint)
  cached <- either fail pure (Checkpoint.finishCheckpointForReplayWithMetrics True checkpoint)
  let totals = D.SnapshotFileMetrics 9223372036854775805 1 3 9223372036854775805 1
  (D.cachedFileMetrics uncached == Nothing) `shouldBe` True
  (D.cachedFileMetrics cached == Just totals) `shouldBe` True
  (D.snapshotFileMetrics uncached == totals) `shouldBe` True
  cached `shouldBe` uncached


checkpointPathOrderTest :: IO ()
checkpointPathOrderTest = do
  let group = PN.NSStruct . V.fromList
      leaf = PN.NSRequired . PN.NSPrimitive
      fields = V.singleton ("add", group
        [ ("path", leaf PN.LtString)
        , ("size", leaf PN.LtInt64)
        , ("modificationTime", leaf PN.LtInt64)
        , ("dataChange", leaf PN.LtBool)
        ])
      checkpoint paths = PN.buildNestedFile fields (V.singleton (V.fromList (map row paths)))
      row name = PN.NRStruct . V.fromList . map PN.NRLeaf $
        [PN.LvString name, PN.LvInt64 1, PN.LvInt64 0, PN.LvBool True]
      interleaved = concatMap (\suffix -> [path "z" suffix, path "é" suffix, path "a" suffix]) ['\x400' .. '\x43f']
      path initial suffix = initial <> "bcdefghijk/" <> T.singleton suffix
      prefixCases = "a" : "a\0" : "a\0b" : "😀" : interleaved
      longPrefixCases = map ("a/common/directory/with/long/path/" <>) prefixCases
      constantBytes = concatMap (\suffix -> map (\initial -> initial <> "-fixed-" <> T.singleton suffix) ["z", "b", "a"]) ['a' .. 'z']
      check paths = do
        bytes <- either fail pure (checkpoint paths)
        snapshot <- either fail pure (Checkpoint.snapshotFromCheckpoint bytes)
        Map.keys (D.tsFiles snapshot) `shouldBe` sort paths
        map (\key -> D.addPath <$> Map.lookup key (D.tsFiles snapshot)) paths `shouldBe` map Just paths
  mapM_ check [prefixCases, longPrefixCases, constantBytes]
  bytes <- either fail pure (checkpoint (interleaved ++ ["a", "a"]))
  Checkpoint.snapshotFromCheckpoint bytes `shouldBe` Left "Delta.Checkpoint: duplicate add path"
  aliased <- either fail pure (checkpoint ["space here.parquet", "space%20here.parquet"])
  Checkpoint.snapshotFromCheckpoint aliased `shouldBe` Left "Delta.Checkpoint: duplicate add path"


snapshotUriEscapesTest :: IO ()
snapshotUriEscapesTest = withCorruptCheckpoint $ \root logDir -> do
  let first = logDir </> "00000000000000000000.json"
      second = logDir </> "00000000000000000001.json"
      add path = "{\"add\":{\"path\":\"" <> path <> "\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
      remove path = "{\"remove\":{\"path\":\"" <> path <> "\",\"dataChange\":true}}\n"
      adds = add "%7e%61.parquet" <> add "part%2fdata.parquet" <> add "part/data.parquet"
        <> add "part%252fdata.parquet" <> add "a+b.parquet" <> add "a%2bb.parquet"
      removes = remove "~a.parquet" <> remove "part%2Fdata.parquet"
      expected = ["a%2bb.parquet", "a+b.parquet", "live.parquet", "part%252fdata.parquet", "part/data.parquet"]
      paths = fmap DIO.activeFilePaths <$> DIO.openDeltaTable root
  BL.appendFile first adds
  BL.writeFile second removes
  paths `shouldReturn` Right expected
  original <- BL.readFile first
  let actions = D.parseLogFile (original <> removes)
      files = map D.addPath . Map.elems . D.tsFiles
  files (foldl' D.applyAction D.emptySnapshot actions) `shouldBe` expected
  files (D.snapshotFromActions actions) `shouldBe` expected
  let initial = D.snapshotFromActions (D.parseLogFile original)
      afterReplacement = initial {D.tsFiles = Map.delete "%7e%61.parquet" (D.tsFiles initial)}
  getField @"tsFiles" initial `shouldBe` D.tsFiles initial
  files (foldl' D.applyAction afterReplacement (D.parseLogFile removes)) `shouldBe` expected
  let reAdded = foldl' D.applyAction initial (D.parseLogFile (add "~a.parquet" <> add "%7E%61.parquet"))
  Map.member "~a.parquet" (D.tsFiles reAdded) `shouldBe` False
  Map.member "%7E%61.parquet" (D.tsFiles reAdded) `shouldBe` True
  files (foldl' D.applyAction reAdded (D.parseLogFile removes)) `shouldBe` expected
  let bothSpellings = initial {D.tsFiles = Map.union (D.tsFiles initial) (D.tsFiles reAdded)}
      duplicated = D.parseLogFile (remove "~a.parquet" <> remove "~a.parquet")
  Map.keys (D.tsFiles (foldl' D.applyAction bothSpellings duplicated))
    `shouldSatisfy` all (`notElem` ["%7e%61.parquet", "%7E%61.parquet"])
  BL.writeFile second (remove "a%2Bb.parquet")
  updated <- DIO.openDeltaTable root >>= either fail pure
  DIO.activeFilePaths updated `shouldSatisfy` elem "a+b.parquet"
  DIO.activeFilePaths updated `shouldSatisfy` notElem "a%2bb.parquet"


emptyPartitionValueTest :: IO ()
emptyPartitionValueTest = do
  let check values = values `shouldBe` Map.fromList [("empty", Nothing), ("null", Nothing), ("nonempty", Just "value")]
      fields = "\"path\":\"file.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"partitionValues\":{\"empty\":\"\",\"null\":null,\"nonempty\":\"value\"}"
      action name = D.parseLogLine ("{\"" <> name <> "\":{" <> fields <> "}}")
  case (action "add", action "remove", action "cdc") of
    (Just (D.ActionAdd add), Just (D.ActionRemove remove), Just (D.ActionCdc cdc)) -> do
      check (D.addPartitionValues add)
      check (D.removePartitionValues remove)
      check (D.cdcPartitionValues cdc)
    other -> expectationFailure ("unexpected actions: " ++ show other)


signedTimestampsTest :: IO ()
signedTimestampsTest =
  case D.parseLogFile
    "{\"add\":{\"path\":\"file.parquet\",\"size\":1,\"modificationTime\":-1,\"dataChange\":true}}\n\
    \{\"remove\":{\"path\":\"file.parquet\",\"deletionTimestamp\":-2,\"dataChange\":true}}\n\
    \{\"metaData\":{\"id\":\"test\",\"schemaString\":\"\",\"createdTime\":-3}}\n\
    \{\"commitInfo\":{\"timestamp\":-4}}\n" of
    [D.ActionAdd add, D.ActionRemove remove, D.ActionMetaData metadata, D.ActionCommitInfo info] -> do
      D.addModificationTime add `shouldBe` -1
      D.removeDeletionTimestamp remove `shouldBe` Just (-2)
      D.mdCreatedTime metadata `shouldBe` Just (-3)
      D.ciTimestamp info `shouldBe` Just (-4)
    other -> expectationFailure ("unexpected actions: " ++ show other)


jsonLongBoundsTest :: IO ()
jsonLongBoundsTest = do
  let largest = "9223372036854775807"
      overflow = "9223372036854775808"
      add n = "{\"add\":{\"path\":\"file.parquet\",\"size\":" <> n <> ",\"modificationTime\":0,\"dataChange\":true}}"
      remove n = "{\"remove\":{\"path\":\"file.parquet\",\"size\":" <> n <> ",\"dataChange\":true}}"
      cdc n = "{\"cdc\":{\"path\":\"file.parquet\",\"size\":" <> n <> ",\"dataChange\":false}}"
      checkpoint n = "{\"checkpointMetadata\":{\"version\":" <> n <> "}}"
      records n = "{\"numRecords\":" <> n <> "}"
      nestedCount n = "{\"nullCount\":{\"outer\":{\"inner\":" <> n <> "}}}"
      parses = either (const False) (const True)
      replay = parses . D.parseLogFileChecked . BL.fromStrict
      checkpointReplay = parses . D.foldCheckpointJson (flip (:)) []
      stats = parses . (Aeson.eitherDecodeStrict' :: BS.ByteString -> Either String D.AddStats)
      check parser build = do
        parser (build largest) `shouldBe` True
        parser (build (largest <> ".0")) `shouldBe` True
        parser (build "0") `shouldBe` True
        parser (build overflow) `shouldBe` False
        parser (build (overflow <> ".0")) `shouldBe` False
        parser (build "-1") `shouldBe` False
  mapM_ (check replay) [add, remove, cdc]
  check checkpointReplay checkpoint
  mapM_ (check stats) [records, nestedCount]


jsonExponentOverflowTest :: IO ()
jsonExponentOverflowTest = do
  let huge = "18446744073709551616" :: BS.ByteString
      add number = "{\"add\":{\"path\":\"file-1e" <> huge <> "\\\".parquet\",\"size\":"
        <> number <> ",\"modificationTime\":0,\"dataChange\":true}}"
      vector number = "{\"add\":{\"path\":\"file.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"deletionVector\":{\"cardinality\":"
        <> number <> "}}}"
      checkpoint number = "{\"checkpointMetadata\":{\"version\":" <> number <> "}}"
      pointer number = "{\"version\":" <> number <> ",\"size\":1}"
      transaction number = "{\"txn\":{\"appId\":\"client\",\"version\":" <> number <> "}}"
      schema number = "{\"type\":\"struct\",\"fields\":[{\"name\":\"id\",\"type\":\"long\",\"nullable\":true,\"metadata\":{\"id\":" <> number <> "}}]}"
      justStats number = do
        let payload = "{\"minValues\":{\"id\":" <> number <> ",\"label\":\"1e" <> TE.decodeUtf8 huge <> "\"}}" :: T.Text
            entry = "{\"add\":{\"path\":\"file.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"stats\":"
              <> Aeson.encode payload <> "}}"
        Just (D.ActionAdd file) <- pure (D.parseLogLine entry)
        Just result <- pure (D.decodeAddStats file)
        pure result
      expectExponentError result = case result of
        Left err -> err `shouldSatisfy` isInfixOf "exponent"
        Right _ -> expectationFailure "JSON number with an overflowing exponent was accepted"
      checked = D.parseLogFileChecked . BL.fromStrict
      validNumber = "10.0e-00000000000000000000000000001"
  checked (add validNumber) `shouldSatisfy` either (const False) (const True)
  validStats <- justStats "10.0E-00000000000000000000000000001" >>= either fail pure
  HM.lookup "id" (D.asMinValues validStats) `shouldBe` Just (Aeson.Number 1)
  let reject prefix = do
        let number = prefix <> huge
        expectExponentError (checked (add number))
        expectExponentError (checked (vector number))
        expectExponentError (D.foldCompactedBytes (flip (:)) [] (add number))
        expectExponentError (D.foldCheckpointJson (flip (:)) [] (checkpoint number))
        expectExponentError (D.parseDeltaSchema (schema (TE.decodeUtf8 number)))
        D.parseLogLine (BL.fromStrict (add number)) `shouldBe` Nothing
        D.parseLastCheckpoint (pointer number) `shouldBe` Nothing
        D.historyFirstTransaction (D.parseCommitHistory (transaction number)) `shouldBe` Nothing
        justStats (TE.decodeUtf8 number) >>= expectExponentError
  mapM_ reject ["1e+", "1E-", "1.0e"]


duplicateJsonKeysTest :: IO ()
duplicateJsonKeysTest = do
  let add fields = "{\"add\":{\"path\":\"file.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true," <> fields <> "}}"
      vector = "\"deletionVector\":{\"cardinality\":1,\"cardinality\":2}"
      txn = "{\"txn\":{\"appId\":\"client\",\"version\":1,\"version\":2}}"
      schema = "{\"type\":\"struct\",\"fields\":[{\"name\":\"id\",\"type\":\"long\",\"nullable\":true,\"metadata\":{\"a\":1,\"a\":2}}]}"
      statsText = "{\"minValues\":{\"nested\":{\"id\":1,\"id\":2}}}" :: T.Text
      rejected input = case D.parseLogFileChecked (BL.fromStrict input) of
        Left err -> err `shouldSatisfy` isInfixOf "duplicate JSON key"
        Right actions -> expectationFailure ("accepted ambiguous JSON " ++ show actions)
      bad = map add
        [ "\"\\u0070ath\":\"other.parquet\""
        , "\"partitionValues\":{\"p\":\"west\",\"p\":\"east\"}"
        , "\"tags\":{\"k\":null,\"k\":\"v\"}"
        , vector
        , "\"future\":[{\"k\":0,\"k\":1}]"
        , "\"future\":0,\"future\":1"
        ]
  mapM_ rejected (txn : bad)
  D.parseLogLine (BL.fromStrict (add vector)) `shouldBe` Nothing
  D.historyFirstTransaction (D.parseCommitHistory txn) `shouldBe` Nothing
  let mixed = "{\"commit\\u0049nfo\":{\"operation\":\"WRITE\"},\"future\":null}"
  D.parseLogLine (BL.fromStrict mixed) `shouldBe` Nothing
  D.parseCommitInfo mixed `shouldBe` Nothing
  case D.parseDeltaSchema schema of
    Left err -> err `shouldSatisfy` isInfixOf "duplicate JSON key"
    Right fields -> expectationFailure ("accepted ambiguous schema " ++ show fields)
  let withStats = BL.fromStrict (add ("\"stats\":" <> BL.toStrict (Aeson.encode statsText)))
  Just (D.ActionAdd file) <- pure (D.parseLogLine withStats)
  case D.decodeAddStats file of
    Just (Left err) -> err `shouldSatisfy` isInfixOf "duplicate JSON key"
    result -> expectationFailure ("accepted ambiguous statistics " ++ show result)
  D.parseLogFileChecked (BL.fromStrict (add "\"future\":[{\"k\":0},{\"k\":1}]"))
    `shouldSatisfy` either (const False) (const True)
  withCorruptCheckpoint $ \root logDir -> do
    let next = logDir </> "00000000000000000001.json"
        lastCommit = logDir </> "00000000000000000002.json"
        compacted = logDir </> "00000000000000000001.00000000000000000002.compacted.json"
    BL.writeFile lastCommit "{\"txn\":{\"appId\":\"client\",\"version\":8}}\n"
    BS.writeFile compacted (add vector)
    table <- DIO.openDeltaTable root >>= either fail pure
    DIO.activeFilePaths table `shouldBe` ["live.parquet"]
    removeFile compacted
    BS.writeFile next (add vector)
    DIO.openDeltaTable root >>= \case
      Left err -> err `shouldSatisfy` isInfixOf "duplicate JSON key"
      Right _ -> expectationFailure "replayed an ambiguous deletion-vector descriptor"


corruptCheckpointRecoveryTest :: IO ()
corruptCheckpointRecoveryTest = withCorruptCheckpoint $ \root _ -> do
  let check result = case result of
        Left err -> expectationFailure err
        Right table -> do
          DIO.activeFilePaths table `shouldBe` ["live.parquet"]
          D.tsAppIds (DIO.dtSnapshot table) `shouldBe` Map.singleton "client" 7
          DIO.dtVersion table `shouldBe` Just 1
  DIO.openDeltaTable root >>= check
  DIO.openDeltaTableAt root 1 >>= check


checkpointPointerAddCountTest :: IO ()
checkpointPointerAddCountTest = withCorruptCheckpoint $ \root logDir -> do
  let stem = logDir </> "00000000000000000001"
      classic = stem ++ ".checkpoint.parquet"
      part ordinal = stem ++ ".checkpoint." ++ ordinal ++ ".0000000002.parquet"
      pointer = logDir </> "_last_checkpoint"
      required name ty = (name, PN.NSRequired (PN.NSPrimitive ty))
      fields name children = (name, PN.NSOptional (PN.NSStruct (V.fromList children)))
      action = PN.NRStruct . V.fromList . map PN.NRLeaf
      protocol = fields "protocol" [required "minReaderVersion" PN.LtInt32, required "minWriterVersion" PN.LtInt32]
      protocolRow = action [PN.LvInt32 1, PN.LvInt32 2]
      metadata = fields "metaData" [required "id" PN.LtString, required "schemaString" PN.LtString]
      metadataRow = action [PN.LvString "table", PN.LvString "{\"type\":\"struct\",\"fields\":[]}"]
      write path schema rows = either fail (BS.writeFile path) (PN.buildNestedFile schema rows)
      latest = DIO.openDeltaTable root
      asOf = DIO.openDeltaTableAt root 1
      expectFiles files opener = do
        table <- opener >>= either fail pure
        DIO.activeFilePaths table `shouldBe` files
        pure (D.tsAppIds (DIO.dtSnapshot table))
      recover opener = expectFiles ["live.parquet"] opener >>= (`shouldBe` Map.singleton "client" 7)
      reject opener = opener >>= \case
        Left err -> err `shouldSatisfy` isInfixOf "_last_checkpoint numOfAddFiles mismatch"
        Right _ -> expectationFailure "opened an incomplete checkpoint without a complete log"
  write classic (V.fromList [protocol, metadata])
    (V.fromList [V.fromList [protocolRow, PN.NRNull], V.fromList [PN.NRNull, metadataRow]])
  BL.writeFile pointer "{\"version\":0,\"size\":3,\"numOfAddFiles\":1}"
  expectFiles [] latest >>= (`shouldBe` Map.empty)
  BL.writeFile pointer "{\"version\":1,\"size\":2,\"numOfAddFiles\":0}"
  expectFiles [] latest >>= (`shouldBe` Map.empty)
  BL.writeFile pointer "{\"version\":1,\"size\":4,\"numOfAddFiles\":1}"
  mapM_ recover [latest, asOf]
  removeFile (logDir </> "00000000000000000000.json")
  mapM_ reject [latest, asOf]
  removeFile classic
  write (part "0000000001") (V.singleton protocol) (V.singleton (V.singleton protocolRow))
  write (part "0000000002") (V.singleton metadata) (V.singleton (V.singleton metadataRow))
  expectFiles [] latest >>= (`shouldBe` Map.empty)
  BL.writeFile pointer "{\"version\":1,\"parts\":2,\"size\":4,\"numOfAddFiles\":1}"
  mapM_ reject [latest, asOf]


v2CheckpointPointerTest :: IO ()
v2CheckpointPointerTest = withCorruptCheckpoint $ \root logDir -> do
  let version = "00000000000000000001"
      name = version ++ ".checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json"
      pointer = logDir </> "_last_checkpoint"
      header =
        "{\"checkpointMetadata\":{\"version\":1}}\n"
          <> "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\"]}}\n"
          <> "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
      file = "{\"add\":{\"path\":\"checkpoint.parquet\",\"size\":42,\"modificationTime\":0,\"dataChange\":false}}\n"
      writePointer manifest = BL.writeFile pointer $ Aeson.encode $
        Aeson.object
          [ "version" Aeson..= (1 :: Int)
          , "size" Aeson..= (4 :: Int)
          , "numOfAddFiles" Aeson..= (1 :: Int)
          , "v2Checkpoint" Aeson..= Aeson.object ["path" Aeson..= manifest]
          ]
      open = DIO.openDeltaTable root
      files = fmap DIO.activeFilePaths <$> open
  writePointer name
  BL.writeFile (logDir </> name) (header <> file)
  files `shouldReturn` Right ["checkpoint.parquet"]
  BL.writeFile (logDir </> name) header
  files `shouldReturn` Right ["live.parquet"]
  removeFile (logDir </> "00000000000000000000.json")
  DIO.openDeltaTableAt root 1 >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "_last_checkpoint numOfAddFiles mismatch"
    Right _ -> expectationFailure "accepted a V2 checkpoint missing an add"
  writePointer (version ++ ".checkpoint.11111111-2222-3333-4444-555555555555.parquet")
  files `shouldReturn` Right []


corruptCheckpointGapTest :: IO ()
corruptCheckpointGapTest = withCorruptCheckpoint $ \root logDir -> do
  removeFile (logDir </> "00000000000000000000.json")
  checkReplayError root "missing JSON commit at version 0"


corruptCheckpointJsonTest :: IO ()
corruptCheckpointJsonTest = withCorruptCheckpoint $ \root logDir -> do
  BL.writeFile (logDir </> "00000000000000000000.json") "{\"add\":{}}\n"
  checkReplayError root "invalid Delta action at line 1"


versionChecksumRecoveryTest :: IO ()
versionChecksumRecoveryTest = withCorruptCheckpoint $ \root logDir -> do
  let latest = logDir </> "00000000000000000001.json"
      checksum = logDir </> "00000000000000000001.crc"
      third = logDir </> "00000000000000000002.json"
      thirdChecksum = logDir </> "00000000000000000002.crc"
      protocol = "\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":7,\"writerFeatures\":[\"domainMetadata\"]}"
      metadata = "\"metadata\":{\"id\":\"recovered\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\",\"partitionColumns\":[],\"configuration\":{},\"format\":{\"provider\":\"parquet\",\"options\":{}}}"
      file path = "{\"path\":\"" <> path <> "\",\"size\":42,\"partitionValues\":{},\"modificationTime\":0,\"dataChange\":false}"
      committedFile path = "{\"path\":\"" <> path <> "\",\"size\":42,\"partitionValues\":{},\"modificationTime\":0,\"dataChange\":true}"
      domain = "{\"domain\":\"test\",\"configuration\":\"abc\",\"removed\":false}"
      transaction = "{\"appId\":\"client\",\"version\":-3}"
      domains = "\"domainMetadata\":[" <> domain <> "]"
      transactions = "\"setTransactions\":[" <> transaction <> "]"
      payload files count size txns =
        "{" <> protocol <> "," <> metadata <> "," <> domains <> "," <> txns
          <> ",\"numProtocol\":1,\"numMetadata\":1,\"numFiles\":" <> count
          <> ",\"tableSizeBytes\":" <> size <> ",\"allFiles\":[" <> files <> "]}"
      complete = payload (file "recovered.parquet") "1" "42" transactions
      recover opener = do
        table <- opener >>= either fail pure
        DIO.activeFilePaths table `shouldBe` ["recovered.parquet"]
        let snapshot = DIO.dtSnapshot table
        fmap D.mdId (D.tsMetaData snapshot) `shouldBe` Just "recovered"
        D.tsAppIds snapshot `shouldBe` Map.singleton "client" (-3)
        D.tsDomains snapshot `shouldBe` Map.singleton "test" "abc"
        D.tsLastCommit snapshot `shouldBe` Nothing
  BL.writeFile checksum complete
  healthy <- DIO.openDeltaTable root >>= either fail pure
  DIO.activeFilePaths healthy `shouldBe` ["live.parquet"]
  BL.writeFile latest ""
  recover (DIO.openDeltaTable root)
  recover (DIO.openDeltaTableAt root 1)
  events <- DIO.streamDeltaCommits root 1 1 >>= either fail pure
  iterIOToList events >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "empty Delta commit"
    Right _ -> expectationFailure "read change events from a zero-byte commit"
  removeFile checksum
  checkReplayError root "empty Delta commit"
  BL.writeFile checksum complete
  let identified = BL.init complete <> ",\"txnId\":\"original\"}"
      boundary txnId = "{\"commitInfo\":{\"txnId\":\"" <> txnId
        <> "\"}}\n{\"remove\":{\"path\":\"live.parquet\",\"dataChange\":true}}\n"
  BL.writeFile latest (boundary "original")
  BL.writeFile checksum identified
  recoveredBoundary <- DIO.openDeltaTable root >>= either fail pure
  DIO.activeFilePaths recoveredBoundary `shouldBe` ["recovered.parquet"]
  boundaryEvents <- DIO.streamDeltaCommits root 1 1 >>= either fail pure
  iterIOToList boundaryEvents >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "original change events are incomplete"
    Right _ -> expectationFailure "read change events from an incomplete commit"
  let identifiedHeader = "{" <> protocol <> "," <> metadata
        <> ",\"numProtocol\":1,\"numMetadata\":1,\"numFiles\":1,\"tableSizeBytes\":42,\"txnId\":\"original\""
      listedFile = "\"allFiles\":[" <> file "recovered.parquet" <> "]"
      incomplete fields missing = do
        BL.writeFile checksum (identifiedHeader <> "," <> fields <> "}")
        let problem = "replayed file metrics disagree with the version checksum; recovery requires " <> missing
        checkReplayError root problem
        changes <- DIO.streamDeltaCommits root 1 1 >>= either fail pure
        iterIOToList changes >>= \case
          Left err -> err `shouldSatisfy` isInfixOf problem
          Right _ -> expectationFailure "read change events despite a checksum discrepancy"
  incomplete (transactions <> "," <> domains) "allFiles"
  incomplete (listedFile <> "," <> domains) "setTransactions"
  incomplete (listedFile <> "," <> transactions) "domainMetadata"
  BL.writeFile checksum (identifiedHeader <> ",\"allFiles\":[]}")
  inconsistentChecksum <- DIO.openDeltaTable root >>= either fail pure
  DIO.activeFilePaths inconsistentChecksum `shouldBe` []
  let metricLast = "{\"numFiles\":1," <> listedFile <> ",\"padding\":\"" <> BL.replicate 1024 120
        <> "\"," <> protocol <> "," <> metadata <> "," <> transactions <> "," <> domains
        <> ",\"numProtocol\":1,\"numMetadata\":1,\"txnId\":\"original\",\"tableSizeBytes\":42}"
  -- The replay has the correct file count and a wrong byte total. The
  -- surviving add contradicts the checksum, so replay cannot recover it.
  BL.writeFile latest (boundary "original" <> "{\"add\":{\"path\":\"wrong.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n")
  BL.writeFile checksum metricLast
  checkReplayError root "checksum add file disagrees"
  BL.writeFile latest (boundary "original")
  BL.writeFile checksum identified
  BL.writeFile latest (boundary "different")
  staleChecksum <- DIO.openDeltaTable root >>= either fail pure
  DIO.activeFilePaths staleChecksum `shouldBe` []
  BL.writeFile checksum complete
  BL.writeFile latest "{\"add\":{\"pa"
  recover (DIO.openDeltaTable root)
  recover (DIO.openDeltaTableAt root 1)
  let extraField suffix raw = BL.init raw <> "," <> suffix <> "}"
      withBounds = extraField "\"stats\":\"{\\\"numRecords\\\":1,\\\"maxValues\\\":{\\\"id\\\":1}}\"" (file "recovered.parquet")
  BL.writeFile checksum (payload withBounds "1" "42" transactions)
  fromChecksum <- DIO.openDeltaTable root >>= either fail pure
  map D.decodeAddStats (DIO.dtActiveFiles fromChecksum) `shouldBe` [Nothing]
  BL.writeFile latest ("{\"add\":" <> withBounds <> "}\n{\"txn\":")
  fromCommit <- DIO.openDeltaTable root >>= either fail pure
  map D.decodeAddStats (DIO.dtActiveFiles fromCommit)
    `shouldBe` [Just (Right (D.AddStats (Just 1) mempty (HM.singleton "id" (Aeson.Number 1)) mempty mempty Nothing))]
  BL.writeFile latest "{\"add\":{\"pa"
  BL.writeFile checksum (payload (extraField "\"future\":{\"child\":[1,2]}" (file "recovered.parquet")) "1" "42" transactions)
  recover (DIO.openDeltaTable root)
  let invalid bytes reason = do
        BL.writeFile checksum bytes
        checkReplayError root "invalid Delta action"
        checkReplayError root reason
      withSizeHistogram counts sizes = extraField
        ("\"fileSizeHistogram\":{\"sortedBinBoundaries\":[0,42],\"fileCounts\":" <> counts <> ",\"totalBytes\":" <> sizes <> "}")
      withDeletedHistogram counts = extraField
        ("\"deletedRecordCountsHistogramOpt\":{\"deletedRecordCounts\":" <> counts <> "}")
      wrongSizeHistogram = withSizeHistogram "[1,0]" "[42,0]"
  BL.writeFile checksum (withSizeHistogram "[0,1]" "[0,42]" (withDeletedHistogram "[1,0,0,0,0,0,0,0,0,0]" complete))
  recover (DIO.openDeltaTable root)
  invalid (wrongSizeHistogram complete) "fileSizeHistogram fileCounts mismatch"
  invalid (withDeletedHistogram "[0,1,0,0,0,0,0,0,0,0]" complete) "deletedRecordCountsHistogramOpt mismatch"
  BL.writeFile latest (boundary "original")
  BL.writeFile checksum (wrongSizeHistogram identified)
  checkReplayError root "fileSizeHistogram fileCounts mismatch"
  corruptEvents <- DIO.streamDeltaCommits root 1 1 >>= either fail pure
  iterIOToList corruptEvents >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "fileSizeHistogram fileCounts mismatch"
    Right _ -> expectationFailure "read change events despite a contradictory checksum"
  BL.writeFile latest "{\"add\":{\"pa"
  invalid (payload (file "recovered.parquet") "2" "42" transactions) "numFiles mismatch"
  invalid (payload (file "recovered.parquet") "1" "41" transactions) "tableSizeBytes mismatch"
  invalid (payload (file "x.parquet" <> "," <> file "%78.parquet") "2" "84" transactions) "duplicate add path"
  invalid (payload (file "recovered.parquet") "1" "42" "\"setTransactions\":null") "setTransactions"
  invalid
    (payload (file "recovered.parquet") "1" "42" "\"setTransactions\":[{\"appId\":\"client\",\"version\":-3},{\"appId\":\"client\",\"version\":8}]")
    "duplicate checkpoint transaction identifier"
  invalid ("{\"numFiles\":1," <> BL.drop 1 complete) "duplicate JSON key"
  invalid (extraField "\"all\\u0046iles\":[]" complete) "duplicate JSON key"
  invalid
    (payload (extraField "\"p\\u0061th\":\"duplicate.parquet\"" (file "recovered.parquet")) "1" "42" transactions)
    "duplicate JSON key"
  invalid
    (payload (extraField "\"future\":{\"child\":1,\"child\":2}" (file "recovered.parquet")) "1" "42" transactions)
    "duplicate JSON key"
  invalid (extraField "\"future\":1e999999999999999999999999" complete) "exponent exceeds"
  invalid (complete <> "[]") "trailing bytes"
  BL.writeFile checksum complete
  BL.writeFile latest "{\"add\":{}}"
  checkReplayError root "invalid Delta action"
  BL.writeFile latest "{\"add\":{\"pa\n"
  checkReplayError root "invalid Delta action"
  BL.writeFile latest $
    "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2}}\n"
      <> "{\"add\":{\"pa"
  checkReplayError root "checksum protocol or metadata disagrees"
  let truncatedAfter action = BL.writeFile latest (action <> "\n{\"add\":{\"pa")
  truncatedAfter ("{\"add\":" <> file "different.parquet" <> "}")
  checkReplayError root "checksum add file disagrees"
  truncatedAfter "{\"remove\":{\"path\":\"%72ecovered.parquet\",\"dataChange\":true}}"
  checkReplayError root "checksum remove file disagrees"
  truncatedAfter "{\"txn\":{\"appId\":\"client\",\"version\":8}}"
  checkReplayError root "checksum transaction disagrees"
  truncatedAfter "{\"txn\":{\"appId\":\"client\",\"version\":-3,\"lastUpdated\":77}}"
  checkReplayError root "checksum transaction disagrees"
  let mismatchedId = extraField "\"txnId\":\"wrong\"" complete
  BL.writeFile checksum mismatchedId
  truncatedAfter "{\"commitInfo\":{\"txnId\":\"original\"}}"
  checkReplayError root "checksum transaction ID disagrees"
  BL.writeFile checksum complete
  truncatedAfter "{\"domainMetadata\":{\"domain\":\"test\",\"configuration\":\"abc\",\"removed\":true}}"
  checkReplayError root "checksum domain metadata disagrees"
  truncatedAfter "{\"domainMetadata\":{\"domain\":\"test\",\"configuration\":\"other\",\"removed\":false}}"
  checkReplayError root "checksum domain metadata disagrees"
  truncatedAfter $
    "{\"add\":" <> committedFile "%72ecovered.parquet" <> "}\n"
      <> "{\"txn\":" <> transaction <> "}\n"
      <> "{\"domainMetadata\":" <> domain <> "}"
  recover (DIO.openDeltaTable root)
  BL.writeFile latest "{\"add\":{\"pa"
  BL.writeFile third $
    "{\"commitInfo\":{\"operation\":\"WRITE\"}}\n"
      <> "{\"txn\":{\"appId\":\"client\",\"version\":8}}\n"
      <> "{\"remove\":{\"path\":\"%72ecovered.parquet\",\"dataChange\":true}}\n"
      <> "{\"add\":" <> file "third.parquet" <> "}\n"
  let afterReplay = do
        recovered <- DIO.openDeltaTable root >>= either fail pure
        DIO.dtVersion recovered `shouldBe` Just 2
        DIO.activeFilePaths recovered `shouldBe` ["third.parquet"]
        let snapshot = DIO.dtSnapshot recovered
        D.tsAppIds snapshot `shouldBe` Map.singleton "client" 8
        (D.ciOperation =<< D.tsLastCommit snapshot) `shouldBe` Just "WRITE"
  afterReplay
  BL.writeFile thirdChecksum "{}"
  afterReplay
  BL.writeFile thirdChecksum (payload (file "recovered.parquet") "1" "42" "\"setTransactions\":[{\"appId\":\"client\",\"version\":8}]")
  removeFile third
  recover (DIO.openDeltaTable root)


checksumPriorStatsTest :: IO ()
checksumPriorStatsTest = withCorruptCheckpoint $ \root logDir -> do
  let first = logDir </> "00000000000000000000.json"
      next = logDir </> "00000000000000000001.json"
      checksum = logDir </> "00000000000000000001.crc"
      protocol = "{\"minReaderVersion\":1,\"minWriterVersion\":2}"
      metadata = "{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}"
      file path upper = "{\"path\":\"" <> path <> "\",\"size\":42,\"modificationTime\":0,\"dataChange\":true,"
        <> "\"stats\":\"{\\\"numRecords\\\":2,\\\"maxValues\\\":{\\\"id\\\":" <> upper <> "}}\"}"
      crc old = "{\"txnId\":\"one\",\"numFiles\":2,\"tableSizeBytes\":84,\"numProtocol\":1,\"numMetadata\":1,\"protocol\":"
        <> protocol <> ",\"metadata\":" <> metadata <> ",\"setTransactions\":[],\"domainMetadata\":[],\"allFiles\":["
        <> old <> "," <> file "new.parquet" "8" <> "]}"
      original = file "live.parquet" "7"
      info = "{\"commitInfo\":{\"txnId\":\"one\"}}\n"
      readBounds = do
        table <- DIO.openDeltaTable root >>= either fail pure
        pure (Map.map (fmap (fmap D.asMaxValues) . D.decodeAddStats) (D.tsFiles (DIO.dtSnapshot table)))
      expectOld expected = readBounds >>= (`shouldBe` Map.fromList [("live.parquet", expected), ("new.parquet", Nothing)])
      bound = Just (Right (HM.singleton "id" (Aeson.Number 7)))
  BL.writeFile first ("{\"protocol\":" <> protocol <> "}\n{\"metaData\":" <> metadata <> "}\n{\"add\":" <> original <> "}\n")
  BL.writeFile checksum (crc original)
  BL.writeFile next info
  expectOld bound
  BL.writeFile next (info <> "{\"add\":")
  expectOld bound
  BL.writeFile checksum (crc (file "live.parquet" "4"))
  expectOld Nothing
  BL.writeFile checksum (crc original)
  removeFile first
  expectOld Nothing


checksumDeletionMetricsTest :: IO ()
checksumDeletionMetricsTest = withCorruptCheckpoint $ \root logDir -> do
  let initial = logDir </> "00000000000000000000.json"
      commit = logDir </> "00000000000000000001.json"
      checksum = logDir </> "00000000000000000001.crc"
      protocol = "{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"deletionVectors\"],\"writerFeatures\":[\"deletionVectors\"]}"
      metadata = "{\"id\":\"test\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\",\"configuration\":{\"delta.enableDeletionVectors\":\"true\"}}"
      file extra = "{\"path\":\"live.parquet\",\"size\":42,\"modificationTime\":0,\"dataChange\":true,\"partitionValues\":{}" <> extra <> "}"
      descriptor = ",\"deletionVector\":{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///tmp/delete.bin\",\"offset\":1,\"sizeInBytes\":32,\"cardinality\":2}"
      provenance = "{\"commitInfo\":{\"txnId\":\"original\"}}\n"
      remove = "{\"remove\":{\"path\":\"live.parquet\",\"dataChange\":true}}\n"
      add extra = "{\"add\":" <> file extra <> "}\n"
      crcWith vector claimed = "{\"txnId\":\"original\",\"numFiles\":1,\"tableSizeBytes\":42,\"numDeletionVectorsOpt\":1,\"numDeletedRecordsOpt\":"
        <> claimed <> ",\"numProtocol\":1,\"numMetadata\":1,\"protocol\":" <> protocol <> ",\"metadata\":" <> metadata
        <> ",\"setTransactions\":[],\"domainMetadata\":[],\"allFiles\":[" <> file vector <> "]}"
      crc = crcWith descriptor
      crcWithHistogram buckets =
        BL.init (crc "2") <> ",\"deletedRecordCountsHistogramOpt\":{\"deletedRecordCounts\":" <> buckets <> "}}"
      descriptorAt offset size count = ",\"deletionVector\":{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///tmp/delete.bin\",\"offset\":"
        <> offset <> ",\"sizeInBytes\":" <> size <> ",\"cardinality\":" <> count <> "}"
      countsLast = "{\"txnId\":\"original\",\"numFiles\":1,\"tableSizeBytes\":42,\"allFiles\":[" <> file descriptor
        <> "],\"padding\":\"" <> BL.replicate 1024 120 <> "\",\"numProtocol\":1,\"numMetadata\":1,\"protocol\":"
        <> protocol <> ",\"metadata\":" <> metadata
        <> ",\"setTransactions\":[],\"domainMetadata\":[],\"numDeletionVectorsOpt\":1,\"numDeletedRecordsOpt\":2}"
      readDV = do
        table <- DIO.openDeltaTable root >>= either fail pure
        case DIO.dtActiveFiles table of
          [active] -> pure (D.addDeletionVector active)
          _ -> fail "expected one active file"
  BL.writeFile initial ("{\"protocol\":" <> protocol <> "}\n{\"metaData\":" <> metadata <> "}\n" <> add "")
  BL.writeFile commit (provenance <> remove <> add descriptor)
  original <- readDV
  original `shouldSatisfy` (/= Nothing)
  BL.writeFile commit provenance
  readDV >>= (`shouldBe` Nothing)
  BL.writeFile checksum (crc "2")
  readDV >>= (`shouldBe` original)
  BL.writeFile checksum countsLast
  readDV >>= (`shouldBe` original)
  events <- DIO.streamDeltaCommits root 1 1 >>= either fail pure
  iterIOToList events >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "original change events are incomplete"
    Right _ -> expectationFailure "read incomplete change events"
  BL.writeFile commit (provenance <> remove <> add descriptor)
  table <- DIO.openDeltaTable root >>= either fail pure
  DIO.validateVersionChecksum table >>= (`shouldBe` Right True)
  BL.writeFile checksum (crcWithHistogram "[0,1,0,0,0,0,0,0,0,0]")
  DIO.validateVersionChecksum table >>= (`shouldBe` Right True)
  BL.writeFile checksum (crcWithHistogram "[1,0,0,0,0,0,0,0,0,0]")
  DIO.validateVersionChecksum table >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "deletedRecordCountsHistogramOpt mismatch"
    Right _ -> expectationFailure "accepted the wrong deletion count bucket"
  BL.writeFile checksum (crcWith (BL.init descriptor <> ",\"maxRowIndex\":11}") "2")
  DIO.validateVersionChecksum table >>= (`shouldBe` Right True)
  let rejectDescriptor bytes = do
        BL.writeFile checksum bytes
        DIO.validateVersionChecksum table >>= \case
          Left err -> err `shouldSatisfy` isInfixOf "descriptor disagrees"
          Right _ -> expectationFailure "accepted an incompatible deletion-vector descriptor"
  rejectDescriptor (crcWith (descriptorAt "0" "32" "2") "2")
  rejectDescriptor (crcWith (descriptorAt "1" "33" "2") "2")
  rejectDescriptor (crcWith (descriptorAt "1" "32" "3") "2")
  rejectDescriptor (crcWith (descriptorAt "1" "-1" "2") "2")
  let inline = ",\"deletionVector\":{\"storageType\":\"i\",\"pathOrInlineDv\":\"wi5b=000010000siXQKl0rr91000f55c8Xg0@@D72lkbi5=-{L\",\"sizeInBytes\":40,\"cardinality\":6}"
  BL.writeFile commit (provenance <> remove <> add inline)
  inlineTable <- DIO.openDeltaTable root >>= either fail pure
  BL.writeFile checksum (crcWith (BL.init inline <> ",\"offset\":null}") "6")
  DIO.validateVersionChecksum inlineTable >>= (`shouldBe` Right True)
  BL.writeFile checksum (crc "8")
  DIO.validateVersionChecksum table >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "numDeletedRecordsOpt mismatch"
    Right _ -> expectationFailure "accepted the wrong deletion count"


versionChecksumTimestampRecoveryTest :: IO ()
versionChecksumTimestampRecoveryTest = withCorruptCheckpoint $ \root logDir -> do
  let commit0 = logDir </> "00000000000000000000.json"
      commit1 = logDir </> "00000000000000000001.json"
      commit2 = logDir </> "00000000000000000002.json"
      checksum = logDir </> "00000000000000000001.crc"
      protocol = "{\"minReaderVersion\":1,\"minWriterVersion\":7,\"writerFeatures\":[\"inCommitTimestamp\"]}"
      metadata = "{\"id\":\"clock\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\",\"configuration\":{\"delta.enableInCommitTimestamps\":\"true\"}}"
      commitInfo stamp = "{\"commitInfo\":{\"inCommitTimestamp\":" <> stamp <> "}}\n"
      crc stamp count =
        "{\"protocol\":" <> protocol <> ",\"metadata\":" <> metadata
          <> ",\"numProtocol\":1,\"numMetadata\":1,\"numFiles\":" <> count
          <> ",\"tableSizeBytes\":0,\"allFiles\":[],\"setTransactions\":[],\"domainMetadata\":[],\"inCommitTimestampOpt\":" <> stamp <> "}"
      partialStamp = "{\"commitInfo\":{\"inCommitTi"
      at stamp = DIO.openDeltaTableAtTimestamp root (posixSecondsToUTCTime (stamp / 1000))
      match stamp version = do
        table <- at stamp >>= either fail pure
        DIO.dtVersion table `shouldBe` Just version
      reject stamp = at stamp >>= \case
        Left err -> err `shouldSatisfy` isInfixOf "missing inCommitTimestamp at version 1"
        Right _ -> expectationFailure "used a checksum without a valid recovery snapshot"
  BL.writeFile commit0 (commitInfo "12000" <> "{\"protocol\":" <> protocol <> "}\n{\"metaData\":" <> metadata <> "}\n")
  BL.writeFile commit1 partialStamp
  BL.writeFile commit2 (commitInfo "12200")
  BL.writeFile checksum (crc "12100" "0")
  match 12099 0
  match 12100 1
  match 12199 1
  match 12200 2
  table <- DIO.openDeltaTableAt root 1 >>= either fail pure
  (D.ciInCommitTimestamp =<< D.tsLastCommit (DIO.dtSnapshot table)) `shouldBe` Just 12100
  latest <- DIO.openDeltaTable root >>= either fail pure
  entries <- DIO.historyEntries latest
  map DIO.heTimestamp entries `shouldBe` [Just 12200, Just 12100, Just 12000]
  BL.writeFile (logDir </> "00000000000000000002.checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json") $
    "{\"checkpointMetadata\":{\"version\":2}}\n"
      <> "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\",\"inCommitTimestamp\"]}}\n"
      <> "{\"metaData\":" <> metadata <> "}\n"
  BL.writeFile checksum (crc "12199" "1")
  reject 12100
  BL.writeFile checksum (crc "12199" "0")
  BL.writeFile commit1 (commitInfo "12100" <> "{\"add\":{\"pa")
  DIO.openDeltaTableAt root 1 >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "checksum in-commit timestamp disagrees"
    Right _ -> expectationFailure "recovered a snapshot with a conflicting in-commit timestamp"
  BL.writeFile commit1 (commitInfo "12100")
  intact <- DIO.openDeltaTableAt root 1 >>= either fail pure
  DIO.validateVersionChecksum intact >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "inCommitTimestamp mismatch"
    Right _ -> expectationFailure "validated a checksum with a conflicting in-commit timestamp"
  BL.writeFile commit1 "{\"txn\":{\"appId\":\"complete\",\"version\":1}}\n"
  reject 12100
  let checkpointProtocol = "{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\",\"inCommitTimestamp\"]}"
      checkpoint = logDir </> "00000000000000000001.checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json"
      checkpointSum props = "{\"protocol\":" <> checkpointProtocol <> ",\"metadata\":" <> props
        <> ",\"numProtocol\":1,\"numMetadata\":1,\"numFiles\":0,\"tableSizeBytes\":0,\"allFiles\":[],\"setTransactions\":[],\"domainMetadata\":[],\"inCommitTimestampOpt\":12100}"
      checkpointInfo = (D.ciInCommitTimestamp =<<) . D.tsLastCommit . DIO.dtSnapshot
      openCheckpoint = DIO.openDeltaTableAt root 1 >>= either fail pure
  BL.writeFile checkpoint $
    "{\"checkpointMetadata\":{\"version\":1}}\n{\"protocol\":" <> checkpointProtocol <> "}\n{\"metaData\":" <> metadata <> "}\n"
  BL.writeFile commit1 partialStamp
  BL.writeFile checksum (checkpointSum (BL.init metadata <> ",\"name\":\"wrong table\"}"))
  fmap checkpointInfo openCheckpoint `shouldReturn` Nothing
  reject 12100
  checkpointLatest <- DIO.openDeltaTable root >>= either fail pure
  fmap (lookup 1 . map (\entry -> (DIO.heVersion entry, DIO.heTimestamp entry))) (DIO.historyEntries checkpointLatest)
    `shouldReturn` Just Nothing
  BL.writeFile checksum (checkpointSum metadata)
  fmap checkpointInfo openCheckpoint `shouldReturn` Just 12100
  byTime <- at 12100 >>= either fail pure
  checkpointInfo byTime `shouldBe` Just 12100
  fmap (lookup 1 . map (\entry -> (DIO.heVersion entry, DIO.heTimestamp entry))) (DIO.historyEntries checkpointLatest)
    `shouldReturn` Just (Just 12100)


versionChecksumValidationTest :: IO ()
versionChecksumValidationTest = withCorruptCheckpoint $ \root logDir -> do
  let file = logDir </> "00000000000000000001.crc"
      protocol = "{\"minReaderVersion\":1,\"minWriterVersion\":2}"
      metadata = "{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}"
      payload count size protocol' metadata' =
        "{\"numFiles\":" <> count <> ",\"tableSizeBytes\":" <> size <> ",\"numProtocol\":1,\"numMetadata\":1,\"protocol\":"
          <> protocol' <> ",\"metadata\":" <> metadata' <> "}"
      correct = payload "1" "42" protocol metadata
      add path size = "{\"path\":\"" <> path <> "\",\"size\":" <> size
        <> ",\"modificationTime\":0,\"dataChange\":false,\"partitionValues\":{}}"
      files entries = BL.init correct <> ",\"allFiles\":[" <> entries <> "]}"
      statsFile stats = BL.init (add "live.parquet" "42") <> ",\"stats\":\"" <> stats <> "\"}"
  BL.appendFile (logDir </> "00000000000000000001.json") "{\"commitInfo\":{\"txnId\":\"original\"}}\n"
  BL.appendFile (logDir </> "00000000000000000001.json") $
    "{\"add\":" <> statsFile "{\\\"numRecords\\\":2,\\\"maxValues\\\":{\\\"id\\\":7}}" <> "}\n"
  table <- DIO.openDeltaTable root >>= either fail pure
  DIO.validateVersionChecksum table >>= (`shouldBe` Right False)
  BL.writeFile file correct
  DIO.validateVersionChecksum table >>= (`shouldBe` Right True)
  let reject bytes message = do
        BL.writeFile file bytes
        DIO.validateVersionChecksum table >>= \case
          Left err -> do
            err `shouldSatisfy` isInfixOf "00000000000000000001.crc"
            err `shouldSatisfy` isInfixOf message
          Right _ -> expectationFailure "accepted a mismatched version checksum"
  reject (payload "2" "42" protocol metadata) "numFiles mismatch"
  reject (payload "1" "43" protocol metadata) "tableSizeBytes mismatch"
  let attach field value = BL.init correct <> ",\"" <> field <> "\":" <> value <> "}"
      sizeBins bounds counts bytes = "{\"sortedBinBoundaries\":" <> bounds <> ",\"fileCounts\":" <> counts <> ",\"totalBytes\":" <> bytes <> "}"
      validBins = sizeBins "[0,42,100]" "[0,1,0]" "[0,42,0]"
      deletedBins buckets = attach "deletedRecordCountsHistogramOpt" ("{\"deletedRecordCounts\":" <> buckets <> "}")
  forM_ ["fileSizeHistogram", "histogramOpt"] $ \name -> do
    BL.writeFile file (attach name validBins)
    DIO.validateVersionChecksum table >>= (`shouldBe` Right True)
  reject (attach "fileSizeHistogram" (sizeBins "[0,42,100]" "[1,0,0]" "[42,0,0]")) "fileCounts mismatch"
  reject (attach "fileSizeHistogram" (sizeBins "[0,42,100]" "[0,1,0]" "[42,0,0]")) "totalBytes mismatch"
  reject (attach "fileSizeHistogram" (sizeBins "[0,42,42]" "[0,1,0]" "[0,42,0]")) "invalid bin boundaries"
  reject (attach "fileSizeHistogram" (sizeBins "[0,42]" "[1]" "[42,0]")) "bin lengths mismatch"
  reject (attach "histogramOpt" (sizeBins "[0]" "[-1]" "[42]")) "nonnegative"
  reject (attach "fileSizeHistogram" (sizeBins "[0]" "[1]" "[9223372036854775808]")) "totalBytes"
  BL.writeFile file (deletedBins "[1,0,0,0,0,0,0,0,0,0]")
  DIO.validateVersionChecksum table >>= (`shouldBe` Right True)
  reject (deletedBins "[0,1,0,0,0,0,0,0,0,0]") "deletedRecordCountsHistogramOpt mismatch"
  reject (deletedBins "[1]") "deletedRecordCountsHistogramOpt bin length mismatch"
  reject (payload "1" "42" "{\"minReaderVersion\":2,\"minWriterVersion\":5}" metadata) "protocol mismatch"
  reject (payload "1" "42" protocol (BL.init metadata <> ",\"name\":\"different\"}")) "metadata mismatch"
  reject ("{\"numFiles\":1," <> BL.drop 1 correct) "duplicate JSON key"
  reject (BL.init correct <> ",\"txnId\":\"wrong\"}") "txnId mismatch"
  reject (files (add "different.parquet" "42")) "allFiles contains a file absent from the snapshot"
  reject (files (add "live.parquet" "7")) "allFiles descriptor disagrees"
  reject (files (add "live.parquet" "42" <> "," <> add "live.parquet" "42")) "allFiles contains a duplicate path"
  reject (files (add "live.parquet" "42" <> "," <> add "%6Cive.parquet" "42")) "allFiles contains a duplicate path"
  reject (files (add "%256Cive.parquet" "42")) "allFiles contains a file absent from the snapshot"
  reject (files "") "allFiles is missing live files"
  reject (files "{}") "path"
  reject (files (statsFile "{\\\"numRecords\\\":2,\\\"maxValues\\\":{\\\"id\\\":0}}")) "allFiles statistics disagree"
  BL.writeFile file (files (statsFile "{ \\\"maxValues\\\":{\\\"id\\\":7}, \\\"numRecords\\\":2, \\\"tightBounds\\\":true }"))
  DIO.validateVersionChecksum table >>= (`shouldBe` Right True)
  BL.writeFile file (files (add "live.parquet" "42"))
  DIO.validateVersionChecksum table >>= (`shouldBe` Right True)
  BL.writeFile file (files (add "%6cive.parquet" "42"))
  DIO.validateVersionChecksum table >>= (`shouldBe` Right True)
  BL.appendFile (logDir </> "00000000000000000002.json") ("{\"add\":" <> add "%6cive.parquet" "42" <> "}\n")
  let aliasChecksum = logDir </> "00000000000000000002.crc"
  BL.writeFile aliasChecksum (files (add "live.parquet" "42"))
  renamed <- DIO.openDeltaTableAt root 2 >>= either fail pure
  DIO.activeFilePaths renamed `shouldBe` ["%6cive.parquet"]
  DIO.validateVersionChecksum renamed >>= (`shouldBe` Right True)
  BL.writeFile file (BL.init correct <> ",\"txnId\":\"original\"}")
  DIO.validateVersionChecksum table >>= (`shouldBe` Right True)
  reject "{}" "numFiles"
  BL.writeFile file correct
  older <- DIO.openDeltaTableAt root 0 >>= either fail pure
  DIO.validateVersionChecksum older >>= (`shouldBe` Right False)


checksumMetadataStateTest :: IO ()
checksumMetadataStateTest = withCorruptCheckpoint $ \root logDir -> do
  let commit = logDir </> "00000000000000000001.json"
      checksum = logDir </> "00000000000000000001.crc"
      protocol = "{\"minReaderVersion\":1,\"minWriterVersion\":7,\"writerFeatures\":[\"domainMetadata\"]}"
      metadata props = "{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\",\"configuration\":" <> props <> "}"
      transaction app version = "{\"appId\":\"" <> app <> "\",\"version\":" <> version <> "}"
      domain name value removed = "{\"domain\":\"" <> name <> "\",\"configuration\":\"" <> value <> "\",\"removed\":" <> removed <> "}"
      liveDomain = domain "live" "settings" "false"
      open props = do
        BL.writeFile commit $ "{\"protocol\":" <> protocol <> "}\n{\"metaData\":" <> metadata props <> "}\n"
          <> "{\"txn\":" <> transaction "beta" "-1" <> "}\n{\"txn\":" <> transaction "alpha" "4" <> "}\n"
          <> "{\"domainMetadata\":" <> liveDomain <> "}\n{\"domainMetadata\":" <> domain "gone" "old" "true" <> "}\n"
        DIO.openDeltaTable root >>= either fail pure
      write props txns domains = BL.writeFile checksum $
        "{\"numFiles\":1,\"tableSizeBytes\":42,\"numProtocol\":1,\"numMetadata\":1,\"protocol\":" <> protocol
          <> ",\"metadata\":" <> metadata props <> ",\"setTransactions\":" <> txns <> ",\"domainMetadata\":" <> domains <> "}"
      validTransactions = "[" <> transaction "alpha" "4" <> "," <> transaction "beta" "-1" <> "]"
      validDomains = "[" <> liveDomain <> "]"
      checked table = DIO.validateVersionChecksum table >>= (`shouldBe` Right True)
      reject table problem = DIO.validateVersionChecksum table >>= \case
        Left err -> err `shouldSatisfy` isInfixOf problem
        Right _ -> expectationFailure ("checksum accepted " ++ problem)
  table <- open "{}"
  write "{}" validTransactions validDomains
  checked table
  write "{}" "[]" validDomains
  reject table "setTransactions mismatch"
  write "{}" ("[" <> transaction "alpha" "4" <> "," <> transaction "alpha" "4" <> "]") validDomains
  reject table "duplicate appId"
  write "{}" validTransactions "[]"
  reject table "domainMetadata mismatch"
  write "{}" validTransactions ("[" <> liveDomain <> "," <> liveDomain <> "]")
  reject table "duplicate domain"
  write "{}" validTransactions ("[" <> domain "gone" "old" "true" <> "]")
  reject table "domainMetadata contains a tombstone"
  let retention = "{\"delta.setTransactionRetentionDuration\":\"interval 1 day\"}"
  retained <- open retention
  write retention ("[" <> transaction "alpha" "4" <> "," <> transaction "expired" "1" <> "]") validDomains
  checked retained
  write retention ("[" <> transaction "alpha" "3" <> "]") validDomains
  reject retained "setTransactions mismatch"


changeFeedChecksumTotalsTest :: IO ()
changeFeedChecksumTotalsTest = withCorruptCheckpoint $ \root logDir -> do
  let first = logDir </> "00000000000000000001.json"
      second = logDir </> "00000000000000000002.json"
      third = logDir </> "00000000000000000003.json"
      writeInfo path txn rest = BL.writeFile path ("{\"commitInfo\":{\"txnId\":\"" <> txn <> "\"}}\n" <> rest)
      add path size = "{\"add\":{\"path\":\"" <> path <> "\",\"size\":" <> size <> ",\"modificationTime\":0,\"dataChange\":true}}\n"
      payload txn bytes = "{\"txnId\":\"" <> txn <> "\",\"numFiles\":2,\"tableSizeBytes\":" <> bytes
        <> ",\"numProtocol\":1,\"numMetadata\":1,\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2},\"metadata\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}"
  writeInfo first "first" (add "a.parquet" "10")
  BL.writeFile (logDir </> "00000000000000000001.crc") (payload "first" "52")
  writeInfo second "second" ("{\"remove\":{\"path\":\"live.parquet\",\"dataChange\":true}}\n" <> add "b.parquet" "20")
  writeInfo third "third" (add "b.parquet" "25")
  BL.writeFile (logDir </> "00000000000000000003.crc") (payload "third" "35")
  let consume = DIO.streamDeltaCommits root 1 3 >>= either fail iterIOToList
  commits <- consume >>= either fail pure
  let hasTotals bytes entry = D.cachedFileMetrics (DIO.dcAfter entry) == Just (D.SnapshotFileMetrics bytes 0 0 0 0)
  zipWith hasTotals [52, 30, 35] commits `shouldBe` [True, True, True]
  BL.writeFile (logDir </> "00000000000000000003.crc") (payload "third" "36")
  consume >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "replayed file metrics disagree with the version checksum"
    Right _ -> expectationFailure "accepted incomplete change events"


changeDataFeedTest :: IO ()
changeDataFeedTest = withSystemTempDirectory "wireform-delta-cdf" $ \root -> do
  let logDir = root </> "_delta_log"
      commit v = logDir </> (replicate (20 - length v) '0' ++ v ++ ".json")
      write v time body = BL.writeFile (commit v) ("{\"commitInfo\":{\"inCommitTimestamp\":" <> time <> "}}\n" <> body)
      add name change extra = "{\"add\":{\"path\":\"" <> name <> "\",\"size\":1,\"modificationTime\":0,\"dataChange\":" <> change <> extra <> "}}\n"
      remove name change extra = "{\"remove\":{\"path\":\"" <> name <> "\",\"dataChange\":" <> change <> extra <> "}}\n"
      readChanges start end = do
        (schema, stream) <- Scan.scanChangeDataFeed root start (Just end) >>= either fail pure
        V.toList (AT.fieldName <$> AT.arrowFields schema) `shouldBe` ["id", "_change_type", "_commit_version", "_commit_timestamp"]
        result <- iterIOToList stream >>= either fail pure
        concat <$> traverse rows result
      rows batch = case V.toList (Scan.batchColumns batch) of
        [AC.ColInt64 _ ids, labels, AC.ColInt64 Nothing versions, AC.ColTimestamp Nothing times] -> do
          changes <- traverse (maybe (fail "missing CDF label") pure . AC.anyTextAt labels) [0 .. Scan.batchRowCount batch - 1]
          pure (zip4 (VS.toList ids) changes (VS.toList versions) (VS.toList times))
        _ -> fail "unexpected CDF row layout"
      plainBytes values = PN.buildNestedFile (V.singleton ("id", PN.NSRequired (PN.NSPrimitive PN.LtInt64)))
        (V.singleton (V.fromList (map (PN.NRLeaf . PN.LvInt64) values)))
  createDirectory logDir
  createDirectory (root </> "_change_data")
  write "0" "100" $
    "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"deletionVectors\"],\"writerFeatures\":[\"changeDataFeed\",\"deletionVectors\",\"inCommitTimestamp\"]}}\n"
      <> "{\"metaData\":{\"id\":\"test\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[{\\\"name\\\":\\\"id\\\",\\\"type\\\":\\\"long\\\",\\\"nullable\\\":true,\\\"metadata\\\":{}}]}\",\"configuration\":{\"delta.enableChangeDataFeed\":\"true\",\"delta.enableInCommitTimestamps\":\"true\"}}}\n"
  let descriptor = ",\"deletionVector\":{\"storageType\":\"i\",\"pathOrInlineDv\":\"wi5b=000010000siXQKl0rr91000f55c8Xg0@@D72lkbi5=-{L\",\"sizeInBytes\":40,\"cardinality\":6}"
      stats = ",\"stats\":\"{\\\"numRecords\\\":35}\""
      deleted = [3, 4, 7, 11, 18, 29]
  BS.writeFile (root </> "original.parquet") =<< either fail pure (plainBytes [0 .. 34])
  write "1" "101" (add "original.parquet" "true" stats)
  write "2" "102" (remove "original.parquet" "true" "" <> add "original.parquet" "true" (stats <> descriptor))
  write "3" "103" (remove "original.parquet" "true" descriptor <> add "original.parquet" "true" stats)
  changes <- readChanges 2 3
  changes `shouldBe` map (\i -> (i, "delete", 2, 102000)) deleted ++ map (\i -> (i, "insert", 3, 103000)) deleted
  let physicalFields = V.fromList
        [ ("id", PN.NSRequired (PN.NSPrimitive PN.LtInt64))
        , ("_change_type", PN.NSRequired (PN.NSPrimitive PN.LtString))
        ]
      columns = V.fromList
        [ V.fromList (map (PN.NRLeaf . PN.LvInt64) [20, 120])
        , V.fromList (map (PN.NRLeaf . PN.LvString) ["update_preimage", "update_postimage"])
        ]
  BS.writeFile (root </> "_change_data" </> "changes.parquet") =<< either fail pure (PN.buildNestedFile physicalFields columns)
  write "4" "104" (remove "original.parquet" "true" "" <> add "unread.parquet" "true" ""
    <> "{\"cdc\":{\"path\":\"_change_data/changes.parquet\",\"size\":1,\"partitionValues\":{},\"dataChange\":false}}\n")
  readChanges 4 4 >>= (`shouldBe` [(20, "update_preimage", 4, 104000), (120, "update_postimage", 4, 104000)])
  write "5" "105" (remove "unread.parquet" "false" "" <> add "also-unread.parquet" "false" "")
  readChanges 5 5 >>= (`shouldBe` [])


changeFeedSchemaTest :: IO ()
changeFeedSchemaTest = withSystemTempDirectory "wireform-delta-cdf-schema" $ \root -> do
  let logDir = root </> "_delta_log"
      commit ver = logDir </> ("0000000000000000000" ++ show (ver :: Int) ++ ".json")
      write ver = BL.writeFile (commit ver) . BLC.unlines
      protocol = "{\"protocol\":{\"minReaderVersion\":2,\"minWriterVersion\":5}}"
      metadata fields = Aeson.encode (Aeson.object ["metaData" Aeson..= Aeson.object
        [ "id" Aeson..= ("cdf-schema" :: T.Text)
        , "schemaString" Aeson..= TE.decodeUtf8 (BL.toStrict (Aeson.encode (struct fields)))
        , "configuration" Aeson..= Map.fromList [("delta.enableChangeDataFeed" :: T.Text, "true" :: T.Text), ("delta.columnMapping.mode", "name")]
        ]])
      struct fields = Aeson.object ["type" Aeson..= ("struct" :: T.Text), "fields" Aeson..= fields]
      field name physical ident kind nullable = Aeson.object
        [ "name" Aeson..= (name :: T.Text), "type" Aeson..= (kind :: Aeson.Value), "nullable" Aeson..= (nullable :: Bool)
        , "metadata" Aeson..= Aeson.object ["delta.columnMapping.id" Aeson..= (ident :: Int), "delta.columnMapping.physicalName" Aeson..= (physical :: T.Text)]
        ]
      fieldId' = field "id" "key" 1 "long" True
      label = field "label" "physical_label" 2 "string" True
      renamedLabel = field "name" "physical_label" 2 "string" True
      record child = field "record" "payload" 3 (struct [child]) True
      nested original = field original "physical_value" 4 "long" True
      add filename = "{\"add\":{\"path\":\"" <> filename <> "\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}"
      remove = "{\"remove\":{\"path\":\"first.parquet\",\"dataChange\":true}}"
      readFeed start end = do
        loaded <- Scan.scanChangeDataFeed root start (Just end)
        case loaded of
          Left err -> pure (Left err)
          Right (schema, stream) -> fmap (fmap (schema,)) (iterIOToList stream)
      rejects start end = readFeed start end >>= \case
        Left err -> err `shouldSatisfy` isInfixOf "non-additive schema change"
        Right _ -> expectationFailure "CDF accepted an incompatible mapped schema"
  createDirectory logDir
  bytes <- either fail pure (PN.buildNestedFile
    (V.singleton ("key", PN.NSRequired (PN.NSPrimitive PN.LtInt64)))
    (V.singleton (V.singleton (PN.NRLeaf (PN.LvInt64 7)))))
  BS.writeFile (root </> "first.parquet") bytes
  write 0 [protocol, metadata [fieldId'], add "first.parquet"]
  write 1 [metadata [fieldId', label, record (nested "original")]]
  (additiveSchema, [batch]) <- readFeed 0 1 >>= either fail pure
  V.toList (AT.fieldName <$> AT.arrowFields additiveSchema) `shouldBe` ["id", "label", "record", "_change_type", "_commit_version", "_commit_timestamp"]
  AC.nullCount (Scan.batchColumns batch V.! 1) `shouldBe` 1
  AC.nullCount (Scan.batchColumns batch V.! 2) `shouldBe` 1
  forM_
    [ [fieldId', renamedLabel, record (nested "original")]
    , [fieldId', record (nested "original")]
    , [field "id" "key" 1 "integer" True, label, record (nested "original")]
    , [field "id" "key" 1 "long" False, label, record (nested "original")]
    , [fieldId', label, record (nested "replacement")]
    ] $ \changed -> do
      write 2 [metadata changed]
      rejects 1 2
      (_, batches) <- readFeed 2 2 >>= either fail pure
      length batches `shouldBe` 0
  write 2 [metadata [fieldId', renamedLabel, record (nested "original")], remove]
  rejects 2 2
  BS.writeFile (root </> "second.parquet") bytes
  write 2 [metadata [fieldId', renamedLabel, record (nested "original")], add "second.parquet"]
  (renamedSchema, [renamedBatch]) <- readFeed 2 2 >>= either fail pure
  (AT.fieldName (AT.arrowFields renamedSchema V.! 1), Scan.batchRowCount renamedBatch) `shouldBe` ("name", 1)
  write 2 [metadata [fieldId', renamedLabel, record (nested "original")]]
  write 3 [metadata [fieldId', label, record (nested "original")]]
  rejects 0 3


checkpointActionInCommitTest :: IO ()
checkpointActionInCommitTest = withCorruptCheckpoint $ \root logDir -> do
  BL.appendFile (logDir </> "00000000000000000001.json") "{\"checkpointMetadata\":{\"version\":1}}\n"
  checkReplayError root "checkpoint actions are invalid in a JSON commit"


checkReplayError :: FilePath -> String -> IO ()
checkReplayError root problem = do
  let check result = case result of
        Left err -> do
          err `shouldSatisfy` isInfixOf "00000000000000000001.checkpoint.parquet"
          err `shouldSatisfy` isInfixOf problem
        Right _ -> expectationFailure "expected the incomplete replay to fail"
  DIO.openDeltaTable root >>= check
  DIO.openDeltaTableAt root 1 >>= check


snapshotCommitInfoTest :: IO ()
snapshotCommitInfoTest = withCorruptCheckpoint $ \root logDir -> do
  BL.appendFile (logDir </> "00000000000000000000.json") "{\"commitInfo\":{\"operation\":\"CREATE TABLE\"}}\n"
  latest <- DIO.openDeltaTable root >>= either fail pure
  initial <- DIO.openDeltaTableAt root 0 >>= either fail pure
  D.tsLastCommit (DIO.dtSnapshot latest) `shouldBe` Nothing
  (D.ciOperation =<< D.tsLastCommit (DIO.dtSnapshot initial)) `shouldBe` Just "CREATE TABLE"


missingSnapshotProvenanceTest :: IO ()
missingSnapshotProvenanceTest =
  withSystemTempDirectory "wireform-delta-missing-provenance" $ \root -> do
    let logDir = root </> "_delta_log"
        first = "00000000000000000000"
        second = "00000000000000000001"
        target = logDir </> second ++ ".json"
        checkpoint = logDir </> second ++ ".checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json"
        compacted = logDir </> first ++ "." ++ second ++ ".compacted.json"
        missing = logDir </> "deleted-commit"
        invalidNewest = logDir </> "00000000000000000001.checkpoint.parquet"
        protocol = "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\
          \\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\"]}}\n"
        metadata = "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
        add = "{\"add\":{\"path\":\"covered.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
        check opener = do
          table <- opener >>= either fail pure
          DIO.dtVersion table `shouldBe` Just 1
          DIO.activeFilePaths table `shouldBe` ["covered.parquet"]
          D.tsLastCommit (DIO.dtSnapshot table) `shouldBe` Nothing
    createDirectory logDir
    -- A dangling link leaves the name visible in a directory listing but
    -- makes the subsequent read fail just as if cleanup removed the file.
    createFileLink missing target
    BL.writeFile invalidNewest "incomplete classic checkpoint"
    BL.writeFile checkpoint ("{\"checkpointMetadata\":{\"version\":1}}\n" <> protocol <> metadata <> add)
    check (DIO.openDeltaTable root)
    removeFile target
    createFileLink target target
    check (DIO.openDeltaTable root)
    removeFile checkpoint
    removeFile invalidNewest
    BL.writeFile compacted ("{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2}}\n" <> metadata <> add)
    check (DIO.openDeltaTableAt root 1)
    removeFile compacted
    DIO.openDeltaTableAt root 1 >>= \case
      Left _ -> pure ()
      Right _ -> expectationFailure "snapshot opened without a source of file actions"


historyTimestampTest :: IO ()
historyTimestampTest = withCorruptCheckpoint $ \root logDir -> do
  let commit0 = logDir </> "00000000000000000000.json"
      commit1 = logDir </> "00000000000000000001.json"
  BL.appendFile commit0 "{\"commitInfo\":{\"timestamp\":8}}\n"
  setModificationTime commit0 (posixSecondsToUTCTime 12)
  BL.writeFile commit1 $
    "{\"commitInfo\":{\"timestamp\":10,\"inCommitTimestamp\":12001}}\n"
      <> "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":7,\"writerFeatures\":[\"inCommitTimestamp\"]}}\n"
      <> "{\"metaData\":{\"id\":\"test\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\",\"configuration\":{\"delta.enableInCommitTimestamps\":\"true\",\"delta.inCommitTimestampEnablementVersion\":\"1\",\"delta.inCommitTimestampEnablementTimestamp\":\"12001\"}}}\n"
  setModificationTime commit1 (posixSecondsToUTCTime 99)
  table <- DIO.openDeltaTable root >>= either fail pure
  entries <- DIO.historyEntries table
  map DIO.heTimestamp entries `shouldBe` [Just 12001, Just 12000]


timeTravelTimestampTest :: IO ()
timeTravelTimestampTest = withCorruptCheckpoint $ \root logDir -> do
  let commit0 = logDir </> "00000000000000000000.json"
      commit1 = logDir </> "00000000000000000001.json"
      commit2 = logDir </> "00000000000000000002.json"
      commit3 = logDir </> "00000000000000000003.json"
      readAt millis = DIO.openDeltaTableAtTimestamp root (posixSecondsToUTCTime (millis / 1000))
      matches (millis, version, paths) = do
        table <- readAt millis >>= either fail pure
        DIO.dtVersion table `shouldBe` Just version
        DIO.activeFilePaths table `shouldBe` paths
      rejects millis problem = readAt millis >>= \case
        Left err -> err `shouldSatisfy` isInfixOf problem
        Right _ -> expectationFailure "opened a snapshot without enough timestamp evidence"
      file name = "{\"add\":{\"path\":\"" <> name <> "\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
      info time = "{\"commitInfo\":{\"inCommitTimestamp\":" <> BLC.pack (show (time :: Int)) <> "}}\n"
  BL.appendFile commit1 (file "second.parquet")
  setModificationTime commit0 (posixSecondsToUTCTime 12)
  setModificationTime commit1 (posixSecondsToUTCTime 11)
  BL.writeFile commit2 $
    info 13000
      <> "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":7,\"writerFeatures\":[\"inCommitTimestamp\"]}}\n"
      <> "{\"metaData\":{\"id\":\"test\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\",\"configuration\":{\"delta.enableInCommitTimestamps\":\"true\",\"delta.inCommitTimestampEnablementVersion\":\"2\",\"delta.inCommitTimestampEnablementTimestamp\":\"13000\"}}}\n"
      <> file "third.parquet"
  BL.writeFile commit3 (info 13100 <> file "fourth.parquet")
  setModificationTime commit2 (posixSecondsToUTCTime 99)
  setModificationTime commit3 (posixSecondsToUTCTime 99)
  let first = ["live.parquet"]
      second = first ++ ["second.parquet"]
      third = second ++ ["third.parquet"]
  mapM_ matches
    [ (12000, 0, first)
    , (12000.9, 0, first)
    , (12001, 1, second)
    , (12999, 1, second)
    , (13000, 2, third)
    , (13099, 2, third)
    , (13100, 3, ["fourth.parquet"] ++ third)
    ]
  rejects 11999 "before the available commit history"
  rejects 13101 "after the latest commit"
  let commit4 = logDir </> "00000000000000000004.json"
      commit5 = logDir </> "00000000000000000005.json"
      commit6 = logDir </> "00000000000000000006.json"
      fourth = "fourth.parquet" : third
  BL.writeFile commit4 (info 13200)
  BL.writeFile commit5 (info 13300 <> file "fifth.parquet")
  BL.writeFile commit6 (info 13400)
  matches (13199, 3, fourth)
  matches (13299, 4, fourth)
  matches (13300, 5, "fifth.parquet" : fourth)
  BL.appendFile commit6
    "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"unknownFutureFeature\"],\"writerFeatures\":[\"unknownFutureFeature\",\"inCommitTimestamp\"]}}\n"
  rejects 13400 "unsupported snapshot reader feature"
  matches (13300, 5, "fifth.parquet" : fourth)
  BL.writeFile commit6 $ info 13400
    <> "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"catalogManaged\"],\"writerFeatures\":[\"catalogManaged\",\"inCommitTimestamp\"]}}\n"
  rejects 13300 "require a catalog to resolve committed versions"
  mapM_ removeFile [commit4, commit5, commit6]
  BL.writeFile commit3 (file "fourth.parquet")
  rejects 13099 "missing inCommitTimestamp"
  BL.writeFile commit3 (info 12999 <> file "fourth.parquet")
  rejects 13099 "non-increasing inCommitTimestamp"
  BL.writeFile commit3 (info 13100 <> file "fourth.parquet")
  createDirectory (logDir </> "00000000000000000004.json")
  matches (13100, 3, ["fourth.parquet"] ++ third)
  BL.writeFile (logDir </> "00000000000000000002.checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json") $
    "{\"checkpointMetadata\":{\"version\":2}}\n"
      <> "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\",\"inCommitTimestamp\"]}}\n"
      <> "{\"metaData\":{\"id\":\"test\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\",\"configuration\":{\"delta.enableInCommitTimestamps\":\"true\",\"delta.inCommitTimestampEnablementVersion\":\"2\",\"delta.inCommitTimestampEnablementTimestamp\":\"13000\"}}}\n"
      <> file "live.parquet" <> file "second.parquet" <> file "third.parquet"
  removeFile commit0
  removeFile commit1
  rejects 12999 "before the available commit history"
  matches (13000, 2, third)
  matches (13050, 2, third)


legacyHistoryTimestampTest :: IO ()
legacyHistoryTimestampTest = withCorruptCheckpoint $ \root logDir -> do
  let commit0 = logDir </> "00000000000000000000.json"
      commit1 = logDir </> "00000000000000000001.json"
  BL.appendFile commit0 "{\"commitInfo\":{\"timestamp\":8}}\n"
  BL.writeFile commit1 $
    "{\"commitInfo\":{\"timestamp\":10,\"inCommitTimestamp\":99999}}\n"
      <> "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":7,\"writerFeatures\":[\"inCommitTimestamp\"]}}\n"
      <> "{\"metaData\":{\"id\":\"test\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\",\"configuration\":{\"delta.enableInCommitTimestamps\":\"false\"}}}\n"
  setModificationTime commit0 (posixSecondsToUTCTime 12)
  setModificationTime commit1 (posixSecondsToUTCTime 11)
  table <- DIO.openDeltaTable root >>= either fail pure
  entries <- DIO.historyEntries table
  map DIO.heTimestamp entries `shouldBe` [Just 12001, Just 12000]


historyCleanupRaceTest :: IO ()
historyCleanupRaceTest = withCorruptCheckpoint $ \root logDir -> do
  let commit0 = logDir </> "00000000000000000000.json"
      commit1 = logDir </> "00000000000000000001.json"
      commit2 = logDir </> "00000000000000000002.json"
  BL.writeFile commit1 "{\"commitInfo\":{\"operation\":\"WRITE\"}}\n"
  BL.writeFile commit2 "{\"commitInfo\":{\"operation\":\"OPTIMIZE\"}}\n"
  setModificationTime commit0 (posixSecondsToUTCTime 12)
  setModificationTime commit1 (posixSecondsToUTCTime 99)
  setModificationTime commit2 (posixSecondsToUTCTime 11)
  table <- DIO.openDeltaTable root >>= either fail pure
  removeFile commit1
  entries <- DIO.historyEntries table
  map DIO.heVersion entries `shouldBe` [2, 0]
  map DIO.heTimestamp entries `shouldBe` [Just 12001, Just 12000]
  map DIO.heOperation entries `shouldBe` [Just "OPTIMIZE", Nothing]


commitInfoProjectionTest :: IO ()
commitInfoProjectionTest = do
  let logBytes = "{\"add\":{\"path\":\"commitInfo\"}}\r\n \t{ \"commit\\u0049nfo\" : {\"operation\":\"WRITE\"}}\n{\"tx\\u006e\":{\"appId\":\"client\",\"version\":7}}\n{\"txn\":{\"appId\":\"later\",\"version\":8}}"
      parsed = D.parseCommitHistory logBytes
  (D.ciOperation =<< D.parseCommitInfo logBytes) `shouldBe` Just "WRITE"
  D.historyCommitInfo parsed `shouldBe` D.parseCommitInfo logBytes
  D.historyFirstTransaction parsed `shouldBe` Just (D.TxnAction "client" 7 Nothing)


catalogManagedTest :: IO ()
catalogManagedTest = withCorruptCheckpoint $ \root logDir -> do
  BL.appendFile (logDir </> "00000000000000000001.json") $
    "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"catalogManaged\"],\"writerFeatures\":[\"catalogManaged\"]}}\n"
  result <- DIO.openDeltaTable root
  case result of
    Left err -> err `shouldSatisfy` isInfixOf "catalog-managed"
    Right _ -> expectationFailure "expected opening the catalog-managed log to fail"


readerFeaturesTest :: IO ()
readerFeaturesTest = withCorruptCheckpoint $ \root logDir -> do
  let append feature =
        BL.appendFile (logDir </> "00000000000000000001.json") $
          "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\""
            <> feature
            <> "\"],\"writerFeatures\":[\""
            <> feature
            <> "\"]}}\n"
  append "deletionVectors"
  table <- DIO.openDeltaTable root >>= either fail pure
  fmap D.pReaderFeatures (D.tsProtocol (DIO.dtSnapshot table)) `shouldBe` Just ["deletionVectors"]
  BL.writeFile (logDir </> "00000000000000000001.json") "{\"txn\":{\"appId\":\"client\",\"version\":7}}\n"
  append "futureReaderFeature"
  result <- DIO.openDeltaTable root
  case result of
    Left err -> err `shouldSatisfy` isInfixOf "unsupported snapshot reader feature futureReaderFeature"
    Right _ -> expectationFailure "expected an unsupported snapshot feature error"


snapshotConfigurationTest :: IO ()
snapshotConfigurationTest =
  withSystemTempDirectory "wireform-delta-snapshot-config" $ \root -> do
    let logDir = root </> "_delta_log"
        initial = logDir </> "00000000000000000000.json"
        protocol features = "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":"
          <> Aeson.encode (features :: [T.Text]) <> ",\"writerFeatures\":" <> Aeson.encode features <> "}}\n"
        metadata config = "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[] }\",\"configuration\":"
          <> Aeson.encode (config :: Map.Map T.Text T.Text) <> "}}\n"
        write features config = BL.writeFile initial (protocol features <> metadata config)
        expectFailure message = DIO.openDeltaTable root >>= \case
          Left err -> err `shouldSatisfy` isInfixOf message
          Right _ -> expectationFailure "opened a table with invalid reader configuration"
        deletionVectors = Map.singleton "delta.enableDeletionVectors" "true"
    createDirectory logDir
    write ["deletionVectors"] (Map.insert "delta.columnMapping.mode" "bogus" deletionVectors)
    expectFailure "unsupported column mapping mode bogus"
    write [] deletionVectors
    expectFailure "delta.enableDeletionVectors requires"
    write ["deletionVectors"] deletionVectors
    table <- DIO.openDeltaTable root >>= either fail pure
    DIO.dtVersion table `shouldBe` Just 0


scanReaderRequirementsTest :: IO ()
scanReaderRequirementsTest = do
  let schema = "{\"type\":\"struct\",\"fields\":[]}"
      metadata = D.MetaDataAction "table" Nothing Nothing Nothing schema [] mempty Nothing
      table value = DIO.DeltaTable "" Nothing Nothing [] snapshot (Just 0)
        where snapshot = D.emptySnapshot {D.tsMetaData = Just metadata, D.tsProtocol = Just value}
      protocol feature = D.ProtocolAction 3 7 [feature] [feature]
      reject message = either (\err -> err `shouldSatisfy` isInfixOf message)
        (const (expectationFailure "reader requirement was bypassed"))
      check message candidate = do
        reject message (Scan.scanTable candidate)
        reject message (Scan.scanTrackedTable candidate)
        reject message (Scan.tableFileStats candidate)
  check "unsupported snapshot reader feature futureReaderFeature" (table (protocol "futureReaderFeature"))
  check "catalog-managed tables require a catalog" (table (protocol "catalogManaged"))
  check "unsupported reader protocol version 4" (table (D.ProtocolAction 4 7 [] []))
  _ <- either fail pure (Scan.scanTable (table (protocol "deletionVectors")))
  _ <- either fail pure (Scan.scanTable (table (D.ProtocolAction 3 7 [] ["futureWriterFeature"])))
  pure ()


metadataFormatTest :: IO ()
metadataFormatTest = do
  let schema = "{\"type\":\"struct\",\"fields\":[]}" :: T.Text
      metadata format =
        "{\"metaData\":{\"id\":\"table\",\"schemaString\":" <> Aeson.encode schema <> format <> "}}"
      parse format = case D.parseLogFileChecked (metadata format) of
        Right [D.ActionMetaData value] -> pure value
        result -> fail (show result)
      reject format = case D.parseLogFileChecked (metadata format) of
        Left err -> err `shouldSatisfy` isInfixOf "format"
        Right _ -> expectationFailure "accepted malformed metadata format"
  missing <- parse ""
  D.mdFormat missing `shouldBe` Nothing
  providerOnly <- parse ",\"format\":{\"provider\":\"orc\"}"
  D.mdFormat providerOnly `shouldBe` Just ("orc", Map.empty)
  let snapshot = D.emptySnapshot {D.tsMetaData = Just providerOnly}
      table = DIO.DeltaTable "" Nothing Nothing [] snapshot (Just 0)
  case Scan.scanTable table of
    Left err -> err `shouldSatisfy` isInfixOf "table data is not Parquet"
    Right _ -> expectationFailure "accepted ORC as Parquet"
  valid <- parse ",\"format\":{\"provider\":\"parquet\",\"options\":{\"compression\":\"zstd\"}}"
  D.mdFormat valid `shouldBe` Just ("parquet", Map.singleton "compression" "zstd")
  mapM_ (reject . (",\"format\":" <>))
    [ "{}"
    , "null"
    , "\"parquet\""
    , "{\"provider\":null}"
    , "{\"provider\":\"parquet\",\"options\":null}"
    , "{\"provider\":\"parquet\",\"options\":[]}"
    , "{\"provider\":\"parquet\",\"options\":{\"compression\":1}}"
    ]


checkpointMetadataFormatTest :: IO ()
checkpointMetadataFormatTest = do
  let string = PN.NSPrimitive PN.LtString
      required = PN.NSRequired string
      options = PN.NSMap required (PN.NSOptional string)
      record = PN.NSStruct . V.fromList
      row = PN.NRStruct . V.fromList
      str = PN.NRLeaf . PN.LvString
      entries = PN.NRMapEntries . V.fromList
      fixture extra values = do
        let fields = V.singleton ("metaData", record ([("id", required), ("schemaString", required)] ++ extra))
            metadata = row ([str "table", str "{\"type\":\"struct\",\"fields\":[]}"] ++ values)
        either fail pure (PN.buildNestedFile fields (V.singleton (V.singleton metadata)))
      decode extra values = Checkpoint.decodeCheckpointFile <$> fixture extra values
      formatFields = [("format", PN.NSOptional (record [("provider", PN.NSOptional string), ("options", options)]))]
      expectFormat extra values expected =
        decode extra values >>= \case
          Right [D.ActionMetaData metadata] -> D.mdFormat metadata `shouldBe` expected
          result -> expectationFailure ("expected one metadata action, got " ++ show result)
      reject extra values field =
        decode extra values >>= \case
          Left err -> err `shouldSatisfy` isInfixOf field
          Right actions -> expectationFailure ("checkpoint accepted an invalid format: " ++ show actions)
  expectFormat [] [] Nothing
  expectFormat formatFields [PN.NRNull] Nothing
  expectFormat formatFields [row [str "orc", entries [(str "compression", str "zstd")]]]
    (Just ("orc", Map.singleton "compression" "zstd"))
  expectFormat formatFields [row [str "parquet", entries []]] (Just ("parquet", Map.empty))
  reject formatFields [row [PN.NRNull, entries []]] "provider"
  reject [("format", record [("options", options)])] [row [entries [(str "compression", str "zstd")]]] "provider"
  reject [("format", required)] [str "parquet"] "format"
  reject formatFields [row [str "parquet", entries [(str "compression", PN.NRNull)]]] "format.options"


checkpointMetadataConfigurationTest :: IO ()
checkpointMetadataConfigurationTest = do
  let text = PN.NSPrimitive PN.LtString
      string = PN.NRLeaf . PN.LvString
      schema = "{\"type\":\"struct\",\"fields\":[]}"
      property = "delta.columnMapping.mode"
      fields =
        V.singleton
          ( "metaData"
          , PN.NSStruct (V.fromList
              [ ("id", PN.NSRequired text)
              , ("schemaString", PN.NSRequired text)
              , ("configuration", PN.NSOptional (PN.NSMap (PN.NSRequired text) (PN.NSOptional text)))
              ])
          )
      checkpoint config = do
        let row = PN.NRStruct (V.fromList [string "table", string schema, config])
        bytes <- either fail pure (PN.buildNestedFile fields (V.singleton (V.singleton row)))
        pure (Checkpoint.decodeCheckpointFile bytes)
      json config = D.parseLogFileChecked $
        Aeson.encode (Aeson.object
          ["metaData" Aeson..= Aeson.object
            ["id" Aeson..= ("table" :: T.Text), "schemaString" Aeson..= schema, "configuration" Aeson..= config]])
      mapEntry value = PN.NRMapEntries (V.singleton (string property, value))
  checkpoint (mapEntry (string "name"))
    `shouldReturn` json (Map.singleton property ("name" :: T.Text))
  checkpoint (PN.NRMapEntries V.empty) `shouldReturn` json (Map.empty :: Map.Map T.Text T.Text)
  checkpoint PN.NRNull `shouldReturn` json Aeson.Null
  let reject = \case
        Left err -> err `shouldSatisfy` isInfixOf "configuration"
        Right actions -> expectationFailure ("accepted a null table configuration value: " ++ show actions)
  reject (json (Map.singleton property Aeson.Null))
  checkpoint (mapEntry PN.NRNull) >>= reject


protocolVersionRangeTest :: IO ()
protocolVersionRangeTest = do
  let protocol reader writer =
        D.parseLogFileChecked
          ("{\"protocol\":{\"minReaderVersion\":" <> reader <> ",\"minWriterVersion\":" <> writer <> "}}")
      reject field result = case result of
        Left err -> err `shouldSatisfy` isInfixOf field
        Right _ -> expectationFailure "accepted a malformed protocol version"
      leaf = PN.NSRequired (PN.NSPrimitive PN.LtInt32)
      schema = V.singleton ("protocol", PN.NSStruct (V.fromList [("minReaderVersion", leaf), ("minWriterVersion", leaf)]))
      inParquet reader writer = do
        let row = PN.NRStruct (V.fromList (map (PN.NRLeaf . PN.LvInt32) [reader, writer]))
        bytes <- either fail pure (PN.buildNestedFile schema (V.singleton (V.singleton row)))
        pure (Checkpoint.decodeCheckpointFile bytes)
  mapM_ (reject "minReaderVersion" . (`protocol` "2")) ["0", "-1", "2147483648"]
  mapM_ (reject "minWriterVersion" . protocol "1") ["0", "-1", "2147483648"]
  protocol "1" "2147483647" `shouldBe` Right [D.ActionProtocol (D.ProtocolAction 1 2147483647 [] [])]
  inParquet 0 2 >>= reject "minReaderVersion"
  inParquet 1 0 >>= reject "minWriterVersion"
  inParquet 1 2147483647 `shouldReturn` Right [D.ActionProtocol (D.ProtocolAction 1 2147483647 [] [])]


protocolFeatureListsTest :: IO ()
protocolFeatureListsTest = do
  let empty = "[]"
      bad = "[null]"
      jsonProtocol versions lists = D.parseLogFileChecked ("{\"protocol\":{" <> versions <> lists <> "}}\n")
      modern = "\"minReaderVersion\":3,\"minWriterVersion\":7"
      legacy = "\"minReaderVersion\":1,\"minWriterVersion\":2"
      writerOnly = "\"minReaderVersion\":1,\"minWriterVersion\":7"
      feature name value = ",\"" <> name <> "\":" <> value
      jsonReader = feature "readerFeatures"
      jsonWriter = feature "writerFeatures"
      checkError result = case result of
        Left err -> err `shouldSatisfy` isInfixOf "Features"
        Right _ -> expectationFailure "invalid protocol feature lists were accepted"
      optionalList = PN.NSOptional (PN.NSList (PN.NSOptional (PN.NSPrimitive PN.LtString)))
      intField = PN.NSRequired (PN.NSPrimitive PN.LtInt32)
      columnNames = [("readerFeatures", optionalList), ("writerFeatures", optionalList)]
      list = PN.NRList . V.fromList
      symbol = PN.NRLeaf . PN.LvString
      parquetProtocol reader writer features = do
        let schema = V.singleton ("protocol", PN.NSStruct (V.fromList
              ([("minReaderVersion", intField), ("minWriterVersion", intField)] ++ take (length features) columnNames)))
            row = PN.NRStruct (V.fromList (map (PN.NRLeaf . PN.LvInt32) [reader, writer] ++ features))
        bytes <- either fail pure (PN.buildNestedFile schema (V.singleton (V.singleton row)))
        pure (Checkpoint.decodeCheckpointFile bytes)
  jsonProtocol modern (jsonReader empty <> jsonWriter empty) `shouldBe` Right [D.ActionProtocol (D.ProtocolAction 3 7 [] [])]
  jsonProtocol modern (jsonReader "[\"timestampNtz\"]" <> jsonWriter "[\"timestampWithoutTimezone\"]")
    `shouldBe` Right [D.ActionProtocol (D.ProtocolAction 3 7 ["timestampNtz"] ["timestampWithoutTimezone"])]
  mapM_ checkError
    [ jsonProtocol modern (jsonWriter empty)
    , jsonProtocol modern (jsonReader empty)
    , jsonProtocol modern (jsonReader "null" <> jsonWriter empty)
    , jsonProtocol modern (jsonReader bad <> jsonWriter empty)
    , jsonProtocol legacy (jsonReader empty)
    , jsonProtocol legacy (jsonWriter empty)
    , jsonProtocol writerOnly ""
    , jsonProtocol modern (jsonReader "[\"deletionVectors\"]" <> jsonWriter empty)
    , jsonProtocol modern (jsonReader empty <> jsonWriter "[\"deletionVectors\"]")
    , jsonProtocol writerOnly (jsonWriter "[\"catalogManaged\"]")
    ]
  parquetProtocol 3 7 [list [], list []] `shouldReturn` Right [D.ActionProtocol (D.ProtocolAction 3 7 [] [])]
  parquetProtocol 1 7 [PN.NRNull, list [symbol "appendOnly"]]
    `shouldReturn` Right [D.ActionProtocol (D.ProtocolAction 1 7 [] ["appendOnly"])]
  parquetProtocol 1 2 [PN.NRNull, PN.NRNull] `shouldReturn` Right [D.ActionProtocol (D.ProtocolAction 1 2 [] [])]
  parquetProtocol 3 7 [] >>= checkError
  parquetProtocol 3 7 [PN.NRNull, list []] >>= checkError
  parquetProtocol 3 7 [list [], PN.NRNull] >>= checkError
  parquetProtocol 3 7 [list [PN.NRNull], list []] >>= checkError
  parquetProtocol 1 2 [list [], PN.NRNull] >>= checkError
  parquetProtocol 3 7 [list [symbol "deletionVectors"], list []] >>= checkError
  parquetProtocol 3 7 [list [], list [symbol "deletionVectors"]] >>= checkError


-- ============================================================
-- Action parser cases
-- ============================================================

addLineTest :: IO ()
addLineTest = case D.parseLogLine addJson of
  Just (D.ActionAdd a) -> do
    D.addPath a `shouldBe` "data/file.parquet"
    D.addSize a `shouldBe` 1024
    D.addModificationTime a `shouldBe` 1700000000000
    D.addDataChange a `shouldBe` True
    D.addStats a `shouldBe` Just "{\"numRecords\":42}"
    Map.lookup "p" (D.addPartitionValues a) `shouldBe` Just (Just "v")
  _ -> expectationFailure "expected ActionAdd"
  where
    addJson = "{\"add\":{\"path\":\"data/file.parquet\",\"size\":1024,\"modificationTime\":1700000000000,\"dataChange\":true,\"stats\":\"{\\\"numRecords\\\":42}\",\"partitionValues\":{\"p\":\"v\"}}}"


addReplayDecoderTest :: IO ()
addReplayDecoderTest = do
  mapM_
    check
    [ "{\"add\":{\"dataChange\":false,\"path\":\"p\\u0020x\",\"size\":9223372036854775807,\"modificationTime\":-9223372036854775808}}"
    , "{\"\\u0061dd\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"partitionValues\":{\"x\":\"\",\"y\":null,\"z\":\"ok\"},\"tags\":{\"x\":\"y\"},\"stats\":null}}"
    , "{\"add\":{\"path\":\"a\",\"size\":2.0,\"modificationTime\":-3e1,\"dataChange\":true}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"deletionVector\":{\"offset\":1},\"future\":[{\"extra\":[true,null]}]}}\r"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"stats\":\"{\\\"numRecords\\\":2}\",\"deletionVector\":null,\"tags\":null,\"partitionValues\":null}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"tags\":{}}}"
    , "{\"add\":{\"baseRowId\":9223372036854775807,\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"defaultRowCommitVersion\":3}}"
    , "{\"add\":{\"defaultRowCommitVersion\":null,\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"baseRowId\":-9223372036854775808}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"baseRowId\":2.0,\"defaultRowCommitVersion\":3e1}}"
    ]
  mapM_
    checkRejected
    [ "{\"add\":{\"path\":\"a\",\"size\":18446744073709551616,\"modificationTime\":0,\"dataChange\":true}}"
    , "{\"add\":{\"path\":\"first\",\"path\":\"second\",\"size\":1,\"modificationTime\":2,\"dataChange\":true}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"baseRowId\":null,\"baseRowId\":9,\"defaultRowCommitVersion\":3}}"
    , "{\"add\":{\"path\":\"a\",\"size\":-1,\"modificationTime\":0,\"dataChange\":true}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":9223372036854775808,\"dataChange\":true}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":null}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"tags\":{\"x\":1}}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"baseRowId\":9223372036854775808}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"defaultRowCommitVersion\":\"bad\"}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true,\"future\":[1,]}}"
    , "{\"add\":{\"path\":\"\255\",\"size\":0,\"modificationTime\":0,\"dataChange\":true}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true},\"txn\":{}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true},\"\\u0061dd\":{\"path\":\"b\",\"size\":0,\"modificationTime\":0,\"dataChange\":true}}"
    , "{\"future\":{},\"future\":{}}"
    , "{\"add\":{\"path\":\"a\",\"size\":0,\"modificationTime\":0,\"dataChange\":true}} garbage"
    ]
  where
    check line =
      case Aeson.eitherDecode (BLC.pack line) :: Either String (Map.Map String D.AddAction) of
        Left err -> expectationFailure err
        Right reference -> D.parseLogFileChecked (BLC.pack line) `shouldBe` Right (map D.ActionAdd (Map.elems reference))
    checkRejected line = case D.parseLogFileChecked ("\n" <> BLC.pack line) of
      Left err -> err `shouldSatisfy` isInfixOf "invalid Delta action at line 2"
      Right _ -> expectationFailure ("accepted malformed add action " ++ line)


removeLineTest :: IO ()
removeLineTest = case D.parseLogLine removeJson of
  Just (D.ActionRemove r) -> do
    D.removePath r `shouldBe` "data/old.parquet"
    D.removeDeletionTimestamp r `shouldBe` Just 1700000005000
  _ -> expectationFailure "expected ActionRemove"
  where
    removeJson = "{\"remove\":{\"path\":\"data/old.parquet\",\"deletionTimestamp\":1700000005000,\"dataChange\":false}}"


protocolLineTest :: IO ()
protocolLineTest = case D.parseLogLine protoJson of
  Just (D.ActionProtocol p) -> do
    D.pMinReaderVersion p `shouldBe` 3
    D.pMinWriterVersion p `shouldBe` 7
    D.pReaderFeatures p `shouldBe` ["deletionVectors"]
    D.pWriterFeatures p `shouldBe` ["deletionVectors", "columnMapping"]
  _ -> expectationFailure "expected ActionProtocol"
  where
    protoJson = "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"deletionVectors\"],\"writerFeatures\":[\"deletionVectors\",\"columnMapping\"]}}"


metaDataLineTest :: IO ()
metaDataLineTest = case D.parseLogLine mdJson of
  Just (D.ActionMetaData md) -> do
    D.mdId md `shouldBe` "abc-uuid"
    D.mdPartitionColumns md `shouldBe` ["p"]
    D.mdSchemaString md `shouldBe` "{\"type\":\"struct\",\"fields\":[]}"
  _ -> expectationFailure "expected ActionMetaData"
  where
    mdJson = "{\"metaData\":{\"id\":\"abc-uuid\",\"format\":{\"provider\":\"parquet\",\"options\":{}},\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\",\"partitionColumns\":[\"p\"],\"configuration\":{}}}"


commitInfoLineTest :: IO ()
commitInfoLineTest = case D.parseLogLine ciJson of
  Just (D.ActionCommitInfo ci) -> do
    D.ciTimestamp ci `shouldBe` Just 1700000000000
    D.ciOperation ci `shouldBe` Just "WRITE"
    D.ciIsBlindAppend ci `shouldBe` Just True
  _ -> expectationFailure "expected ActionCommitInfo"
  where
    ciJson = "{\"commitInfo\":{\"timestamp\":1700000000000,\"operation\":\"WRITE\",\"operationParameters\":{},\"isBlindAppend\":true}}"


freeFormCommitInfoTest :: IO ()
freeFormCommitInfoTest = withCorruptCheckpoint $ \root logDir -> do
  let provenance =
        "{\"commitInfo\":{\"timestamp\":\"2026-11-01\",\"inCommitTimestamp\":12001,"
          <> "\"operation\":\"WRITE\",\"operationParameters\":[1,2],\"isBlindAppend\":null,"
          <> "\"isolationLevel\":false,\"txnVersion\":-5,\"custom\":{\"a\":true}}}"
      bytes = BL.toStrict provenance
  BL.appendFile (logDir </> "00000000000000000001.json") (provenance <> "\n")
  table <- DIO.openDeltaTable root >>= either fail pure
  DIO.activeFilePaths table `shouldBe` ["live.parquet"]
  case D.tsLastCommit (DIO.dtSnapshot table) of
    Nothing -> expectationFailure "missing commit provenance"
    Just info -> do
      D.parseCommitInfo bytes `shouldBe` Just info
      D.ciOperation info `shouldBe` Just "WRITE"
      D.ciInCommitTimestamp info `shouldBe` Just 12001
      D.ciTimestamp info `shouldBe` Nothing
      D.ciExtra info `shouldBe` HM.fromList
        [ ("timestamp", Aeson.String "2026-11-01")
        , ("operationParameters", Aeson.toJSON ([1, 2] :: [Int]))
        , ("isolationLevel", Aeson.Bool False)
        , ("txnVersion", Aeson.Number (-5))
        , ("custom", Aeson.object ["a" Aeson..= True])
        ]
  BL.writeFile (logDir </> "00000000000000000001.checkpoint.aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee.json") $
    "{\"checkpointMetadata\":{\"version\":1}}\n"
      <> "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"v2Checkpoint\"],\"writerFeatures\":[\"v2Checkpoint\"]}}\n"
      <> "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
      <> "{\"add\":{\"path\":\"live.parquet\",\"size\":42,\"modificationTime\":0,\"dataChange\":true}}\n"
  removeFile (logDir </> "00000000000000000000.json")
  recovered <- DIO.openDeltaTable root >>= either fail pure
  DIO.activeFilePaths recovered `shouldBe` ["live.parquet"]
  D.tsLastCommit (DIO.dtSnapshot recovered) `shouldBe` D.tsLastCommit (DIO.dtSnapshot table)
  entries <- DIO.historyEntries recovered
  map DIO.heOperation entries `shouldBe` [Just "WRITE"]


txnLineTest :: IO ()
txnLineTest = case D.parseLogLine txnJson of
  Just (D.ActionTxn t) -> do
    D.txnAppId t `shouldBe` "stream-1"
    D.txnVersion t `shouldBe` 7
  _ -> expectationFailure "expected ActionTxn"
  where
    txnJson = "{\"txn\":{\"appId\":\"stream-1\",\"version\":7,\"lastUpdated\":1700000000000}}"


cdcLineTest :: IO ()
cdcLineTest = case D.parseLogLine cdcJson of
  Just (D.ActionCdc c) -> do
    D.cdcPath c `shouldBe` "_change_data/cdc-1.parquet"
    D.cdcSize c `shouldBe` 2048
    D.cdcDataChange c `shouldBe` False
  _ -> expectationFailure "expected ActionCdc"
  where
    cdcJson = "{\"cdc\":{\"path\":\"_change_data/cdc-1.parquet\",\"size\":2048,\"dataChange\":false,\"partitionValues\":{}}}"


-- ============================================================
-- Snapshot fold
-- ============================================================

snapshotFoldTest :: IO ()
snapshotFoldTest = do
  let actions =
        [ D.ActionAdd
            D.AddAction
              { D.addPath = "data/a.parquet"
              , D.addSize = 100
              , D.addModificationTime = 1
              , D.addDataChange = True
              , D.addStats = Nothing
              , D.addParsedStats = Nothing
              , D.addPartitionValues = Map.empty
              , D.addTags = Map.empty
              , D.addDeletionVector = Nothing
              , D.addBaseRowId = Nothing
              , D.addDefaultRowCommitVersion = Nothing
              }
        , D.ActionAdd
            D.AddAction
              { D.addPath = "data/b.parquet"
              , D.addSize = 200
              , D.addModificationTime = 2
              , D.addDataChange = True
              , D.addStats = Nothing
              , D.addParsedStats = Nothing
              , D.addPartitionValues = Map.empty
              , D.addTags = Map.empty
              , D.addDeletionVector = Nothing
              , D.addBaseRowId = Nothing
              , D.addDefaultRowCommitVersion = Nothing
              }
        , D.ActionRemove
            D.RemoveAction
              { D.removePath = "data/a.parquet"
              , D.removeDeletionTimestamp = Just 3
              , D.removeDataChange = True
              , D.removeExtendedFileMetadata = Nothing
              , D.removeSize = Nothing
              , D.removePartitionValues = Map.empty
              , D.removeDeletionVector = Nothing
              , D.removeBaseRowId = Nothing
              , D.removeDefaultRowCommitVersion = Nothing
              }
        ]
      snap = D.snapshotFromActions actions
  Map.keys (D.tsFiles snap) `shouldBe` ["data/b.parquet"]
  map D.addPath (D.activeFiles actions) `shouldBe` ["data/b.parquet"]


segmentMetadataActionsTest :: IO ()
segmentMetadataActionsTest =
  withCorruptCheckpoint $ \root logDir -> do
    let numbered = logDir </> "00000000000000000001.json"
        compacted = logDir </> "00000000000000000001.00000000000000000002.compacted.json"
        protocol = "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2}}\n"
        metadata = "{\"metaData\":{\"id\":\"id\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
        txn name version = "{\"txn\":{\"appId\":\"" <> name <> "\",\"version\":" <> version <> "}}\n"
        reject path message contents = do
          BL.writeFile path contents
          DIO.openDeltaTable root >>= \case
            Left err -> err `shouldSatisfy` isInfixOf message
            Right _ -> expectationFailure "duplicate metadata action was accepted"
        check path = do
          BL.writeFile path (protocol <> metadata <> txn "client" "-1" <> txn "other" "9")
          table <- DIO.openDeltaTable root >>= either fail pure
          D.tsAppIds (DIO.dtSnapshot table) `shouldBe` Map.fromList [("client", -1), ("other", 9)]
        checkDuplicates path = do
          reject path "duplicate protocol action" (protocol <> protocol)
          reject path "duplicate metadata action" (metadata <> metadata)
          reject path "duplicate transaction appId" (txn "client" "7" <> txn "other" "9" <> txn "client" "-1")
    check numbered
    checkDuplicates numbered
    removeFile numbered
    check compacted
    checkDuplicates compacted


segmentSchemaRecoveryTest :: IO ()
segmentSchemaRecoveryTest =
  withCorruptCheckpoint $ \root logDir -> do
    let initial = logDir </> "00000000000000000000.json"
        next = logDir </> "00000000000000000001.json"
        lastCommit = logDir </> "00000000000000000002.json"
        compacted = logDir </> "00000000000000000001.00000000000000000002.compacted.json"
        checkpoint = logDir </> "00000000000000000001.checkpoint.parquet"
        metadata = "{\"metaData\":{\"id\":\"bad\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\"}\"}}\n"
        transaction = "{\"txn\":{\"appId\":\"client\",\"version\":8}}\n"
        reject file open = open >>= \case
          Left err -> do
            err `shouldSatisfy` isInfixOf file
            err `shouldSatisfy` isInfixOf "invalid table schema"
          Right _ -> expectationFailure "accepted an invalid table schema"
    removeFile checkpoint
    BL.writeFile lastCommit transaction
    BL.writeFile compacted (metadata <> transaction)
    table <- DIO.openDeltaTable root >>= either fail pure
    DIO.activeFilePaths table `shouldBe` ["live.parquet"]
    D.mdId <$> D.tsMetaData (DIO.dtSnapshot table) `shouldBe` Just "table"
    removeFile compacted
    BL.appendFile next metadata
    reject "00000000000000000001.json" (DIO.openDeltaTable root)
    BL.writeFile initial ("{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2}}\n" <> metadata)
    reject "00000000000000000000.json" (DIO.openDeltaTableAt root 0)


segmentFileActionsTest :: IO ()
segmentFileActionsTest =
  withSystemTempDirectory "wireform-delta-segment-files" $ \root -> do
    let logDir = root </> "_delta_log"
        first = logDir </> "00000000000000000000.json"
        next = logDir </> "00000000000000000001.json"
        compacted = logDir </> "00000000000000000001.00000000000000000002.compacted.json"
        addPrefix = "{\"add\":{\"path\":\"file.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true"
        vector offset =
          ",\"deletionVector\":{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///vectors\",\"sizeInBytes\":32,\"cardinality\":1,\"offset\":"
            <> offset <> "}"
        add offset = addPrefix <> vector offset <> "}}\n"
        addOther name = "{\"add\":{\"path\":\"" <> name <> "\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
        remove offset = "{\"remove\":{\"path\":\"file.parquet\",\"dataChange\":true" <> vector offset <> "}}\n"
        check path rows = do
          BL.writeFile path rows
          table <- DIO.openDeltaTable root >>= either fail pure
          let expected = D.addDeletionVector <$> Map.elems (D.tsFiles (D.snapshotFromActions (D.parseLogFile (add "2"))))
          map D.addDeletionVector (DIO.dtActiveFiles table) `shouldBe` expected
        reject path problem rows = do
          BL.writeFile path rows
          DIO.openDeltaTable root >>= \case
            Left err -> err `shouldSatisfy` isInfixOf problem
            Right _ -> expectationFailure "invalid file actions were accepted"
    createDirectory logDir
    BL.writeFile first
      ("{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"deletionVectors\"],\"writerFeatures\":[\"deletionVectors\"]}}\n"
        <> "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\"}}\n"
        <> add "1")
    mapM_ (check next) [add "2" <> remove "1", remove "1" <> add "2"]
    reject next "missing the prior remove" (add "2")
    reject next "missing the prior remove" (add "2" <> remove "3")
    reject next "duplicate add path" (add "1" <> add "2")
    reject next "duplicate add path" (add "1" <> addOther "z.parquet" <> addOther "a.parquet" <> add "1")
    reject next "duplicate remove" (remove "1" <> remove "3")
    reject next "same logical file" (remove "1" <> add "1")
    reject next "storageType" (addPrefix <> ",\"deletionVector\":{}}}\n")
    BL.writeFile next (add "2" <> remove "1" <> addOther "z.parquet" <> addOther "a.parquet")
    shuffled <- DIO.openDeltaTable root >>= either fail pure
    DIO.activeFilePaths shuffled `shouldBe` ["a.parquet", "file.parquet", "z.parquet"]
    removeFile next
    check compacted (remove "3" <> add "2" <> remove "1")
    reject compacted "duplicate remove" (remove "1" <> remove "1" <> add "2")
    reject compacted "missing the prior remove" (remove "3" <> add "2")


deletionVectorReplayTest :: IO ()
deletionVectorReplayTest = do
  let noVector = "{\"add\":{\"path\":\"p.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}"
      removeNoVector = "{\"remove\":{\"path\":\"p.parquet\",\"dataChange\":true}}"
      action prefix offset =
        prefix
          <> "{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///vectors\",\"offset\":"
          <> offset
          <> ",\"sizeInBytes\":32,\"cardinality\":1}}}"
      addVector = action "{\"add\":{\"path\":\"p.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"deletionVector\":"
      removeVector = action "{\"remove\":{\"path\":\"p.parquet\",\"dataChange\":true,\"deletionVector\":"
      replay = D.tsFiles . D.snapshotFromActions . D.parseLogFile . BL.intercalate "\n"
      expected = replay [addVector "2"]
  Map.keys expected `shouldBe` ["p.parquet"]
  replay [noVector, addVector "2", removeNoVector] `shouldBe` expected
  replay [noVector, removeNoVector, addVector "2"] `shouldBe` expected
  replay [addVector "1", addVector "2", removeVector "1"] `shouldBe` expected
  replay [addVector "1", removeVector "1", addVector "2"] `shouldBe` expected
  replay [addVector "2", removeVector "2"] `shouldBe` Map.empty


transactionReplayTest :: IO ()
transactionReplayTest = do
  let record app version = "{\"txn\":{\"appId\":\"" <> app <> "\",\"version\":" <> version <> "}}"
      replay = D.tsAppIds . D.snapshotFromActions . D.parseLogFile . BL.intercalate "\n"
      events = [record "a" "7", record "b" "4", record "a" "3"]
  replay events `shouldBe` Map.fromList [("a", 3), ("b", 4)]
  replay (events ++ [record "a" "-1"]) `shouldBe` Map.fromList [("a", -1), ("b", 4)]


snapshotProtoMetaTest :: IO ()
snapshotProtoMetaTest = do
  let proto =
        D.ActionProtocol
          D.ProtocolAction
            { D.pMinReaderVersion = 1
            , D.pMinWriterVersion = 2
            , D.pReaderFeatures = []
            , D.pWriterFeatures = []
            }
      md =
        D.ActionMetaData
          D.MetaDataAction
            { D.mdId = "u"
            , D.mdName = Just "tbl"
            , D.mdDescription = Nothing
            , D.mdFormat = Nothing
            , D.mdSchemaString = "{\"type\":\"struct\",\"fields\":[]}"
            , D.mdPartitionColumns = []
            , D.mdConfiguration = Map.empty
            , D.mdCreatedTime = Nothing
            }
      snap = D.snapshotFromActions [proto, md]
  D.pMinWriterVersion <$> D.tsProtocol snap `shouldBe` Just 2
  fmap D.mdName (D.tsMetaData snap) `shouldBe` Just (Just "tbl")


-- ============================================================
-- Schema-string decoder
-- ============================================================

schemaTest :: IO ()
schemaTest = case D.dsFields <$> D.parseDeltaSchema schemaJson of
  Right [f0, f1, f2, f3] -> do
    D.dfName f0 `shouldBe` "id"
    D.dfType f0 `shouldBe` D.DTLong
    D.dfNullable f0 `shouldBe` False
    D.dfType f1 `shouldBe` D.DTString
    case D.dfType f2 of
      D.DTArray D.DTString _ -> pure ()
      other -> expectationFailure ("unexpected tags type: " ++ show other)
    D.dfType f3 `shouldBe` D.DTDecimal 10 2
  other -> expectationFailure ("unexpected schema: " ++ show other)
  where
    schemaJson =
      "{\"type\":\"struct\",\"fields\":[\
      \{\"name\":\"id\",\"type\":\"long\",\"nullable\":false,\"metadata\":{}},\
      \{\"name\":\"name\",\"type\":\"string\",\"nullable\":true,\"metadata\":{}},\
      \{\"name\":\"tags\",\"type\":{\"type\":\"array\",\"elementType\":\"string\",\"containsNull\":true},\"nullable\":true,\"metadata\":{}},\
      \{\"name\":\"amount\",\"type\":\"decimal(10,2)\",\"nullable\":true,\"metadata\":{}}\
      \]}"


nestedSchemaMetadataTest :: IO ()
nestedSchemaMetadataTest = case D.dsFields <$> D.parseDeltaSchema schema of
  Right [parent, absent, variant] -> do
    D.dfType absent `shouldBe` D.DTVoid
    D.dfType variant `shouldBe` D.DTVariant
    case D.dfType parent of
      D.DTStruct [nested] ->
        HM.lookup "delta.columnMapping.physicalName" (D.dfMetadata nested)
          `shouldBe` Just (Aeson.String "col-physical")
      other -> expectationFailure ("unexpected nested type: " ++ show other)
  other -> expectationFailure ("unexpected schema: " ++ show other)
  where
    schema =
      "{\"type\":\"struct\",\"fields\":[\
      \{\"name\":\"parent\",\"type\":{\"type\":\"struct\",\"fields\":[\
      \{\"name\":\"nested\",\"type\":\"long\",\"nullable\":false,\"metadata\":{\"delta.columnMapping.physicalName\":\"col-physical\"}}\
      \]},\"nullable\":true,\"metadata\":{}},\
      \{\"name\":\"absent\",\"type\":\"void\",\"nullable\":true,\"metadata\":{}},\
      \{\"name\":\"data\",\"type\":\"variant\",\"nullable\":true,\"metadata\":{}}\
      \]}"


malformedSchemaTest :: IO ()
malformedSchemaTest =
  mapM_
    check
    [ "{\"type\":\"struct\"}"
    , "{\"type\":\"struct\",\"fields\":[{\"type\":\"long\",\"nullable\":true,\"metadata\":{}}]}"
    , "{\"type\":\"struct\",\"fields\":[{\"name\":\"amount\",\"type\":\"decimal(38,39)\",\"nullable\":true,\"metadata\":{}}]}"
    ]
  where
    check schema = case D.parseDeltaSchema schema of
      Left _ -> pure ()
      Right parsed -> expectationFailure ("accepted malformed schema: " ++ show parsed)


schemaFieldNamesTest :: IO ()
schemaFieldNamesTest = do
  let field :: T.Text -> Aeson.Value -> Aeson.Value
      field name ty = Aeson.object
        ["name" Aeson..= name, "type" Aeson..= ty, "nullable" Aeson..= True, "metadata" Aeson..= HM.empty @T.Text @Aeson.Value]
      struct :: [Aeson.Value] -> Aeson.Value
      struct fields = Aeson.object ["type" Aeson..= ("struct" :: T.Text), "fields" Aeson..= fields]
      parse = D.parseDeltaSchema . TE.decodeUtf8 . BL.toStrict . Aeson.encode
      leaf name = field name (Aeson.String "long")
      child name fields = field name (struct fields)
      duplicate = [leaf "measurement", leaf "Measurement"]
      array :: Aeson.Value -> Aeson.Value
      array element = Aeson.object
        ["type" Aeson..= ("array" :: T.Text), "elementType" Aeson..= element, "containsNull" Aeson..= True]
      reject schema = case parse schema of
        Left err -> err `shouldSatisfy` isInfixOf "duplicate Delta field names"
        Right parsed -> expectationFailure ("accepted duplicate names: " ++ show parsed)
  reject (struct duplicate)
  reject (struct [child "metrics" duplicate])
  reject (struct [field "metrics" (array (struct duplicate))])
  _ <- either fail pure (parse (struct [child "first" [leaf "measurement"], child "second" [leaf "Measurement"]]))
  _ <- either fail pure (parse (struct [leaf "straße", leaf "strasse"]))
  pure ()


-- ============================================================
-- _last_checkpoint
-- ============================================================

lastCheckpointTest :: IO ()
lastCheckpointTest = case D.parseLastCheckpoint payload of
  Nothing -> expectationFailure "expected Just"
  Just lc -> do
    D.lcVersion lc `shouldBe` 12
    D.lcSize lc `shouldBe` 100
    D.lcParts lc `shouldBe` Just 4
    D.lcSizeInBytes lc `shouldBe` Just 1024
    D.lcNumOfAddFiles lc `shouldBe` Just 90
  where
    payload = "{\"version\":12,\"size\":100,\"parts\":4,\"sizeInBytes\":1024,\"numOfAddFiles\":90}"


lastCheckpointDuplicateKeysTest :: IO ()
lastCheckpointDuplicateKeysTest = do
  D.parseLastCheckpoint "{\"version\":12,\"version\":13,\"size\":100}" `shouldBe` Nothing
  D.parseLastCheckpoint "{\"version\":12,\"\\u0076ersion\":13,\"size\":100}" `shouldBe` Nothing
  D.parseLastCheckpoint "{\"version\":12,\"size\":100,\"extra\":[{\"a\":1,\"a\":2}]}" `shouldBe` Nothing
  fmap D.lcVersion (D.parseLastCheckpoint "{\"version\":12,\"size\":100,\"extra\":[{\"a\":1},{\"a\":2}]} \r\n")
    `shouldBe` Just 12


lastCheckpointChecksumTest :: IO ()
lastCheckpointChecksumTest = do
  let pointer = Just (D.LastCheckpoint 12 100 Nothing Nothing Nothing)
      signed checksum body = "{\"checksum\":\"" <> checksum <> "\"," <> body <> "}"
      -- Delta PROTOCOL.md's MD5 example, with the required size and version.
      example = "\"k0\":\"'v 0'\",\"k1\":{\"k2\":2,\"k3\":[\"v3\",[1,2],{\"k4\":\"v4\",\"k5\":[\"v5\",\"v6\",\"v7\"]}]}"
      declared = "\"size\":100,\"version\":12"
      digest = "05f027fa657e98913fe7f6e8130976d7"
      nested = "\"nested\":{\"checksum\":\"kept\"},\"unicode\":\"\\u00e9+/~\",\"other\":[0,1,2,3,4,5,6,7,8,9,10,11]"
  D.parseLastCheckpoint (signed digest (example <> "," <> declared)) `shouldBe` pointer
  D.parseLastCheckpoint (signed digest (declared <> "," <> example) <> "\r\n") `shouldBe` pointer
  D.parseLastCheckpoint (signed digest (example <> ",\"size\":101,\"version\":12")) `shouldBe` Nothing
  -- Path sorting is normative. Spark sorts the entire path-value pair,
  -- which orders the prefix-related indices 1, 10, and 11 differently.
  forM_ ["9ba259f02eb174eaa1cc9943b519573b", "6c636566587c871d69a1a2f9ebb86d60"] $ \checksum ->
    D.parseLastCheckpoint (signed checksum (nested <> "," <> declared)) `shouldBe` pointer
  D.parseLastCheckpoint (signed "635c42639bd7ebaedb6f416c881ddc88" ("\"extra\":[null,true,false,2.71]," <> declared))
    `shouldBe` pointer
  -- Numeric literals in the JVM's Double.toString format used by Jackson.
  let sparkNumbers zero =
        "\"numeric7\":[\"x\\\"7\\\\9\",[1.0E-4,1.0E7,1.0E23,4.9E-324,"
          <> zero <> ",0.001,1.0,0,2.71,1.0000000000000002E20]]"
      sparkDigest = "cbcfb5aa735481e6959a7362b5ecbd6a"
  D.parseLastCheckpoint (signed sparkDigest (sparkNumbers "-0.0" <> "," <> declared)) `shouldBe` pointer
  D.parseLastCheckpoint (signed sparkDigest (declared <> ",  " <> sparkNumbers "-0.0")) `shouldBe` pointer
  D.parseLastCheckpoint (signed sparkDigest (sparkNumbers "0.0" <> "," <> declared)) `shouldBe` Nothing
  forM_ ["null", "42", "\"bad\""] $ \checksum ->
    D.parseLastCheckpoint ("{\"size\":100,\"version\":12,\"checksum\":" <> checksum <> "}") `shouldBe` Nothing
  withCorruptCheckpoint $ \root logDir -> do
    BL.writeFile (logDir </> "_last_checkpoint") (BL.fromStrict (signed digest declared))
    table <- DIO.openDeltaTable root >>= either fail pure
    DIO.dtLastCheckpoint table `shouldBe` Nothing
    DIO.activeFilePaths table `shouldBe` ["live.parquet"]


-- ============================================================
-- AddStats
-- ============================================================

addStatsTest :: IO ()
addStatsTest = case decodeAddStats statsJson of
  Just s -> do
    D.asNumRecords s `shouldBe` Just 1000
    HM.lookup "id" (D.asNullCount s) `shouldBe` Just 3
  Nothing -> expectationFailure "expected to decode stats"
  where
    statsJson = "{\"numRecords\":1000,\"minValues\":{\"id\":1},\"maxValues\":{\"id\":999},\"nullCount\":{\"id\":3}}"


decodeAddStats :: BL.ByteString -> Maybe D.AddStats
decodeAddStats bs = case Aeson.decode bs of
  Just v -> case Aeson.fromJSON v of
    Aeson.Success a -> Just a
    Aeson.Error _ -> Nothing
  Nothing -> Nothing


nestedStatsTest :: IO ()
nestedStatsTest = do
  stats <- either fail pure (Aeson.eitherDecode "{\"nullCount\":{\"scalar\":2,\"nested\":{\"value\":3}}}")
  D.asNullCount stats `shouldBe` HM.singleton "scalar" 2
  HM.lookup "nested" (D.asNullCounts stats) `shouldBe` Just (D.NestedNullCounts (HM.singleton "value" (D.NullCount 3)))
  D.effectiveTightBounds stats `shouldBe` True
  wide <- either fail pure (Aeson.eitherDecode "{\"tightBounds\":false}")
  D.effectiveTightBounds wide `shouldBe` False


{- | The 'D.decodeAddStats' helper threads the raw @stats@
string through 'D.AddStats' for callers; we just need to
pin its three branches.
-}
decodeAddStatsHelperTest :: IO ()
decodeAddStatsHelperTest = do
  let mkAdd s =
        D.AddAction
          { D.addPath = "x"
          , D.addSize = 0
          , D.addModificationTime = 0
          , D.addDataChange = True
          , D.addStats = s
          , D.addParsedStats = Nothing
          , D.addPartitionValues = mempty
          , D.addTags = mempty
          , D.addDeletionVector = Nothing
          , D.addBaseRowId = Nothing
          , D.addDefaultRowCommitVersion = Nothing
          }
  -- No stats: Nothing.
  D.decodeAddStats (mkAdd Nothing) `shouldBe` Nothing
  -- Valid JSON: Just (Right ...).
  case D.decodeAddStats (mkAdd (Just "{\"numRecords\":7}")) of
    Just (Right s) -> D.asNumRecords s `shouldBe` Just 7
    other -> expectationFailure ("expected Just (Right ...), got " ++ show other)
  -- Malformed JSON: Just (Left ...).
  case D.decodeAddStats (mkAdd (Just "not-json")) of
    Just (Left _) -> pure ()
    other -> expectationFailure ("expected Just (Left _), got " ++ show other)
  structured <- either fail pure (Aeson.eitherDecode "{\"numRecords\":4,\"nullCount\":{\"missing\":null,\"nested\":{\"missing\":null,\"present\":1}}}")
  let parsed = (mkAdd Nothing) {D.addParsedStats = Just structured}
  D.decodeAddStats parsed `shouldBe` Just (Right structured)
  D.asNullCounts structured
    `shouldBe` HM.singleton "nested" (D.NestedNullCounts (HM.singleton "present" (D.NullCount 1)))
  fmap (fmap D.asNumRecords) (D.decodeAddStats (parsed {D.addStats = Just "{\"numRecords\":7}"}))
    `shouldBe` Just (Right (Just 7))


checkpointVariantStatsTest :: IO ()
checkpointVariantStatsTest = do
  let value =
        Variant.VObject
          (Map.fromList [("$['a']", Variant.VString "min-string"), ("$['b']['c']", Variant.VInt8 1)])
      leaf = PN.NSRequired . PN.NSPrimitive
      record = PN.NSStruct . V.fromList
      fields =
        V.singleton
          ( "add"
          , record
              [ ("path", leaf PN.LtString)
              , ("size", leaf PN.LtInt64)
              , ("modificationTime", leaf PN.LtInt64)
              , ("dataChange", leaf PN.LtBool)
              , ("stats_parsed", record [("minValues", record [("variant", PN.NSOptional PN.NSVariant)])])
              ]
          )
  encoded <- either fail pure (Variant.encodeVariant value)
  let oneRecord = PN.NRStruct . V.fromList
      row =
        oneRecord
          [ PN.NRLeaf (PN.LvString "file.parquet")
          , PN.NRLeaf (PN.LvInt64 1)
          , PN.NRLeaf (PN.LvInt64 0)
          , PN.NRLeaf (PN.LvBool True)
          , oneRecord [oneRecord [PN.NRVariantBytes (Variant.variantMetadata encoded) (Variant.variantValue encoded)]]
          ]
  bytes <- either fail pure (PN.buildNestedFile fields (V.singleton (V.singleton row)))
  let protocol = D.ProtocolAction 3 7 ["variantType", "variantShredding"] ["variantType", "variantShredding"]
      metadata = D.MetaDataAction "table" Nothing Nothing Nothing
        "{\"type\":\"struct\",\"fields\":[{\"name\":\"bound\",\"type\":\"variant\",\"nullable\":true,\"metadata\":{\"delta.columnMapping.physicalName\":\"variant\",\"delta.columnMapping.id\":1}}]}"
        [] (Map.singleton "delta.columnMapping.mode" "name") Nothing
      mappedProtocol = protocol
        { D.pReaderFeatures = "columnMapping" : D.pReaderFeatures protocol
        , D.pWriterFeatures = "columnMapping" : D.pWriterFeatures protocol
        }
      snapshot = D.emptySnapshot {D.tsProtocol = Just mappedProtocol, D.tsMetaData = Just metadata}
      table = DIO.DeltaTable "" Nothing Nothing [] snapshot (Just 0)
      check input = do
        actions <- either fail pure (Checkpoint.decodeCheckpointFile input)
        case actions of
          [D.ActionAdd add] -> case D.decodeAddStats add of
            Just (Right stats) -> do
              HM.lookup "variant" (D.asMinValues stats) `shouldBe` Just (Aeson.String expected)
              readStats <- either fail pure (Scan.tableFileStats table)
              Just logical <- either fail pure (readStats add)
              D.asMinValues logical `shouldBe` HM.singleton "bound" (Aeson.String expected)
            other -> expectationFailure ("expected Variant statistics, got " ++ show other)
          _ -> expectationFailure "expected one checkpoint add"
  check bytes
  parquet <- either fail pure (PR.loadParquetFile bytes)
  trailer <- either fail pure (PF.readFooterTrailer bytes)
  let footer = PR.pfFooter parquet
      stripVariant node | PT.seLogicalType node == Just (PT.LTVariant 1) = node {PT.seLogicalType = Nothing}
      stripVariant node = node
      field name kind = Aeson.object
        ["name" Aeson..= (name :: T.Text), "type" Aeson..= (kind :: Aeson.Value),
         "nullable" Aeson..= True, "metadata" Aeson..= Aeson.object []]
      struct fields' = Aeson.object ["type" Aeson..= ("struct" :: T.Text), "fields" Aeson..= (fields' :: [Aeson.Value])]
      sparkMetadata kind =
        let bound = struct [field "variant" kind]
            stats = struct [field "minValues" bound, field "maxValues" bound]
            schema = struct [field "add" (struct [field "stats_parsed" stats])]
        in Just (V.singleton (PT.KeyValue "org.apache.spark.sql.parquet.row.metadata" (Just (BL.toStrict (Aeson.encode schema)))))
      plainMetadata = sparkMetadata (struct
        [field "metadata" (Aeson.String "binary"), field "value" (Aeson.String "binary")])
      legacyFooter = footer {PT.fmSchema = V.map stripVariant (PT.fmSchema footer)}
      dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
      replaceFooter f = BS.take dataEnd bytes <> PF.writeFooter f
  check (replaceFooter (legacyFooter {PT.fmKeyValueMetadata = sparkMetadata (Aeson.String "variant")}))
  mapM_ (\fileMetadata -> do
    [D.ActionAdd plain] <- either fail pure (Checkpoint.decodeCheckpointFile
      (replaceFooter (legacyFooter {PT.fmKeyValueMetadata = fileMetadata})))
    Just (Right plainStats) <- pure (D.decodeAddStats plain)
    HM.lookup "variant" (D.asMinValues plainStats) `shouldBe` Just (Aeson.object []))
    [Nothing, plainMetadata]
  where
    -- Spark's DeltaStatsJsonUtils encodes the metadata before the value.
    expected = "5DR}p5HpNdvjbtatpi(cu0wW^cTu=P0096c4jMddzy]{KA+PA73&{td"


scanTypedOnlyVariantTest :: IO ()
scanTypedOnlyVariantTest =
  withSystemTempDirectory "wireform-delta-typed-variant" $ \root -> do
    let logDir = root </> "_delta_log"
        optionalVariant = PN.NSOptional $ PN.NSStruct $ V.fromList
          [ ("metadata", PN.NSRequired (PN.NSPrimitive PN.LtBinary))
          , ("typed_value", PN.NSOptional (PN.NSPrimitive PN.LtInt32))
          ]
        fields = V.singleton ("v", optionalVariant)
        value n = PN.NRStruct $ V.fromList
          [ PN.NRLeaf (PN.LvBinary (BS.pack [1, 0, 0]))
          , n
          ]
        rows = V.fromList [value (PN.NRLeaf (PN.LvInt32 7)), PN.NRNull, value PN.NRNull]
        logActions features size =
          "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":"
            <> BB.lazyByteString (Aeson.encode features)
            <> ",\"writerFeatures\":" <> BB.lazyByteString (Aeson.encode features) <> "}}\n"
            <> "{\"metaData\":{\"id\":\"variant-test\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[{\\\"name\\\":\\\"v\\\",\\\"type\\\":\\\"variant\\\",\\\"nullable\\\":true,\\\"metadata\\\":{}}]}\"}}\n"
            <> "{\"add\":{\"path\":\"data.parquet\",\"partitionValues\":{},\"size\":"
            <> BB.intDec size
            <> ",\"modificationTime\":0,\"dataChange\":true}}\n"
    -- The Variant writer supplies a fallback field. Build the equivalent
    -- physical struct and annotate its footer to exercise its omission.
    bytes <- either fail pure (PN.buildNestedFile fields (V.singleton rows))
    file <- either fail pure (PR.loadParquetFile bytes)
    trailer <- either fail pure (PF.readFooterTrailer bytes)
    let footer = PR.pfFooter file
        schema = PT.fmSchema footer
        annotated = schema V.// [(1, (schema V.! 1) {PT.seLogicalType = Just (PT.LTVariant 1)})]
        dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
        variantFile = BS.take dataEnd bytes <> PF.writeFooter (footer {PT.fmSchema = annotated})
    BS.writeFile (root </> "data.parquet") variantFile
    createDirectory logDir
    let openResult :: [T.Text] -> IO (Either String DIO.DeltaTable)
        openResult features = do
          BL.writeFile (logDir </> "00000000000000000000.json") (BB.toLazyByteString (logActions features (BS.length variantFile)))
          DIO.openDeltaTable root
        open features = openResult features >>= either fail pure
        check features = do
          table <- open features
          (logical, batches) <- either fail pure (Scan.scanTable table)
          output <- iterIOToList batches >>= either fail pure
          case output of
            [batch] -> do
              reader <- either fail pure (VA.compileVariantArrowReader (AT.arrowFields logical V.! 0) (Scan.batchColumns batch V.! 0))
              V.generateM (Scan.batchRowCount batch) (either fail pure . VA.variantArrowAt reader)
                >>= (\actual -> actual `shouldBe` V.fromList [Just (Variant.VInt32 7), Nothing, Just Variant.VNull])
            _ -> expectationFailure "expected one Variant batch"
        reject message result = case result of
          Left err -> err `shouldSatisfy` isInfixOf message
          Right _ -> expectationFailure ("accepted Variant data without " ++ message)
    check ["variantType", "variantShredding"]
    check ["variantType-preview", "variantShredding-preview"]
    check ["variantShredding-preview"]
    plain <- open ["variantType"]
    (_, batches) <- either fail pure (Scan.scanTable plain)
    iterIOToList batches >>= reject "variantShredding"
    [plainFile] <- pure (DIO.dtActiveFiles plain)
    let statsFile = plainFile {D.addStats = Just "{\"numRecords\":3,\"minValues\":{\"v\":\"bound\"},\"nullCount\":{\"v\":1}}"}
    readStats <- either fail pure (Scan.tableFileStats plain)
    Just stats <- either fail pure (readStats statsFile)
    D.asMinValues stats `shouldBe` HM.empty
    D.asNullCount stats `shouldBe` HM.singleton "v" 1
    openResult ["variantShredding"] >>= reject "variantType"
    unknown <- open []
    reject "variantType" (Scan.scanTable unknown)
    encoded <- either fail pure (Variant.encodeVariant (Variant.VInt32 7))
    let plainFields = V.singleton ("v", PN.NSOptional PN.NSVariant)
        plainCell = PN.NRVariantBytes (Variant.variantMetadata encoded) (Variant.variantValue encoded)
        plainRows = V.singleton (V.fromList [plainCell, PN.NRNull])
    plainBytes <- either fail pure (PN.buildNestedFile plainFields plainRows)
    BS.writeFile (root </> "data.parquet") plainBytes
    (_, plainBatches) <- either fail pure (Scan.scanTable plain)
    plainOutput <- iterIOToList plainBatches >>= either fail pure
    sum (map Scan.batchRowCount plainOutput) `shouldBe` 2
    -- Spark can emit the same shredded layout without the annotation.
    BS.writeFile (root </> "data.parquet") bytes
    check ["variantType", "variantShredding"]
    check ["variantShredding-preview"]
    (_, legacyBatches) <- either fail pure (Scan.scanTable plain)
    iterIOToList legacyBatches >>= reject "variantShredding"


scanMappedLegacyVariantsTest :: IO ()
scanMappedLegacyVariantsTest =
  withSystemTempDirectory "wireform-delta-mapped-legacy-variants" $ \root -> do
    let logDir = root </> "_delta_log"
        dataPath = root </> "data.parquet"
        required = PN.NSRequired . PN.NSPrimitive
        variant = PN.NSStruct $ V.fromList
          [ ("metadata", required PN.LtBinary)
          , ("typed_value", PN.NSOptional (PN.NSPrimitive PN.LtInt32))
          ]
        fields = V.fromList
          [ ("list-physical", PN.NSList (PN.NSOptional variant))
          , ("map-physical", PN.NSMap (required PN.LtString) (PN.NSOptional variant))
          ]
        value n = PN.NRStruct (V.fromList
          [PN.NRLeaf (PN.LvBinary (BS.pack [1, 0, 0])), PN.NRLeaf (PN.LvInt32 n)])
        columns = V.fromList
          [ PN.NRList (V.fromList [value 7, PN.NRNull])
          , PN.NRMapEntries (V.singleton (PN.NRLeaf (PN.LvString "key"), value 42))
          ]
        field name physical ident kind = Aeson.object
          [ "name" Aeson..= (name :: T.Text), "type" Aeson..= kind, "nullable" Aeson..= True
          , "metadata" Aeson..= Aeson.object
              ["delta.columnMapping.physicalName" Aeson..= (physical :: T.Text), "delta.columnMapping.id" Aeson..= (ident :: Int)]
          ]
        schema = Aeson.object
          [ "type" Aeson..= ("struct" :: T.Text)
          , "fields" Aeson..=
              [ field "items" "stale-list-name" 1 (Aeson.object
                  ["type" Aeson..= ("array" :: T.Text), "elementType" Aeson..= ("variant" :: T.Text), "containsNull" Aeson..= True])
              , field "lookup" "stale-map-name" 2 (Aeson.object
                  ["type" Aeson..= ("map" :: T.Text), "keyType" Aeson..= ("string" :: T.Text),
                   "valueType" Aeson..= ("variant" :: T.Text), "valueContainsNull" Aeson..= True])
              ]
          ]
    bytes <- either fail pure (PN.buildNestedFile fields (V.map V.singleton columns))
    file <- either fail pure (PR.loadParquetFile bytes)
    trailer <- either fail pure (PF.readFooterTrailer bytes)
    let footer = PR.pfFooter file
        identify node = case PT.seName node of
          "list-physical" -> node {PT.seFieldId = Just 1}
          "map-physical" -> node {PT.seFieldId = Just 2}
          _ -> node
        dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
        dataFile = BS.take dataEnd bytes <> PF.writeFooter (footer {PT.fmSchema = V.map identify (PT.fmSchema footer)})
        actions = mconcat
          [ "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"variantType\",\"variantShredding\",\"columnMapping\"],\"writerFeatures\":[\"variantType\",\"variantShredding\",\"columnMapping\"]}}\n"
          , "{\"metaData\":{\"id\":\"legacy\",\"configuration\":{\"delta.columnMapping.mode\":\"id\"},\"schemaString\":"
          , Aeson.encode (TE.decodeUtf8 (BL.toStrict (Aeson.encode schema)))
          , "}}\n"
          , "{\"add\":{\"path\":\"data.parquet\",\"size\":"
          , BLC.pack (show (BS.length dataFile))
          , ",\"modificationTime\":0,\"dataChange\":true}}\n"
          ]
    createDirectory logDir
    BS.writeFile dataPath dataFile
    BL.writeFile (logDir </> "00000000000000000000.json") actions
    table <- DIO.openDeltaTable root >>= either fail pure
    (logical, batches) <- either fail pure (Scan.scanTable table)
    [batch] <- iterIOToList batches >>= either fail pure
    let listField = AT.arrowFields logical V.! 0
        mapField = AT.arrowFields logical V.! 1
        arrayVariant = AT.fieldChildren listField V.! 0
        mapVariant = AT.fieldChildren (AT.fieldChildren mapField V.! 0) V.! 1
    case V.toList (Scan.batchColumns batch) of
      [AC.ColList _ offsets items, AC.ColMap _ mapOffsets _ values] -> do
        offsets `shouldBe` VS.fromList [0, 2]
        mapOffsets `shouldBe` VS.fromList [0, 1]
        itemReader <- either fail pure (VA.compileVariantArrowReader arrayVariant items)
        VA.variantArrowAt itemReader 0 `shouldBe` Right (Just (Variant.VInt32 7))
        VA.variantArrowAt itemReader 1 `shouldBe` Right Nothing
        mapReader <- either fail pure (VA.compileVariantArrowReader mapVariant values)
        VA.variantArrowAt mapReader 0 `shouldBe` Right (Just (Variant.VInt32 42))
      _ -> expectationFailure "expected list and map of logical Variants"


checkpointTimestampStatsTest :: IO ()
checkpointTimestampStatsTest = do
  let leaf = PN.NSRequired . PN.NSPrimitive
      group = PN.NSStruct . V.fromList
      struct = PN.NRStruct . V.fromList
      bounds = group
        [ ("legacy", leaf PN.LtInt96)
        , ("extended", leaf (PN.LtTimestamp96 True PT.LtMicros))
        , ("beyondInt64", leaf (PN.LtTimestamp96 True PT.LtNanos))
        , ("ordinary", leaf (PN.LtTimestamp False PT.LtMicros))
        , ("early", leaf (PN.LtTimestamp True PT.LtMicros))
        ]
      schema = V.singleton ("add", group
        [ ("path", leaf PN.LtString)
        , ("size", leaf PN.LtInt64)
        , ("modificationTime", leaf PN.LtInt64)
        , ("dataChange", leaf PN.LtBool)
        , ("stats_parsed", group [("minValues", bounds)])
        ])
      bytes = BL.toStrict . BB.toLazyByteString
      legacy = bytes (BB.int64LE (-1) <> BB.int32LE 2440588)
      extended = bytes (BB.int64LE (-1) <> BB.int32LE (-1))
      beyondInt64 = bytes (BB.word64LE 0x8000000000000000 <> BB.int32LE 0)
      row = V.singleton $ struct
        [ PN.NRLeaf (PN.LvString "file.parquet")
        , PN.NRLeaf (PN.LvInt64 1)
        , PN.NRLeaf (PN.LvInt64 0)
        , PN.NRLeaf (PN.LvBool True)
        , struct [struct
            [ PN.NRLeaf (PN.LvFixedBinary legacy)
            , PN.NRLeaf (PN.LvFixedBinary extended)
            , PN.NRLeaf (PN.LvFixedBinary beyondInt64)
            , PN.NRLeaf (PN.LvInt64 (-1))
            , PN.NRLeaf (PN.LvInt64 (-55_767_401_608_000_000))
            ]]
        ]
  input <- either fail pure (PN.buildNestedFile schema (V.singleton row))
  actions <- either fail pure (Checkpoint.decodeCheckpointFile input)
  case actions of
    [D.ActionAdd file] -> case D.decodeAddStats file of
      Just (Right stats) -> D.asMinValues stats `shouldBe` HM.fromList
        [ ("legacy", Aeson.String "1969-12-31 23:59:59.999999999")
        , ("extended", Aeson.String "1969-12-31T23:59:59.999999Z")
        , ("beyondInt64", Aeson.String "2262-04-11T23:47:16.854775808Z")
        , ("ordinary", Aeson.String "1969-12-31 23:59:59.999999")
        , ("early", Aeson.String "0202-10-20T23:06:32Z")
        ]
      other -> expectationFailure ("expected timestamp bounds, got " ++ show other)
    _ -> expectationFailure "expected one checkpoint add"


sparkIntervalTest :: IO ()
sparkIntervalTest =
  withSystemTempDirectory "wireform-delta-spark-interval" $ \root -> do
    let field name typ = Aeson.object
          [ "name" Aeson..= (name :: T.Text), "type" Aeson..= (typ :: T.Text)
          , "nullable" Aeson..= True, "metadata" Aeson..= Aeson.object []
          ]
        schema = Aeson.object
          [ "type" Aeson..= ("struct" :: T.Text)
          , "fields" Aeson..=
              [field "months" "interval year to month", field "elapsed" "interval day to second",
               field "partition_months" "interval year to month", field "partition_elapsed" "interval day to second"]
          ]
        meta = D.MetaDataAction "table" Nothing Nothing Nothing (TE.decodeUtf8 (BL.toStrict (Aeson.encode schema)))
          ["partition_months", "partition_elapsed"] mempty Nothing
        inputFields = V.fromList
          [ ("months", PN.NSOptional (PN.NSPrimitive PN.LtInt32))
          , ("elapsed", PN.NSOptional (PN.NSPrimitive PN.LtInt64))
          ]
        values leaf bounds = V.fromList [PN.NRLeaf (leaf (fst bounds)), PN.NRNull, PN.NRLeaf (leaf (snd bounds))]
        inputValues = V.fromList [values PN.LvInt32 (minBound, maxBound), values PN.LvInt64 (minBound, 999999)]
        scan months elapsed = do
          let add = D.AddAction "interval.parquet" 1 0 True Nothing Nothing
                (Map.fromList [("partition_months", months), ("partition_elapsed", elapsed)]) mempty Nothing Nothing Nothing
              snapshot = D.emptySnapshot
                { D.tsProtocol = Just (D.ProtocolAction 1 2 [] [])
                , D.tsMetaData = Just meta
                , D.tsFiles = Map.singleton (D.addPath add) add
                }
              table = DIO.DeltaTable root Nothing Nothing [] snapshot (Just 0)
          (resultSchema, stream) <- either fail pure (Scan.scanTable table)
          result <- iterIOToList stream
          pure (resultSchema, result)
    bytes <- either fail pure (PN.buildNestedFile inputFields inputValues)
    BS.writeFile (root </> "interval.parquet") bytes
    (resultSchema, result) <- scan (Just "INTERVAL '-0-11' YEAR TO MONTH") (Just "INTERVAL '-106751991 04:00:54.775808000' DAY TO SECOND")
    map AT.fieldType (V.toList (AT.arrowFields resultSchema)) `shouldBe`
      [AT.AInterval AT.YearMonth, AT.ADuration AT.Microsecond, AT.AInterval AT.YearMonth, AT.ADuration AT.Microsecond]
    [batch] <- either fail pure result
    case V.toList (Scan.batchColumns batch) of
      [AC.ColIntervalYearMonth monthValid months, AC.ColDuration durationValid elapsed,
        AC.ColIntervalYearMonth Nothing partMonths, AC.ColDuration Nothing partElapsed] -> do
          (VS.head months, VS.last months) `shouldBe` (minBound, maxBound)
          (VS.head elapsed, VS.last elapsed) `shouldBe` (minBound, 999999)
          AC.isValidAt monthValid 1 `shouldBe` False
          AC.isValidAt durationValid 1 `shouldBe` False
          partMonths `shouldBe` VS.replicate 3 (-11)
          partElapsed `shouldBe` VS.replicate 3 minBound
      _ -> expectationFailure "expected month intervals and microsecond durations"
    let invalid months elapsed = scan months elapsed >>= \case
          (_, Left err) -> err `shouldSatisfy` isInfixOf "invalid partition value"
          _ -> expectationFailure "accepted an invalid ANSI interval partition"
    invalid (Just "INTERVAL '0-12' YEAR TO MONTH") Nothing
    invalid (Just "INTERVAL '178956970-8' YEAR TO MONTH") Nothing
    invalid Nothing (Just "INTERVAL '0 24:00:00' DAY TO SECOND")
    invalid Nothing (Just "INTERVAL '0 00:00:00.0000001' DAY TO SECOND")


sparkLegacyDateTest :: IO ()
sparkLegacyDateTest =
  withSystemTempDirectory "wireform-delta-spark-legacy-date" $ \root -> do
    let logDir = root </> "_delta_log"
        dataPath = root </> "date.parquet"
        commit = logDir </> "00000000000000000000.json"
        leaf = PN.NSRequired . PN.NSPrimitive
        group = PN.NSStruct . V.fromList
        struct = PN.NRStruct . V.fromList
        date = PN.NRLeaf . PN.LvInt32
        fixture = V.fromList [date (-141704), date (-171596), date (-719530), PN.NRNull, date (-141427), date 0]
        dayField fieldType metadata = Aeson.object
          ["name" Aeson..= ("day" :: T.Text), "type" Aeson..= (fieldType :: T.Text),
           "nullable" Aeson..= True, "metadata" Aeson..= (metadata :: Aeson.Value)]
        commitActions features column =
          let protocol = Aeson.object
                ["minReaderVersion" Aeson..= (3 :: Int), "minWriterVersion" Aeson..= (7 :: Int),
                 "readerFeatures" Aeson..= (features :: [T.Text]), "writerFeatures" Aeson..= features]
              schema = Aeson.object ["type" Aeson..= ("struct" :: T.Text), "fields" Aeson..= [column]]
              metadata = Aeson.object
                ["id" Aeson..= ("test" :: T.Text), "schemaString" Aeson..= TE.decodeUtf8 (BL.toStrict (Aeson.encode schema))]
          in "{\"protocol\":" <> Aeson.encode protocol <> "}\n"
            <> "{\"metaData\":" <> Aeson.encode metadata <> "}\n"
            <> "{\"add\":{\"path\":\"date.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
        patchFooter bytes metadata = do
          file <- either fail pure (PR.loadParquetFile bytes)
          trailer <- either fail pure (PF.readFooterTrailer bytes)
          let dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
          pure (BS.take dataEnd bytes <> PF.writeFooter ((PR.pfFooter file) {PT.fmKeyValueMetadata = Just (V.fromList metadata)}))
        version name = PT.KeyValue "org.apache.spark.version" (Just name)
        legacy = PT.KeyValue "org.apache.spark.legacyDateTime" (Just "")
        readColumn bytes = do
          BS.writeFile dataPath bytes
          table <- DIO.openDeltaTable root >>= either fail pure
          (_, batches) <- either fail pure (Scan.scanTable table)
          [batch] <- iterIOToList batches >>= either fail pure
          pure (Scan.batchColumns batch V.! 0)
        readDates bytes = do
          column <- readColumn bytes
          case column of
            AC.ColDate32 valid values ->
              pure (V.generate (VS.length values) (\i -> if AC.isValidAt valid i then Just (values VS.! i) else Nothing))
            _ -> fail "expected DATE column"
        corrected = V.fromList [Just (-141714), Just (-171605), Just (-719528), Nothing, Just (-141427), Just 0]
        stored = V.fromList [Just (-141704), Just (-171596), Just (-719530), Nothing, Just (-141427), Just 0]
    createDirectory logDir
    BL.writeFile commit (commitActions [] (dayField "date" (Aeson.object [])))
    dataBytes <- either fail pure
      (PN.buildNestedFile (V.singleton ("day", PN.NSOptional (PN.NSPrimitive PN.LtDate))) (V.singleton fixture))
    patchFooter dataBytes [version "3.5.8", legacy] >>= readDates >>= (`shouldBe` corrected)
    patchFooter dataBytes [version "2.4.8"] >>= readDates >>= (`shouldBe` corrected)
    patchFooter dataBytes [version "10.0.0"] >>= readDates >>= (`shouldBe` stored)
    patchFooter dataBytes [version "3.5.8"] >>= readDates >>= (`shouldBe` stored)
    readDates dataBytes >>= (`shouldBe` stored)
    let change = Aeson.object ["fromType" Aeson..= ("date" :: T.Text), "toType" Aeson..= ("timestamp_ntz" :: T.Text)]
    BL.writeFile commit $ commitActions ["typeWidening", "timestampNtz"]
      (dayField "timestamp_ntz" (Aeson.object ["delta.typeChanges" Aeson..= [change]]))
    widened <- patchFooter dataBytes [version "3.5.8", legacy] >>= readColumn
    case widened of
      AC.ColTimestamp valid values ->
        V.generate (VS.length values) (\i -> if AC.isValidAt valid i then Just (values VS.! i) else Nothing)
          `shouldBe` V.map (fmap ((* 86_400_000_000) . fromIntegral)) corrected
      _ -> expectationFailure "expected widened timestamp_ntz column"
    let bounds = group [("day", leaf PN.LtDate)]
        checkpointSchema = V.singleton ("add", group
          [ ("path", leaf PN.LtString), ("size", leaf PN.LtInt64), ("modificationTime", leaf PN.LtInt64)
          , ("dataChange", leaf PN.LtBool), ("stats_parsed", group [("minValues", bounds), ("maxValues", bounds)])
          ])
        row = struct
          [ PN.NRLeaf (PN.LvString "date.parquet"), PN.NRLeaf (PN.LvInt64 1)
          , PN.NRLeaf (PN.LvInt64 0), PN.NRLeaf (PN.LvBool True)
          , struct [struct [date (-141704)], struct [date (-141427)]]
          ]
    checkpoint <- either fail pure (PN.buildNestedFile checkpointSchema (V.singleton (V.singleton row)))
    legacyCheckpoint <- patchFooter checkpoint [version "3.5.8", legacy]
    [D.ActionAdd add] <- either fail pure (Checkpoint.decodeCheckpointFile legacyCheckpoint)
    Just (Right stats) <- pure (D.decodeAddStats add)
    D.asMinValues stats `shouldBe` HM.singleton "day" (Aeson.String "1582-01-01")
    D.asMaxValues stats `shouldBe` HM.singleton "day" (Aeson.String "1582-10-15")


sparkLegacyTimestampTest :: IO ()
sparkLegacyTimestampTest =
  withSystemTempDirectory "wireform-delta-spark-legacy-time" $ \root -> do
    let logDir = root </> "_delta_log"
        dataPath = root </> "time.parquet"
        ancient = -12_243_196_799_876_544 :: Int64
        before1900 = -2_208_988_800_000_001
        start1900 = before1900 + 1
        ancientMillis = ancient `div` 1000
        ancientRebased = ancient - 861_450_000_000
        millisRebased = ancientMillis * 1000 - 861_450_000_000
        leaf = PN.NSRequired . PN.NSPrimitive
        optional = PN.NSOptional . PN.NSPrimitive
        group = PN.NSStruct . V.fromList
        struct = PN.NRStruct . V.fromList
        time = PN.NRLeaf . PN.LvInt64
        int96 micros =
          let (day, fraction) = micros `divMod` 86_400_000_000
              encoded = BB.int64LE (fraction * 1000) <> BB.int32LE (fromIntegral (day + 2440588))
          in PN.NRLeaf (PN.LvFixedBinary (BL.toStrict (BB.toLazyByteString encoded)))
        utcMicros = PN.LtTimestamp True PT.LtMicros
        utcMillis = PN.LtTimestamp True PT.LtMillis
        localMicros = PN.LtTimestamp False PT.LtMicros
        version value = PT.KeyValue "org.apache.spark.version" (Just value)
        zone value = PT.KeyValue "org.apache.spark.timeZone" (Just value)
        legacy = PT.KeyValue "org.apache.spark.legacyDateTime" (Just "")
        legacy96 = PT.KeyValue "org.apache.spark.legacyINT96" Nothing
        amsterdam = zone "Europe/Amsterdam"
        add = "{\"add\":{\"path\":\"time.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
        protocol = "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"timestampNtz\"],\"writerFeatures\":[\"timestampNtz\"]}}\n"
        fields = V.fromList [("utc", optional utcMicros), ("millis", optional utcMillis), ("int96", optional PN.LtInt96), ("local", optional localMicros)]
        deltaField (name, format) = Aeson.object
          [ "name" Aeson..= name, "type" Aeson..= (format :: T.Text)
          , "nullable" Aeson..= True, "metadata" Aeson..= Aeson.object []
          ]
        schema = Aeson.object ["type" Aeson..= ("struct" :: T.Text), "fields" Aeson..=
          V.map deltaField (V.imap (\i (name, _) -> (name, if i == 3 then "timestamp_ntz" else "timestamp")) fields)]
        metadata = Aeson.object
          ["id" Aeson..= ("test" :: T.Text), "schemaString" Aeson..= TE.decodeUtf8 (BL.toStrict (Aeson.encode schema))]
        input = V.fromList
          [ V.fromList [time ancient, PN.NRNull, time before1900, time start1900]
          , V.fromList [time ancientMillis, PN.NRNull, time (-1), time 0]
          , V.fromList [int96 ancient, PN.NRNull, int96 before1900, int96 start1900]
          , V.fromList [time ancient, PN.NRNull, time before1900, time start1900]
          ]
        patchFooter bytes entries = do
          file <- either fail pure (PR.loadParquetFile bytes)
          trailer <- either fail pure (PF.readFooterTrailer bytes)
          let dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
          pure (BS.take dataEnd bytes <> PF.writeFooter ((PR.pfFooter file) {PT.fmKeyValueMetadata = Just (V.fromList entries)}))
        scan bytes entries = do
          patchFooter bytes entries >>= BS.writeFile dataPath
          table <- DIO.openDeltaTable root >>= either fail pure
          (_, batches) <- either fail pure (Scan.scanTable table)
          fmap (map (map timestampValues . V.toList . Scan.batchColumns)) <$> iterIOToList batches
        timestampValues (AC.ColTimestamp valid values) =
          V.toList (V.generate (VS.length values) (\i -> if AC.isValidAt valid i then Just (values VS.! i) else Nothing))
        timestampValues _ = []
        expected rebased = [Just rebased, Nothing, Just (before1900 + 3_600_000_000), Just start1900]
        original = [Just ancient, Nothing, Just before1900, Just start1900]
        originalMillis = [Just (ancientMillis * 1000), Nothing, Just (-1000), Just 0]
        suffix message = \case
          Left err -> err `shouldSatisfy` isInfixOf message
          Right _ -> expectationFailure "expected the timestamp scan to fail"

    createDirectory logDir
    BL.writeFile (logDir </> "00000000000000000000.json") (protocol <> "{\"metaData\":" <> Aeson.encode metadata <> "}\n" <> add)
    bytes <- either fail pure (PN.buildNestedFile fields input)
    scan bytes [version "3.5.8", amsterdam, legacy, legacy96] >>= (`shouldBe` Right
      [[expected ancientRebased, [Just millisRebased, Nothing, Just (-1000), Just 0], expected ancientRebased, original]])
    scan bytes [version "3.0.3", amsterdam] >>= (`shouldBe` Right [[original, originalMillis, expected ancientRebased, original]])
    scan bytes [version "10.0.0", amsterdam] >>= (`shouldBe` Right [[original, originalMillis, original, original]])
    scan bytes [version "2.4.8", zone "UTC"] >>= (`shouldBe` Right
      [[[Just (ancient - 864_000_000_000), Nothing, Just before1900, Just start1900],
        [Just (ancientMillis * 1000 - 864_000_000_000), Nothing, Just (-1000), Just 0],
        [Just (ancient - 864_000_000_000), Nothing, Just before1900, Just start1900], original]])
    scan bytes [version "2.4.8"] >>= suffix "requires a known writer timezone"
    scan bytes [version "2.4.8", zone "No/Such_Zone"] >>= suffix "No/Such_Zone"
    let lastBce = -62_135_773_200_000_001
        firstCe = lastBce + 1
        edges convert = V.fromList [convert lastBce, convert firstCe]
        edgeColumns = V.fromList [edges time, edges (time . (`div` 1000)), edges int96, edges time]
        correction = 175_350_000_000
    edgeBytes <- either fail pure (PN.buildNestedFile fields edgeColumns)
    scan edgeBytes [version "3.5.8", amsterdam, legacy, legacy96] >>= (`shouldBe` Right
      [[[Just (lastBce + correction), Just (firstCe + correction)],
        [Just (firstCe + correction - 1000), Just (firstCe + correction)],
        [Just (lastBce + correction), Just (firstCe + correction)],
        [Just lastBce, Just firstCe]]])

    let bounds = group [("utc", leaf utcMicros), ("millis", leaf utcMillis), ("int96", leaf PN.LtInt96), ("local", leaf localMicros)]
        checkpointSchema = V.singleton ("add", group
          [ ("path", leaf PN.LtString), ("size", leaf PN.LtInt64), ("modificationTime", leaf PN.LtInt64)
          , ("dataChange", leaf PN.LtBool), ("stats_parsed", group [("minValues", bounds), ("maxValues", bounds)])
          ])
        row low high = struct
          [ PN.NRLeaf (PN.LvString "time.parquet"), time 1, time 0, PN.NRLeaf (PN.LvBool True)
          , struct [struct low, struct high]
          ]
        checkpoint low high entries = do
          checkpointBytes <- either fail pure (PN.buildNestedFile checkpointSchema (V.singleton (V.singleton (row low high))))
          marked <- patchFooter checkpointBytes entries
          [D.ActionAdd file] <- either fail pure (Checkpoint.decodeCheckpointFile marked)
          Just (Right stats) <- pure (D.decodeAddStats file)
          pure stats
    stats <- checkpoint
      [time ancient, time ancientMillis, int96 ancient, time ancient]
      [time start1900, time (start1900 `div` 1000), int96 start1900, time ancient]
      [version "3.5.8", amsterdam, legacy, legacy96]
    D.asMinValues stats `shouldBe` HM.fromList
      [ ("utc", Aeson.String "1582-01-01T08:42:30.123456Z")
      , ("millis", Aeson.String "1582-01-01T08:42:30.123Z")
      , ("int96", Aeson.String "1582-01-01T08:42:30.123456Z")
      , ("local", Aeson.String "1582-01-11 08:00:00.123456")
      ]
    D.asMaxValues stats `shouldBe` HM.fromList
      [ ("utc", Aeson.String "1900-01-01T00:59:59.999999Z")
      , ("millis", Aeson.String "1900-01-01T00:59:59.999999Z")
      , ("int96", Aeson.String "1900-01-01T00:59:59.999999Z")
      , ("local", Aeson.String "1582-01-11 08:00:00.123456")
      ]
    unknown <- checkpoint
      [time ancient, time ancientMillis, int96 ancient, time ancient]
      [time start1900, time (start1900 `div` 1000), int96 start1900, time ancient]
      [version "2.4.8"]
    D.asMinValues unknown `shouldBe` HM.singleton "local" (Aeson.String "1582-01-11 08:00:00.123456")
    D.asMaxValues unknown `shouldBe` D.asMinValues unknown


timestamp96ScanTest :: IO ()
timestamp96ScanTest =
  withSystemTempDirectory "wireform-delta-timestamp96" $ \root -> do
    let logDir = root </> "_delta_log"
        dataFile = root </> "time.parquet"
        protocol = "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"timestampNtz\"],\"writerFeatures\":[\"timestampNtz\"]}}\n"
        metadata = "{\"metaData\":{\"id\":\"table\",\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[{\\\"name\\\":\\\"utc\\\",\\\"type\\\":\\\"timestamp\\\",\\\"nullable\\\":true,\\\"metadata\\\":{}},{\\\"name\\\":\\\"local\\\",\\\"type\\\":\\\"timestamp_ntz\\\",\\\"nullable\\\":true,\\\"metadata\\\":{}}]}\"}}\n"
        add = "{\"add\":{\"path\":\"time.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n"
        makeFile localUnit localAdjusted values = do
          let field adjusted unit = PN.NSOptional (PN.NSPrimitive (PN.LtTimestamp96 adjusted unit))
              fields = V.fromList [("utc", field True PT.LtNanos), ("local", field localAdjusted localUnit)]
              cell Nothing = pure PN.NRNull
              cell (Just value) = PN.NRLeaf . PN.LvFixedBinary . TS.encodeTimestamp96 <$> either fail pure (TS.timestamp96FromInteger value)
          rows <- V.mapM (V.mapM cell . V.fromList) (V.fromList [map fst values, map snd values])
          bytes <- either fail pure (PN.buildNestedFile fields rows)
          BS.writeFile dataFile bytes
        scan = do
          table <- DIO.openDeltaTable root >>= either fail pure
          case Scan.scanTable table of
            Left err -> pure (Left err)
            Right (_, stream) -> fmap (map (map counts . V.toList . Scan.batchColumns)) <$> iterIOToList stream
        counts (AC.ColTimestamp validity values) =
          V.toList (V.generate (VS.length values) (\i -> if AC.isValidAt validity i then Just (values VS.! i) else Nothing))
        counts _ = []
        checkError message = scan >>= \case
          Left err -> err `shouldSatisfy` isInfixOf message
          Right _ -> expectationFailure "expected invalid extended timestamp scan to fail"
        smallest = toInteger (minBound :: Int64)
        largest = toInteger (maxBound :: Int64)
    createDirectory logDir
    BL.writeFile (logDir </> "00000000000000000000.json") (protocol <> metadata <> add)
    makeFile PT.LtMillis False [(Just (smallest * 1000), Just (-1)), (Nothing, Nothing), (Just (largest * 1000), Just 2)]
    scan >>= (\actual -> actual `shouldBe` Right [[[Just minBound, Nothing, Just maxBound], [Just (-1000), Nothing, Just 2000]]])
    makeFile PT.LtMicros False [(Just 0, Just smallest), (Just 0, Just largest)]
    scan >>= (\actual -> actual `shouldBe` Right [[[Just 0, Just 0], [Just minBound, Just maxBound]]])
    makeFile PT.LtMicros False [(Just (-1001), Nothing)]
    checkError "loses sub-unit precision"
    makeFile PT.LtMicros False [(Just ((largest + 1) * 1000), Nothing)]
    checkError "exceeds Arrow Int64 range"
    makeFile PT.LtMillis False [(Nothing, Just (smallest `quot` 1000 - 1))]
    checkError "exceeds Arrow Int64 range"
    makeFile PT.LtMicros True [(Nothing, Just 1)]
    checkError "incompatible timestamp"


timestampNtzFeatureTest :: IO ()
timestampNtzFeatureTest = do
  let schema = "{\"type\":\"struct\",\"fields\":[{\"name\":\"values\",\"type\":{\"type\":\"array\",\"elementType\":\"timestamp_ntz\",\"containsNull\":true},\"nullable\":true,\"metadata\":{}}]}"
      meta = D.MetaDataAction "table" Nothing Nothing Nothing schema [] mempty Nothing
      table protocol = DIO.DeltaTable "" Nothing Nothing [] snapshot (Just 0)
        where snapshot = D.emptySnapshot {D.tsMetaData = Just meta, D.tsProtocol = Just protocol}
      modern = D.ProtocolAction 3 7 [] []
      requireFeature result = case result of
        Left err -> err `shouldSatisfy` isInfixOf "timestampNtz reader feature"
        Right _ -> expectationFailure "timestamp_ntz accepted without its reader feature"
  requireFeature (Scan.scanTable (table modern))
  requireFeature (Scan.tableFileStats (table (D.ProtocolAction 1 2 [] [])))
  requireFeature (Scan.scanTable (table (modern {D.pReaderFeatures = ["timestampNtz"]})))
  requireFeature (Scan.scanTable (table (D.ProtocolAction 2 7 ["timestampNtz"] ["timestampNtz"])))
  mapM_ (\feature -> do
    let protocol = modern {D.pReaderFeatures = [feature], D.pWriterFeatures = [feature]}
    _ <- either fail pure (Scan.scanTable (table protocol))
    pure ()) ["timestampNtz", "timestampWithoutTimezone"]


scanChunkOrderTest :: IO ()
scanChunkOrderTest =
  withSystemTempDirectory "wireform-delta-chunk-order" $ \root -> do
    let path = root </> "part.parquet"
        leaf = PN.NSRequired (PN.NSPrimitive PN.LtInt64)
        fields = V.fromList [("id", leaf), ("amount", leaf)]
        columns = V.fromList [V.singleton (PN.NRLeaf (PN.LvInt64 7)), V.singleton (PN.NRLeaf (PN.LvInt64 900))]
        scan = do
          opened <- Scan.openDataFile root (D.tsFiles snapshot Map.! "part.parquet")
          pure $ do
            (_, batches) <- opened
            map (map firstValue . V.toList . Scan.batchColumns) <$> iterToList batches
        snapshot = D.snapshotFromActions (D.parseLogFile "{\"add\":{\"path\":\"part.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true}}\n")
        firstValue (AC.ColInt64 _ values) = Just (VS.head values)
        firstValue _ = Nothing
    bytes <- either fail pure (PN.buildNestedFile fields columns)
    file <- either fail pure (PR.loadParquetFile bytes)
    trailer <- either fail pure (PF.readFooterTrailer bytes)
    let footer = PR.pfFooter file
        dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
        swap group = group {PT.rgColumns = V.reverse (PT.rgColumns group)}
        reordered = footer {PT.fmRowGroups = V.map swap (PT.fmRowGroups footer)}
    BS.writeFile path bytes
    scan >>= (\result -> result `shouldBe` Right [[Just 7, Just 900]])
    BS.writeFile path (BS.take dataEnd bytes <> PF.writeFooter reordered)
    scan >>= \case
      Left err -> err `shouldSatisfy` isInfixOf "column chunk path differs from schema"
      Right values -> expectationFailure ("accepted reordered column chunks: " ++ show values)


scanEscapedFilePathTest :: IO ()
scanEscapedFilePathTest =
  withSystemTempDirectory "wireform-delta-escaped-path" $ \root -> do
    let escapedName = "a%5Cb%3F%23%252F.parquet"
        physicalName = "a\\b?#%2F.parquet"
        fields = V.singleton ("id", PN.NSRequired (PN.NSPrimitive PN.LtInt64))
        row = V.singleton (V.singleton (PN.NRLeaf (PN.LvInt64 42)))
        add uri = D.AddAction uri 1 0 True Nothing Nothing Map.empty Map.empty Nothing Nothing Nothing
        scan uri = Scan.openDataFile root (add uri)
        reject uri = scan uri >>= \case
          Left err -> err `shouldSatisfy` isInfixOf "Delta.Path:"
          Right _ -> expectationFailure ("accepted invalid path " ++ T.unpack uri)
    if pathSeparator == '\\'
      then mapM_ reject [escapedName, "C%3A/path.parquet"]
      else do
        bytes <- either fail pure (PN.buildNestedFile fields row)
        BS.writeFile (root </> physicalName) bytes
        canonical <- canonicalizePath root
        forM_ [escapedName, "file://" <> T.pack canonical <> "/" <> escapedName] $ \uri -> do
          (_, stream) <- scan uri >>= either fail pure
          batches <- either fail pure (iterToList stream)
          case batches of
            [batch] | [AC.ColInt64 Nothing values] <- V.toList (Scan.batchColumns batch) ->
              values `shouldBe` VS.singleton 42
            _ -> expectationFailure "expected one decoded Parquet row"
        -- Both files exist, so treating a URI delimiter as a literal
        -- filename character would silently open the wrong file.
        forM_ ["a?query.parquet", "a#fragment.parquet"] $ \name -> do
          BS.writeFile (root </> name) bytes
          reject (T.pack name)
          reject (T.pack (canonical </> name))
    mapM_ reject ["a\\b.parquet", "%2e%2e/%2Foutside", "%2Fetc/passwd", "x%00.parquet", "x%GG.parquet"]


scanKeyOnlyMapTest :: IO ()
scanKeyOnlyMapTest =
  withSystemTempDirectory "wireform-delta-key-only-map" $ \root -> do
    let path = root </> "keys.parquet"
        leaf = PN.NSRequired (PN.NSPrimitive PN.LtInt64)
        fields = V.singleton ("lookup", PN.NSOptional (PN.NSList leaf))
        key = PN.NRLeaf . PN.LvInt64
        rows = V.singleton $ V.fromList
          [PN.NRNull, PN.NRList V.empty, PN.NRList (V.fromList [key 3, key 9])]
    bytes <- either fail pure (PN.buildNestedFile fields rows)
    file <- either fail pure (PR.loadParquetFile bytes)
    trailer <- either fail pure (PF.readFooterTrailer bytes)
    let footer = PR.pfFooter file
        dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
        elements = PT.fmSchema footer
        annotate = (elements V.! 1) {PT.seLogicalType = Just PT.LTMap, PT.seConvertedType = Just PT.CTMap}
        entries = (elements V.! 2) {PT.seName = "key_value"}
        keyField = (elements V.! 3) {PT.seName = "key"}
        renameChunk chunk = chunk
          {PT.ccMetadata = fmap (\metadata -> metadata {PT.cmPathInSchema = V.fromList ["lookup", "key_value", "key"]}) (PT.ccMetadata chunk)}
        mapFooter = footer
          { PT.fmSchema = elements V.// [(1, annotate), (2, entries), (3, keyField)]
          , PT.fmRowGroups = V.map (\group -> group {PT.rgColumns = V.map renameChunk (PT.rgColumns group)}) (PT.fmRowGroups footer)
          }
        table allowsNulls =
          let schema = TE.decodeUtf8 . BL.toStrict . Aeson.encode $ Aeson.object
                [ "type" Aeson..= ("struct" :: T.Text)
                , "fields" Aeson..= [Aeson.object
                    [ "name" Aeson..= ("lookup" :: T.Text)
                    , "nullable" Aeson..= True
                    , "metadata" Aeson..= Aeson.object []
                    , "type" Aeson..= Aeson.object
                        [ "type" Aeson..= ("map" :: T.Text)
                        , "keyType" Aeson..= ("long" :: T.Text)
                        , "valueType" Aeson..= ("string" :: T.Text)
                        , "valueContainsNull" Aeson..= allowsNulls
                        ]
                    ]]
                ]
              metadata = D.MetaDataAction "table" Nothing Nothing Nothing schema [] mempty Nothing
              add = D.AddAction "keys.parquet" 1 0 True Nothing Nothing mempty mempty Nothing Nothing Nothing
              snapshot = D.snapshotFromActions [D.ActionMetaData metadata, D.ActionAdd add]
          in DIO.DeltaTable root Nothing Nothing [] snapshot (Just 0)
        scan allowsNulls = do
          (_, stream) <- either fail pure (Scan.scanTable (table (allowsNulls :: Bool)))
          iterIOToList stream
        reject reason allowsNulls = scan allowsNulls >>= \case
          Left err -> err `shouldSatisfy` isInfixOf reason
          Right _ -> expectationFailure "accepted incompatible map storage"
    BS.writeFile path bytes
    reject "incompatible physical type" True
    BS.writeFile path (BS.take dataEnd bytes <> PF.writeFooter mapFooter)
    batches <- scan True >>= either fail pure
    case batches of
      [batch]
        | [AC.ColMap valid offsets (AC.ColInt64 Nothing keys) values@AC.ColUtf8 {}] <- V.toList (Scan.batchColumns batch) -> do
            map (AC.isValidAt valid) [0, 1, 2] `shouldBe` [False, True, True]
            offsets `shouldBe` VS.fromList [0, 0, 0, 2]
            keys `shouldBe` VS.fromList [3, 9]
            AC.columnLength values `shouldBe` 2
            AC.nullCount values `shouldBe` 2
      _ -> expectationFailure "expected three map rows with typed null values"
    reject "map has no physical values for a non-nullable value type" False


legacyPartitionOffsetTest :: IO ()
legacyPartitionOffsetTest =
  withSystemTempDirectory "wireform-delta-partition-offset" $ \root -> do
    let schema = V.singleton (PT.SchemaElement "schema" Nothing Nothing (Just 0) Nothing Nothing Nothing Nothing)
        footer = PT.FileMetadata 2 schema 1 (V.singleton (PT.RowGroup V.empty 0 1 Nothing)) Nothing Nothing Nothing
        bytes = PF.parquetMagic <> PF.writeFooter footer
        partitionValues = Map.fromList . zip ["utc", "local"]
        utcText = Just "1970-01-01 01:00:00.123456"
        localText = Just "1970-01-01 00:00:00.999999"
        makeFile name utcValue = D.AddAction name (fromIntegral (BS.length bytes)) 0 True Nothing Nothing
          (partitionValues [utcValue, localText]) Map.empty Nothing (Just 100) (Just 9)
        first = makeFile "a.parquet" utcText
        second = makeFile "b.parquet" (Just "1970-01-01T01:00:00.123456+01:00")
        missing = first {D.addPartitionValues = Map.delete "utc" (D.addPartitionValues first)}
        nullFile = makeFile "a.parquet" Nothing
        metadata = D.MetaDataAction "table" Nothing Nothing Nothing
          "{\"type\":\"struct\",\"fields\":[{\"name\":\"utc\",\"type\":\"timestamp\",\"nullable\":true,\"metadata\":{}},{\"name\":\"local\",\"type\":\"timestamp_ntz\",\"nullable\":true,\"metadata\":{}}]}"
          ["utc", "local"] (Map.fromList
            [ ("delta.enableRowTracking", "true")
            , ("delta.rowTracking.materializedRowIdColumnName", "__stable_id")
            , ("delta.rowTracking.materializedRowCommitVersionColumnName", "__stable_version")
            ]) Nothing
        protocol = D.ProtocolAction 3 7 ["timestampNtz"] ["timestampNtz", "rowTracking", "domainMetadata"]
        table files = DIO.DeltaTable root Nothing Nothing []
          (D.snapshotFromActions (D.ActionMetaData metadata : D.ActionProtocol protocol : map D.ActionAdd files)) (Just 0)
        resolve add local
          | D.addPath add == "a.parquet"
          , local == LocalTime (fromGregorian 1970 1 1) (TimeOfDay 1 0 0.123456) = Just 3628
          | otherwise = Nothing
        timestamps = map cell . V.toList . Scan.batchColumns
        cell (AC.ColTimestamp valid values)
          | AC.isValidAt valid 0 = Just (VS.head values)
        cell _ = Nothing
        scan run files = do
          (_, batches) <- either fail pure (run (table files))
          fmap (map timestamps) <$> iterIOToList batches
        checkError fragment result = case result of
          Left err -> err `shouldSatisfy` isInfixOf fragment
          Right _ -> expectationFailure ("accepted partition without " ++ fragment)
    mapM_ (\name -> BS.writeFile (root </> name) bytes) ["a.parquet", "b.parquet"]
    scan Scan.scanTable [first] `shouldReturn` Right [[Just 3600123456, Just 999999]]
    let scanWithOffset = Scan.scanTableWithPartitionOffset resolve
        expected = [[Just (-27876544), Just 999999], [Just 123456, Just 999999]]
    scan scanWithOffset [first, second] `shouldReturn` Right expected
    scan scanWithOffset [nullFile] `shouldReturn` Right [[Nothing, Just 999999]]
    scan scanWithOffset [missing] >>= checkError "missing partition value"
    scan (Scan.scanTableWithPartitionOffset (\_ _ -> Nothing)) [first] >>= checkError "invalid partition value"
    scan (Scan.scanTableWithPartitionOffset (\_ _ -> Just 86400)) [first] >>= checkError "invalid partition value"
    (_, tracked) <- either fail pure (Scan.scanTrackedTableWithPartitionOffset resolve (table [first, second]))
    batches <- iterIOToList tracked >>= either fail pure
    map (timestamps . Scan.trackedData) batches `shouldBe` expected
    concatMap (VS.toList . Scan.rowCommitVersions) batches `shouldBe` [9, 9]


scanColumnlessFileTest :: IO ()
scanColumnlessFileTest =
  withSystemTempDirectory "wireform-delta-columnless" $ \root -> do
    let path = root </> "part.parquet"
        rows = 65_540
        schema = V.singleton (PT.SchemaElement "schema" Nothing Nothing (Just 0) Nothing Nothing Nothing Nothing)
        rowGroup n = PT.RowGroup V.empty 0 n Nothing
        footer = PT.FileMetadata 2 schema rows (V.fromList [rowGroup 0, rowGroup rows]) Nothing Nothing Nothing
        bytes = PF.parquetMagic <> PF.writeFooter footer
        field name ty ident physical = Aeson.object
          [ "name" Aeson..= (name :: T.Text)
          , "type" Aeson..= (ty :: T.Text)
          , "nullable" Aeson..= True
          , "metadata" Aeson..= Aeson.object
              [ "delta.columnMapping.id" Aeson..= (ident :: Int)
              , "delta.columnMapping.physicalName" Aeson..= (physical :: T.Text)
              ]
          ]
        definition = Aeson.object
          [ "type" Aeson..= ("struct" :: T.Text)
          , "fields" Aeson..= [field "region" "string" 1 "col_region", field "new" "long" 2 "col_new"]
          ]
        meta = D.MetaDataAction "table" Nothing Nothing Nothing (TE.decodeUtf8 (BL.toStrict (Aeson.encode definition)))
          ["region"] (Map.singleton "delta.columnMapping.mode" "id") Nothing
        dv = Aeson.object
          [ "storageType" Aeson..= ("i" :: T.Text)
          , "pathOrInlineDv" Aeson..= ("wi5b=000010000siXQKl0rr91000f55c8Xg0@@D72lkbi5=-{L" :: T.Text)
          , "sizeInBytes" Aeson..= (40 :: Int)
          , "cardinality" Aeson..= (6 :: Int)
          ]
        file = D.AddAction "part.parquet" (fromIntegral (BS.length bytes)) 0 True
          (Just "{\"numRecords\":65540}") Nothing (Map.singleton "col_region" (Just "eu")) Map.empty (Just dv) (Just 100) (Just 7)
        features = ["columnMapping", "deletionVectors"]
        protocol = D.ProtocolAction 3 7 features (features ++ ["rowTracking", "domainMetadata"])
        snapshot = D.emptySnapshot {D.tsMetaData = Just meta, D.tsProtocol = Just protocol, D.tsFiles = Map.singleton "part.parquet" file}
        table = DIO.DeltaTable root Nothing Nothing [] snapshot (Just 0)
    BS.writeFile path bytes
    (logical, batches) <- either fail pure (Scan.scanTable table)
    V.toList (AT.fieldName <$> AT.arrowFields logical) `shouldBe` ["region", "new"]
    output <- iterIOToList batches >>= either fail pure
    map Scan.batchRowCount output `shouldBe` [65_530, 4]
    forM_ output $ \batch -> case V.toList (Scan.batchColumns batch) of
      [region, absent] -> do
        let n = Scan.batchRowCount batch
        AC.columnLength region `shouldBe` n
        AC.anyTextAt region 0 `shouldBe` Just "eu"
        AC.anyTextAt region (n - 1) `shouldBe` Just "eu"
        AC.columnLength absent `shouldBe` n
        AC.nullCount absent `shouldBe` n
      _ -> expectationFailure "expected a partition column and a synthesized null column"
    let trackedMeta = meta {D.mdConfiguration = Map.union (D.mdConfiguration meta) (Map.fromList
          [ ("delta.enableRowTracking", "true")
          , ("delta.rowTracking.materializedRowIdColumnName", "__stable_id")
          , ("delta.rowTracking.materializedRowCommitVersionColumnName", "__stable_version")
          ])}
        trackedTable = table {DIO.dtSnapshot = snapshot {D.tsMetaData = Just trackedMeta}}
    (_, tracked) <- either fail pure (Scan.scanTrackedTable trackedTable)
    trackedOutput <- iterIOToList tracked >>= either fail pure
    map (Scan.batchRowCount . Scan.trackedData) trackedOutput `shouldBe` [65_530, 4]
    let expected = map (+ 100) (filter (`notElem` [3, 4, 7, 11, 18, 29]) [0 .. rows - 1])
    concatMap (VS.toList . Scan.rowIds) trackedOutput `shouldBe` expected
    concatMap (VS.toList . Scan.rowCommitVersions) trackedOutput `shouldBe` replicate (length expected) 7
    let rejectVectorProtocol invalidProtocol = do
          let invalid = trackedTable {DIO.dtSnapshot = (DIO.dtSnapshot trackedTable) {D.tsProtocol = Just invalidProtocol}}
              reject result = case result of
                Left err -> err `shouldSatisfy` isInfixOf "deletionVectors"
                Right _ -> expectationFailure "accepted an active deletion vector without the required protocol"
          (_, regular) <- either fail pure (Scan.scanTable invalid)
          iterIOToList regular >>= reject
          (_, trackedInvalid) <- either fail pure (Scan.scanTrackedTable invalid)
          iterIOToList trackedInvalid >>= reject
          stats <- either fail pure (Scan.tableFileStats invalid)
          reject (stats file)
    mapM_ rejectVectorProtocol
      [ protocol {D.pReaderFeatures = ["columnMapping"]}
      , protocol {D.pWriterFeatures = filter (/= "deletionVectors") (D.pWriterFeatures protocol)}
      , protocol {D.pMinReaderVersion = 2}
      ]
    (_, physical) <- Scan.openDataFile root file >>= either fail pure
    physicalOutput <- either fail pure (iterToList physical)
    map Scan.batchRowCount physicalOutput `shouldBe` [65_530, 4]
    all (V.null . Scan.batchColumns) physicalOutput `shouldBe` True
    -- A consumer taking one batch must not materialize the claimed group.
    let hugeRows = (maxBound :: Int64) `div` 2
        enormous = footer {PT.fmNumRows = hugeRows, PT.fmRowGroups = V.singleton (rowGroup hugeRows)}
        withoutDv = file {D.addDeletionVector = Nothing}
        hugeTable = table {DIO.dtSnapshot = snapshot {D.tsFiles = Map.singleton "part.parquet" withoutDv}}
    BS.writeFile path (PF.parquetMagic <> PF.writeFooter enormous)
    (_, limited) <- either fail pure (Scan.scanTable hugeTable)
    first <- iterIOToList (iterIOTake 1 limited) >>= either fail pure
    map Scan.batchRowCount first `shouldBe` [65_536]


scanProjectionTest :: IO ()
scanProjectionTest = withSystemTempDirectory "wireform-delta-projection" $ \root -> do
  let path = root </> "part.parquet"
      leaf = PN.NSRequired (PN.NSPrimitive PN.LtInt64)
      fields = V.fromList [("id", leaf), ("unselected", leaf)]
      values = V.fromList . map (PN.NRLeaf . PN.LvInt64)
      columns = V.fromList [values [7, 9], values [100, 200]]
      schema = "{\"type\":\"struct\",\"fields\":[{\"name\":\"id\",\"type\":\"long\",\"nullable\":true,\"metadata\":{}},{\"name\":\"unselected\",\"type\":\"long\",\"nullable\":true,\"metadata\":{}},{\"name\":\"region\",\"type\":\"string\",\"nullable\":true,\"metadata\":{}},{\"name\":\"added\",\"type\":\"long\",\"nullable\":true,\"metadata\":{}}]}"
      meta = D.MetaDataAction "table" Nothing Nothing Nothing schema ["region"] mempty Nothing
      file = D.AddAction "part.parquet" 1 0 True Nothing Nothing (Map.singleton "region" (Just "eu")) mempty Nothing Nothing Nothing
      snap = D.snapshotFromActions [D.ActionMetaData meta, D.ActionAdd file]
      table = DIO.DeltaTable root Nothing Nothing [] snap (Just 0)
      run scan = do
        (output, stream) <- either fail pure scan
        results <- iterIOToList stream
        pure (V.toList (AT.fieldName <$> AT.arrowFields output), results)
  original <- either fail pure (PN.buildNestedFile fields columns)
  parquet <- either fail pure (PR.loadParquetFile original)
  let chunk = PT.rgColumns (PT.fmRowGroups (PR.pfFooter parquet) V.! 0) V.! 1
  metadata <- maybe (fail "missing fixture column metadata") pure (PT.ccMetadata chunk)
  let offset = fromIntegral (PT.cmDataPageOffset metadata)
  -- An invalid Compact Protocol field header in the unselected page.
  BS.writeFile path (BS.take offset original <> BS.replicate 8 255 <> BS.drop (offset + 8) original)
  (_, everything) <- run (Scan.scanTable table)
  case everything of
    Left err -> err `shouldSatisfy` isInfixOf "unselected"
    Right _ -> expectationFailure "the full scan decoded the corrupted Parquet page"
  (names, selected) <- run (Scan.scanTableProjected ["region", "added", "id"] table)
  names `shouldBe` ["region", "added", "id"]
  [batch] <- either fail pure selected
  Scan.batchRowCount batch `shouldBe` 2
  case V.toList (Scan.batchColumns batch) of
    [region, added, AC.ColInt64 Nothing ids] -> do
      map (AC.anyTextAt region) [0, 1] `shouldBe` [Just "eu", Just "eu"]
      AC.nullCount added `shouldBe` 2
      ids `shouldBe` VS.fromList [7, 9]
    _ -> expectationFailure "unexpected projected columns"
  (empty, counted) <- run (Scan.scanTableProjected [] table)
  empty `shouldBe` []
  [count] <- either fail pure counted
  Scan.batchRowCount count `shouldBe` 2
  V.null (Scan.batchColumns count) `shouldBe` True
  forM_ [(["missing"], "missing from the snapshot"), (["id", "id"], "duplicate column")] $ \(names', message) ->
    case Scan.scanTableProjected names' table of
      Left err -> err `shouldSatisfy` isInfixOf message
      Right _ -> expectationFailure "an invalid projection was accepted"


scanPredicateTest :: IO ()
scanPredicateTest = withSystemTempDirectory "wireform-delta-predicate" $ \root -> do
  let leaf kind = PN.NSOptional (PN.NSPrimitive kind)
      fields = V.fromList [("id", leaf PN.LtInt64), ("score", leaf PN.LtInt64), ("label", leaf PN.LtString)]
      ints = V.fromList . map (maybe PN.NRNull (PN.NRLeaf . PN.LvInt64))
      labels = V.fromList . map (PN.NRLeaf . PN.LvString)
      columns = V.fromList
        [ ints (map Just [1 .. 6]), ints [Nothing, Just 1, Just 3, Just 1, Just 5, Just 8]
        , labels ["_a", "a", "a", "🙂b", "🙂b", "🙂bb"]
        ]
      schema = "{\"type\":\"struct\",\"fields\":[{\"name\":\"id\",\"type\":\"long\",\"nullable\":true,\"metadata\":{}},{\"name\":\"score\",\"type\":\"long\",\"nullable\":true,\"metadata\":{}},{\"name\":\"label\",\"type\":\"string\",\"nullable\":true,\"metadata\":{}}]}"
      meta = D.MetaDataAction "table" Nothing Nothing Nothing schema [] mempty Nothing
      add name bounds dv = D.AddAction name 1 0 True (Just bounds) Nothing mempty mempty dv Nothing Nothing
      stats = "{\"numRecords\":6,\"minValues\":{\"score\":1},\"maxValues\":{\"score\":8},\"tightBounds\":false}"
      missing = add "missing.parquet" "{\"numRecords\":2,\"minValues\":{\"score\":-9},\"maxValues\":{\"score\":-1}}" Nothing
      protocol = D.ProtocolAction 3 7 ["deletionVectors"] ["deletionVectors"]
      table files = DIO.DeltaTable root Nothing Nothing [] (D.snapshotFromActions
        ([D.ActionProtocol protocol, D.ActionMetaData meta] ++ map D.ActionAdd files)) (Just 0)
      score = Predicate.Column "score" []
      value = Predicate.PVInt64
      gt x = score (Predicate.Compare Predicate.Greater (value x))
      readIds snapshot condition = do
        (output, stream) <- either fail pure (Scan.scanTableFiltered (Just ["id"]) condition snapshot)
        (AT.fieldName <$> AT.arrowFields output) `shouldBe` V.singleton "id"
        batches <- iterIOToList stream >>= either fail pure
        fmap concat $ traverse (\batch -> case V.toList (Scan.batchColumns batch) of
          [AC.ColInt64 _ ids] -> pure (VS.toList ids)
          _ -> fail "unexpected filter output") batches
  bytes <- either fail pure (PN.buildNestedFile fields columns)
  BS.writeFile (root </> "live.parquet") bytes
  -- A single portable Roaring array container deletes row position 3.
  let bitmap = BL.toStrict (BB.toLazyByteString (foldMap id
        [ BB.word32LE 1681511377, BB.word64LE 1, BB.word32LE 0, BB.word32LE 12346
        , BB.word32LE 1, BB.word16LE 0, BB.word16LE 0, BB.word32LE 16, BB.word16LE 3
        ]))
      -- Encode the portable bitmap as a stored DV with the first record at offset one.
      vectorFile = root </> "vector.bin"
  canonicalRoot <- canonicalizePath root
  let testFile = add "live.parquet" stats (Just (Aeson.object
        [ "storageType" Aeson..= ("p" :: T.Text)
        , "pathOrInlineDv" Aeson..= ("file://" <> T.pack (canonicalRoot </> "vector.bin"))
        , "offset" Aeson..= (1 :: Int)
        , "sizeInBytes" Aeson..= BS.length bitmap
        , "cardinality" Aeson..= (1 :: Int)
        ]))
      live = table [testFile]
  BS.writeFile vectorFile (BL.toStrict (BB.toLazyByteString
    (BB.word8 1 <> BB.word32BE (fromIntegral (BS.length bitmap)) <> BB.byteString bitmap <> BB.word32BE 0xdc8be776)))
  readIds (table [missing, testFile]) (gt 2) >>= (`shouldBe` [3, 5, 6])
  readIds live (Predicate.Not (gt 2)) >>= (`shouldBe` [2])
  readIds live (Predicate.Not (score (Predicate.In [Just (value 1), Nothing]))) >>= (`shouldBe` [])
  readIds live (Predicate.Not (score (Predicate.NullSafeEqual (Just (value 1))))) >>= (`shouldBe` [1, 3, 5, 6])
  readIds live (Predicate.Column "label" [] (Predicate.Like "_b")) >>= (`shouldBe` [5])
  readIds live (Predicate.Column "label" [] (Predicate.Like "\\_a")) >>= (`shouldBe` [1])
  readIds live (Predicate.Column "label" [] (Predicate.StringLength Predicate.Equal 2)) >>= (`shouldBe` [1, 5])
  let filterNone = Scan.scanTableFiltered Nothing (gt 10) (table [missing, testFile])
  (_, none) <- either fail pure filterNone
  iterIOToList none >>= either fail (\batches -> length batches `shouldBe` 0)
  (_, noColumns) <- either fail pure (Scan.scanTableFiltered (Just []) (gt 2) live)
  [rows] <- iterIOToList noColumns >>= either fail pure
  Scan.batchRowCount rows `shouldBe` 3
  V.null (Scan.batchColumns rows) `shouldBe` True
  let nullOnly = missing {D.addStats = Just "{\"numRecords\":2,\"nullCount\":{\"score\":2},\"tightBounds\":false}"}
      noNulls = missing {D.addStats = Just "{\"numRecords\":2,\"nullCount\":{\"score\":0}}"}
      partial = missing {D.addStats = Just "{\"numRecords\":2,\"nullCount\":{\"score\":1},\"tightBounds\":false}"}
      isNull = score Predicate.IsNull
  readIds (table [nullOnly]) (Predicate.Not isNull) >>= (`shouldBe` [])
  readIds (table [noNulls]) isNull >>= (`shouldBe` [])
  (_, dataNeeded) <- either fail pure (Scan.scanTableFiltered (Just ["id"]) isNull (table [partial]))
  iterIOToList dataNeeded >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "missing.parquet"
    Right _ -> expectationFailure "a wide intermediate null count pruned the file"
  let onlyMin = missing {D.addStats = Just "{\"minValues\":{\"score\":-2}}"}
      onlyMax = missing {D.addStats = Just "{\"maxValues\":{\"score\":-2}}"}
      invalidMin = missing {D.addStats = Just "{\"minValues\":{\"score\":\"invalid\"},\"maxValues\":{\"score\":-2}}"}
      reversed = missing {D.addStats = Just "{\"minValues\":{\"score\":-1},\"maxValues\":{\"score\":-2}}"}
      compareScore op x = score (Predicate.Compare op (value x))
      needsFile file condition = do
        (_, candidateBatches) <- either fail pure (Scan.scanTableFiltered (Just ["id"]) condition (table [file]))
        iterIOToList candidateBatches >>= \case
          Left err -> err `shouldSatisfy` isInfixOf "missing.parquet"
          Right _ -> expectationFailure "skipped a file whose bounds could not rule out the predicate"
  forM_ [(onlyMin, Predicate.Less), (onlyMax, Predicate.Greater), (invalidMin, Predicate.Greater)] $ \(file, op) ->
    readIds (table [file]) (compareScore op (-2)) `shouldReturn` []
  readIds (table [onlyMax]) (compareScore Predicate.Equal (-1)) `shouldReturn` []
  readIds (table [onlyMin]) (score (Predicate.In [Just (value (-3)), Nothing])) `shouldReturn` []
  let labelMin = missing {D.addStats = Just "{\"minValues\":{\"label\":\"🙂b\"},\"maxValues\":{\"label\":null}}"}
      beforeLabel = Predicate.Column "label" [] (Predicate.Compare Predicate.Less (Predicate.PVText "🙂a"))
  readIds (table [labelMin]) beforeLabel `shouldReturn` []
  needsFile onlyMax (compareScore Predicate.GreaterEqual (-2))
  needsFile onlyMin (compareScore Predicate.LessEqual (-2))
  needsFile onlyMax (compareScore Predicate.NotEqual (-2))
  needsFile onlyMin (compareScore Predicate.NotEqual (-2))
  needsFile reversed (compareScore Predicate.Greater (-2))
  let noStats = missing {D.addStats = Nothing}
      inSet = score . Predicate.In
      notInWithNull = Predicate.Not (inSet [Just (value 1), Nothing])
      fixedScore = missing {D.addStats = Just "{\"minValues\":{\"score\":-2},\"maxValues\":{\"score\":-2}}"}
  readIds (table [noStats]) notInWithNull `shouldReturn` []
  readIds (table [noStats]) (inSet [Nothing]) `shouldReturn` []
  readIds (table [noStats]) (Predicate.Not (Predicate.Or (inSet [Nothing]) (Predicate.Always False))) `shouldReturn` []
  readIds (table [noStats, testFile]) (Predicate.And notInWithNull (gt 10)) `shouldReturn` []
  readIds (table [fixedScore, testFile]) (Predicate.Or notInWithNull (gt 10)) `shouldReturn` []
  readIds live (Predicate.Or notInWithNull (gt 2)) `shouldReturn` [3, 5, 6]
  readIds live (Predicate.Not notInWithNull) `shouldReturn` [2]
  readIds (table [fixedScore]) (Predicate.Not (inSet [Just (value (-2)), Just (value 5)])) `shouldReturn` []
  needsFile onlyMax (Predicate.Not (inSet [Just (value (-2))]))
  needsFile missing (Predicate.Not (inSet [Just (value (-1)), Just (value (-9))]))


scanTemporalPruningTest :: IO ()
scanTemporalPruningTest = withSystemTempDirectory "wireform-delta-temporal-pruning" $ \root -> do
  let leaf = PN.NSRequired . PN.NSPrimitive
      group = PN.NSStruct . V.fromList
      row = PN.NRStruct . V.fromList
      value = PN.NRLeaf
      fields =
        [ ("day", "date", PN.LtDate, PN.LvInt32 0)
        , ("stamp", "timestamp", PN.LtTimestamp True PT.LtMicros, PN.LvInt64 123000)
        , ("local", "timestamp_ntz", PN.LtTimestamp False PT.LtMicros, PN.LvInt64 123000)
        ]
      schemaField (name, kind, _, _) = Aeson.object
        [ "name" Aeson..= name, "type" Aeson..= (kind :: T.Text)
        , "nullable" Aeson..= True, "metadata" Aeson..= Aeson.object []
        ]
      schema = TE.decodeUtf8 . BL.toStrict . Aeson.encode $
        Aeson.object ["type" Aeson..= ("struct" :: T.Text), "fields" Aeson..= map schemaField fields]
      metadata = D.MetaDataAction "table" Nothing Nothing Nothing schema [] mempty Nothing
      protocol = D.ProtocolAction 3 7 ["timestampNtz"] ["timestampNtz"]
      table files = DIO.DeltaTable root Nothing Nothing []
        (D.snapshotFromActions (D.ActionProtocol protocol : D.ActionMetaData metadata : map D.ActionAdd files)) (Just 0)
      temporalFields = map (\(name, _, ty, _) -> (name, leaf ty)) fields
      bounds = group temporalFields
      boundRow = row (map (\(_, _, _, x) -> value x) fields)
      checkpointSchema = V.singleton ("add", group
        [ ("path", leaf PN.LtString), ("size", leaf PN.LtInt64), ("modificationTime", leaf PN.LtInt64)
        , ("dataChange", leaf PN.LtBool), ("stats_parsed", group [("minValues", bounds), ("maxValues", bounds)])
        ])
      checkpointRow = row
        [value (PN.LvString "missing.parquet"), value (PN.LvInt64 1), value (PN.LvInt64 0),
         value (PN.LvBool False), row [boundRow, boundRow]]
      scan files condition = do
        (_, batches) <- either fail pure (Scan.scanTableFiltered (Just []) condition (table files))
        fmap (sum . map Scan.batchRowCount) <$> iterIOToList batches
      gt name = Predicate.Column name [] . Predicate.Compare Predicate.Greater
      eq name = Predicate.Column name [] . Predicate.Compare Predicate.Equal
      stats = "{\"minValues\":{\"day\":\"1970-01-01\",\"stamp\":\"1969-12-31T23:00:00.123-01:00\",\"local\":\"1970-01-01 00:00:00.123\"},\"maxValues\":{\"day\":\"1970-01-01\",\"stamp\":\"1969-12-31T23:00:00.123-01:00\",\"local\":\"1970-01-01 00:00:00.123\"}}"
  checkpoint <- either fail pure (PN.buildNestedFile checkpointSchema (V.singleton (V.singleton checkpointRow)))
  [D.ActionAdd structured] <- either fail pure (Checkpoint.decodeCheckpointFile checkpoint)
  let json = structured {D.addStats = Just stats, D.addParsedStats = Nothing}
  forM_ [json, structured] $ \file -> do
    scan [file] (gt "day" (Predicate.PVText "1970-01-01")) `shouldReturn` Right 0
    scan [file] (gt "stamp" (Predicate.PVInt64 124000)) `shouldReturn` Right 0
    scan [file] (gt "local" (Predicate.PVInt64 124000)) `shouldReturn` Right 0
  Just (Right decoded) <- pure (D.decodeAddStats structured)
  let checkpointMaximum = structured {D.addParsedStats = Just decoded {D.asMinValues = mempty}}
  scan [checkpointMaximum] (gt "stamp" (Predicate.PVInt64 124000)) `shouldReturn` Right 0
  let unknownOffset = json {D.addStats = Just
        "{\"minValues\":{\"stamp\":\"1970-01-01 00:00:00.123\"},\"maxValues\":{\"stamp\":\"1970-01-01 00:00:00.123\"}}"}
  scan [unknownOffset] (gt "stamp" (Predicate.PVInt64 124000)) >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "missing.parquet"
    Right _ -> expectationFailure "skipped a file using timestamp bounds without an offset"
  let columns = V.fromList
        [ V.singleton (value (PN.LvInt32 0)), V.singleton (value (PN.LvInt64 123789))
        , V.singleton (value (PN.LvInt64 123789))
        ]
  bytes <- either fail pure (PN.buildNestedFile (V.fromList temporalFields) columns)
  BS.writeFile (root </> "live.parquet") bytes
  forM_ [json, structured] $ \file -> do
    let live = file {D.addPath = "live.parquet"}
    scan [live] (eq "day" (Predicate.PVInt64 0)) `shouldReturn` Right 1
    scan [live] (eq "stamp" (Predicate.PVInt64 123789)) `shouldReturn` Right 1
    scan [live] (eq "local" (Predicate.PVInt64 123789)) `shouldReturn` Right 1


scanPartitionPruningTest :: IO ()
scanPartitionPruningTest = withSystemTempDirectory "wireform-delta-prune-partitions" $ \root -> do
  let schema = "{\"type\":\"struct\",\"fields\":[{\"name\":\"id\",\"type\":\"long\",\"nullable\":true,\"metadata\":{\"delta.columnMapping.id\":1,\"delta.columnMapping.physicalName\":\"col_id\"}},{\"name\":\"region\",\"type\":\"string\",\"nullable\":true,\"metadata\":{\"delta.columnMapping.id\":2,\"delta.columnMapping.physicalName\":\"col_region\"}}]}"
      protocol = D.ProtocolAction 2 5 [] []
      meta = D.MetaDataAction "table" Nothing Nothing Nothing schema ["region"] (Map.singleton "delta.columnMapping.mode" "name") Nothing
      add name region = D.ActionAdd (D.AddAction name 1 0 True Nothing Nothing
        (Map.singleton "col_region" region) mempty Nothing Nothing Nothing)
      table actions = DIO.DeltaTable root Nothing Nothing []
        (D.snapshotFromActions (D.ActionProtocol protocol : D.ActionMetaData meta : actions)) (Just 0)
      mismatched = add "missing-europe.parquet" (Just "eu")
      nullRegion = add "missing-null.parquet" Nothing
      matching = add "asia.parquet" (Just "asia")
      withUpperBound (D.ActionAdd file) =
        D.ActionAdd file {D.addStats = Just "{\"maxValues\":{\"col_id\":1}}"}
      withUpperBound action = action
      isAsia = Predicate.Column "region" [] (Predicate.Compare Predicate.Equal (Predicate.PVText "asia"))
      idGt = Predicate.Column "id" [] (Predicate.Compare Predicate.Greater (Predicate.PVInt64 1))
      scan snapshot condition = do
        (_, stream) <- either fail pure (Scan.scanTableFiltered (Just ["id"]) condition snapshot)
        iterIOToList stream
      ids result = do
        batches <- either fail pure result
        fmap concat $ traverse (\batch -> case V.toList (Scan.batchColumns batch) of
          [AC.ColInt64 _ values] -> pure (VS.toList values)
          _ -> fail "expected ordered Int64 IDs") batches
  let leaf = PN.NSRequired (PN.NSPrimitive PN.LtInt64)
      columns = V.singleton (V.fromList (map (PN.NRLeaf . PN.LvInt64) [1, 2]))
  bytes <- either fail pure (PN.buildNestedFile (V.singleton ("col_id", leaf)) columns)
  BS.writeFile (root </> "asia.parquet") bytes
  scan (table [mismatched, nullRegion, matching]) (Predicate.And isAsia idGt) >>= ids >>= (`shouldBe` [2])
  scan (table [nullRegion, matching]) (Predicate.Not isAsia) >>= ids >>= (`shouldBe` [])
  scan (table [mismatched, matching]) (Predicate.Or isAsia idGt) >>= \case
    Left err -> err `shouldSatisfy` isInfixOf "missing-europe.parquet"
    Right _ -> expectationFailure "an OR with an unknown data-column branch pruned the file"
  let bounded = table [withUpperBound mismatched, withUpperBound nullRegion, matching]
  scan bounded (Predicate.Or isAsia idGt) >>= ids >>= (`shouldBe` [1, 2])
  scan bounded (Predicate.Or idGt isAsia) >>= ids >>= (`shouldBe` [1, 2])
  let mixedNot = Predicate.Not (Predicate.And (Predicate.Not isAsia) (Predicate.Not idGt))
  scan bounded mixedNot >>= ids >>= (`shouldBe` [1, 2])
  let withFixedId (D.ActionAdd file) =
        D.ActionAdd file {D.addStats = Just "{\"minValues\":{\"col_id\":1},\"maxValues\":{\"col_id\":1}}"}
      withFixedId action = action
      idIsOne = Predicate.Column "id" [] (Predicate.Compare Predicate.Equal (Predicate.PVInt64 1))
  scan (table [withFixedId mismatched]) (Predicate.Not idIsOne) >>= ids >>= (`shouldBe` [])
  let exact = table [matching]
      notEqualToOne = Predicate.Not (Predicate.Column "id" [] (Predicate.NullSafeEqual (Just (Predicate.PVInt64 1))))
  scan exact notEqualToOne >>= ids >>= (`shouldBe` [2])


scanNestedNullPruningTest :: IO ()
scanNestedNullPruningTest = withSystemTempDirectory "wireform-delta-nested-null" $ \root -> do
  let schema = "{\"type\":\"struct\",\"fields\":[{\"name\":\"s\",\"type\":{\"type\":\"struct\",\"fields\":[{\"name\":\"x\",\"type\":\"long\",\"nullable\":true,\"metadata\":{\"delta.columnMapping.id\":2,\"delta.columnMapping.physicalName\":\"col_x\"}}]},\"nullable\":true,\"metadata\":{\"delta.columnMapping.id\":1,\"delta.columnMapping.physicalName\":\"col_s\"}}]}"
      meta = D.MetaDataAction "table" Nothing Nothing Nothing schema []
        (Map.singleton "delta.columnMapping.mode" "name") Nothing
      protocol = D.ProtocolAction 3 7 ["columnMapping", "deletionVectors"] ["columnMapping", "deletionVectors"]
      stats count tight = "{\"numRecords\":2,\"nullCount\":{\"col_s\":{\"col_x\":"
        <> count <> "}},\"tightBounds\":" <> tight <> "}"
      vector = Aeson.object
        [ "storageType" Aeson..= ("p" :: T.Text)
        , "pathOrInlineDv" Aeson..= ("file:///missing-vector.bin" :: T.Text)
        , "sizeInBytes" Aeson..= (32 :: Int), "cardinality" Aeson..= (1 :: Int)
        ]
      file name raw dv = D.AddAction name 1 0 True (Just raw) Nothing mempty mempty dv Nothing Nothing
      missing count tight dv = file "missing.parquet" (stats count tight) dv
      isNull = Predicate.Column "s" ["x"] Predicate.IsNull
      nullSafe = Predicate.Column "s" ["x"] (Predicate.NullSafeEqual Nothing)
      scan candidate condition = do
        let actions = [D.ActionProtocol protocol, D.ActionMetaData meta, D.ActionAdd candidate]
            table = DIO.DeltaTable root Nothing Nothing [] (D.snapshotFromActions actions) (Just 0)
        (_, batches) <- either fail pure (Scan.scanTableFiltered (Just []) condition table)
        fmap (sum . map Scan.batchRowCount) <$> iterIOToList batches
      mustRead candidate condition = scan candidate condition >>= \case
        Left err -> err `shouldSatisfy` isInfixOf "missing.parquet"
        Right _ -> expectationFailure "a nested null count pruned a potentially matching file"
      struct = PN.NSOptional (PN.NSStruct (V.singleton ("col_x", PN.NSOptional (PN.NSPrimitive PN.LtInt64))))
      presentValue = PN.NRStruct (V.singleton (PN.NRLeaf (PN.LvInt64 42)))
  scan (missing "0" "true" Nothing) isNull `shouldReturn` Right 0
  let allPresent = missing "0" "true" Nothing
  Just (Right structured) <- pure (D.decodeAddStats allPresent)
  scan (allPresent {D.addStats = Nothing, D.addParsedStats = Just structured}) isNull `shouldReturn` Right 0
  scan (missing "0" "false" (Just vector)) nullSafe `shouldReturn` Right 0
  scan (missing "2" "false" (Just vector)) (Predicate.Not isNull) `shouldReturn` Right 0
  scan (missing "1" "true" (Just vector)) (Predicate.Not nullSafe) `shouldReturn` Right 0
  mustRead (missing "1" "false" (Just vector)) (Predicate.Not isNull)
  mustRead (missing "3" "true" Nothing) (Predicate.Not isNull)
  let literal = Predicate.PVInt64 42
      eq = Predicate.Column "s" ["x"] (Predicate.Compare Predicate.Equal literal)
      inSet = Predicate.Column "s" ["x"] (Predicate.In [Just literal])
      safeEq = Predicate.Column "s" ["x"] (Predicate.NullSafeEqual (Just literal))
      allNull = missing "2" "true" Nothing
  forM_ [eq, Predicate.Not eq, inSet, Predicate.Not inSet, safeEq, Predicate.Or eq inSet] $ \condition ->
    scan allNull condition `shouldReturn` Right 0
  scan (missing "1" "true" (Just vector)) eq `shouldReturn` Right 0
  scan (missing "2" "false" (Just vector)) eq `shouldReturn` Right 0
  mustRead (missing "1" "false" (Just vector)) eq
  mustRead (missing "3" "true" Nothing) eq
  mustRead allNull (Predicate.Not safeEq)
  bytes <- either fail pure $
    PN.buildNestedFile (V.singleton ("col_s", struct)) (V.singleton (V.fromList [PN.NRNull, presentValue]))
  BS.writeFile (root </> "live.parquet") bytes
  let live = file "live.parquet" (stats "1" "true") Nothing
  scan live isNull `shouldReturn` Right 1
  scan live (Predicate.Not isNull) `shouldReturn` Right 1
  scan live eq `shouldReturn` Right 1
  scan live (Predicate.Not eq) `shouldReturn` Right 0
  scan live (Predicate.Not safeEq) `shouldReturn` Right 1


timestampUpperBoundTest :: IO ()
timestampUpperBoundTest =
  withSystemTempDirectory "wireform-delta-stat-ceiling" $ \root -> do
    let logDir = root </> "_delta_log"
        schema = "{\"type\":\"struct\",\"fields\":[{\"name\":\"stamp\",\"type\":\"timestamp\",\"nullable\":true,\"metadata\":{}}]}" :: T.Text
        stats raw = Aeson.encode (Aeson.object ["maxValues" Aeson..= Aeson.object ["stamp" Aeson..= (raw :: T.Text)]])
        commit raw =
          [ "{\"protocol\":{\"minReaderVersion\":1,\"minWriterVersion\":2}}"
          , Aeson.encode (Aeson.object ["metaData" Aeson..= Aeson.object ["id" Aeson..= ("table" :: T.Text), "schemaString" Aeson..= schema]])
          , "{\"add\":{\"path\":\"data.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"stats\":"
              <> Aeson.encode (TE.decodeUtf8 (BL.toStrict (stats raw))) <> "}}"
          ]
        check raw expected = do
          BL.writeFile (logDir </> "00000000000000000000.json") (BLC.unlines (commit raw))
          table <- DIO.openDeltaTable root >>= either fail pure
          readStats <- either fail pure (Scan.tableFileStats table)
          bounds <- traverse (either fail pure . readStats) (DIO.dtActiveFiles table)
          map (fmap D.asMaxValues) bounds `shouldBe` [Just expected]
        bound = HM.singleton "stamp" . Aeson.String
    createDirectory logDir
    check "294247-01-10T04:00:54.775Z" (bound "294247-01-10T04:00:54.775807Z")
    -- Spark 4 preserves the seconds of offsets used before standard time.
    check "0202-10-21T00:00:00.000+00:53:28" (bound "0202-10-20T23:06:32.001Z")
    check "1880-01-01T23:59:59.999-00:01:15" (bound "1880-01-02T00:01:15Z")
    check "1880-01-01T23:59:59.999-00:01:60" HM.empty
    check "1880-01-01T23:59:59.999+00:0115" HM.empty
    check "1969-12-31T23:59:59.999Z" (bound "1970-01-01T00:00:00Z")
    check "2000-02-29T23:59:59.999Z" (bound "2000-03-01T00:00:00Z")
    check "1900-02-29T23:59:59.999Z" HM.empty
    check "2026-04-31T00:00:00Z" HM.empty
    check "2026-05-01T00:00:00.Z" HM.empty
    check "2026-05-01T00:00:00.1234567Z" HM.empty
    check "2026-05-01T00:00:00.1234560Z" (bound "2026-05-01T00:00:00.1234560Z")


ambiguousTimestampBoundsTest :: IO ()
ambiguousTimestampBoundsTest = do
  let schema = "{\"type\":\"struct\",\"fields\":[{\"name\":\"stamp\",\"type\":\"timestamp\",\"nullable\":true,\"metadata\":{}},{\"name\":\"local\",\"type\":\"timestamp_ntz\",\"nullable\":true,\"metadata\":{}}]}"
      metadata = D.MetaDataAction "table" Nothing Nothing Nothing schema [] mempty Nothing
      protocol = D.ProtocolAction 3 7 ["timestampNtz"] ["timestampNtz"]
      snapshot = D.emptySnapshot {D.tsMetaData = Just metadata, D.tsProtocol = Just protocol}
      table = DIO.DeltaTable "" Nothing Nothing [] snapshot (Just 0)
      raw = "{\"numRecords\":2,\"minValues\":{\"stamp\":\"2026-10-25 02:30:00.123456\",\"local\":\"2026-10-25 02:30:00.123456\"},\"maxValues\":{\"stamp\":\"2026-10-25T03:00:00.123456+01:00\",\"local\":\"2026-10-25 03:00:00.123\"},\"nullCount\":{\"stamp\":0,\"local\":0}}" :: T.Text
      upper = HM.fromList
        [ ("stamp", Aeson.String "2026-10-25T03:00:00.123456+01:00")
        , ("local", Aeson.String "2026-10-25 03:00:00.124")
        ]
  Just (D.ActionAdd file) <- pure . D.parseLogLine $
    "{\"add\":{\"path\":\"data.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"stats\":" <> Aeson.encode raw <> "}}"
  readStats <- either fail pure (Scan.tableFileStats table)
  Just stats <- either fail pure (readStats file)
  D.asMinValues stats `shouldBe` HM.singleton "local" (Aeson.String "2026-10-25 02:30:00.123456")
  D.asMaxValues stats `shouldBe` upper
  D.asNullCount stats `shouldBe` HM.fromList [("stamp", 0), ("local", 0)]
  Just (Right parsed) <- pure (D.decodeAddStats file)
  Just projected <- either fail pure (readStats (file {D.addStats = Nothing, D.addParsedStats = Just parsed}))
  projected `shouldBe` stats
  let zoneless = Aeson.String "2026-10-25 02:30:00.123"
      unknown = parsed {D.asMaxValues = HM.insert "stamp" zoneless (D.asMaxValues parsed)}
  Just withoutMaximum <- either fail pure (readStats (file {D.addStats = Nothing, D.addParsedStats = Just unknown}))
  D.asMaxValues withoutMaximum `shouldBe` HM.singleton "local" (Aeson.String "2026-10-25 03:00:00.124")


floatingStatsBoundsTest :: IO ()
floatingStatsBoundsTest = do
  let schema = "{\"type\":\"struct\",\"fields\":[{\"name\":\"single\",\"type\":\"float\",\"nullable\":true,\"metadata\":{}},{\"name\":\"double\",\"type\":\"double\",\"nullable\":true,\"metadata\":{}},{\"name\":\"subnormal\",\"type\":\"double\",\"nullable\":true,\"metadata\":{}}]}"
      metadata = D.MetaDataAction "table" Nothing Nothing Nothing schema [] mempty Nothing
      snapshot = D.emptySnapshot {D.tsMetaData = Just metadata}
      table = DIO.DeltaTable "" Nothing Nothing [] snapshot (Just 0)
  Just (D.ActionAdd file) <- pure (D.parseLogLine "{\"add\":{\"path\":\"data.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"stats\":\"{\\\"minValues\\\":{\\\"single\\\":-1.4e-45,\\\"double\\\":-0.1,\\\"subnormal\\\":-4.9e-324},\\\"maxValues\\\":{\\\"single\\\":1.4e-45,\\\"double\\\":0.1,\\\"subnormal\\\":4.9e-324}}\"}}")
  readStats <- either fail pure (Scan.tableFileStats table)
  Just stats <- either fail pure (readStats file)
  let rationalBound bounds key = case HM.lookup key bounds of
        Just (Aeson.Number value) -> Just (toRational value)
        _ -> Nothing
      check key exact = do
        rationalBound (D.asMinValues stats) key `shouldBe` Just (negate exact)
        rationalBound (D.asMaxValues stats) key `shouldBe` Just exact
  check "single" (1 / 2 ^ (149 :: Int))
  check "double" (toRational (0.1 :: Double))
  check "subnormal" (1 / 2 ^ (1074 :: Int))


unavailableStatsBoundsTest :: IO ()
unavailableStatsBoundsTest = do
  let schema = "{\"type\":\"struct\",\"fields\":[{\"name\":\"score\",\"type\":\"double\",\"nullable\":true,\"metadata\":{}},{\"name\":\"label\",\"type\":\"string\",\"nullable\":true,\"metadata\":{}},{\"name\":\"nested\",\"type\":{\"type\":\"struct\",\"fields\":[{\"name\":\"n\",\"type\":\"long\",\"nullable\":true,\"metadata\":{}}]},\"nullable\":true,\"metadata\":{}}]}"
      metadata = D.MetaDataAction "table" Nothing Nothing Nothing schema [] mempty Nothing
      snapshot = D.emptySnapshot {D.tsMetaData = Just metadata}
      table = DIO.DeltaTable "" Nothing Nothing [] snapshot (Just 0)
      payload = "{\"minValues\":{\"score\":\"-Infinity\",\"label\":null,\"nested\":{\"n\":null}},\"maxValues\":{\"score\":\"NaN\",\"label\":\"z\",\"nested\":{\"n\":7}}}" :: T.Text
  Just (D.ActionAdd file) <- pure . D.parseLogLine $
    "{\"add\":{\"path\":\"data.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"stats\":" <> Aeson.encode payload <> "}}"
  Just (Right raw) <- pure (D.decodeAddStats file)
  HM.lookup "score" (D.asMinValues raw) `shouldBe` Just (Aeson.String "-Infinity")
  HM.lookup "score" (D.asMaxValues raw) `shouldBe` Just (Aeson.String "NaN")
  HM.lookup "label" (D.asMinValues raw) `shouldBe` Just Aeson.Null
  readStats <- either fail pure (Scan.tableFileStats table)
  Just stats <- either fail pure (readStats file)
  D.asMinValues stats `shouldBe` HM.empty
  D.asMaxValues stats `shouldBe` HM.fromList
    [("label", Aeson.String "z"), ("nested", Aeson.object ["n" Aeson..= (7 :: Int)])]


invalidStatsBoundsTest :: IO ()
invalidStatsBoundsTest = do
  let field name kind = Aeson.object
        ["name" Aeson..= (name :: T.Text), "type" Aeson..= (kind :: T.Text),
         "nullable" Aeson..= True, "metadata" Aeson..= Aeson.object []]
      schema = TE.decodeUtf8 . BL.toStrict . Aeson.encode $ Aeson.object
        ["type" Aeson..= ("struct" :: T.Text), "fields" Aeson..=
          [field "small" "byte", field "large" "long", field "dec" "decimal(5,2)", field "ts" "timestamp", field "fp" "double"]]
      metadata = D.MetaDataAction "table" Nothing Nothing Nothing schema [] mempty Nothing
      snapshot = D.emptySnapshot {D.tsMetaData = Just metadata}
      table = DIO.DeltaTable "" Nothing Nothing [] snapshot (Just 0)
      payload = "{\"minValues\":{\"small\":-129,\"large\":-9223372036854775809,\"dec\":-1000,\"ts\":\"bad\"},\"maxValues\":{\"small\":127.0,\"large\":9223372036854775807,\"dec\":999.9900,\"ts\":\"1970-01-01T00:00:00.123456Z\"}}" :: T.Text
      fractional = "{\"minValues\":{\"small\":1.5,\"dec\":0.001,\"fp\":-1e1000000},\"maxValues\":{\"dec\":1e1000000,\"ts\":\"294248-01-01T00:00:00Z\",\"fp\":1e1000000}}" :: T.Text
      readAdd stats = D.parseLogLine $
        "{\"add\":{\"path\":\"data.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"stats\":" <> Aeson.encode stats <> "}}"
  readStats <- either fail pure (Scan.tableFileStats table)
  Just (D.ActionAdd file) <- pure (readAdd payload)
  Just (Right raw) <- pure (D.decodeAddStats file)
  HM.size (D.asMinValues raw) `shouldBe` 4
  Just stats <- either fail pure (readStats file)
  D.asMinValues stats `shouldBe` HM.empty
  D.asMaxValues stats `shouldBe` D.asMaxValues raw
  Just (D.ActionAdd invalid) <- pure (readAdd fractional)
  Just rejected <- either fail pure (readStats invalid)
  D.asMinValues rejected `shouldBe` HM.empty
  D.asMaxValues rejected `shouldBe` HM.empty


partitionFileStatsTest :: IO ()
partitionFileStatsTest = do
  let field name kind ident physical = Aeson.object
        [ "name" Aeson..= (name :: T.Text), "type" Aeson..= (kind :: T.Text), "nullable" Aeson..= True
        , "metadata" Aeson..= Aeson.object
            ["delta.columnMapping.id" Aeson..= (ident :: Int), "delta.columnMapping.physicalName" Aeson..= (physical :: T.Text)]
        ]
      schema = TE.decodeUtf8 . BL.toStrict . Aeson.encode $ Aeson.object
        [ "type" Aeson..= ("struct" :: T.Text)
        , "fields" Aeson..= [field "day" "date" 1 "p_day", field "amount" "long" 2 "p_amount"]
        ]
      metadata = D.MetaDataAction "table" Nothing Nothing Nothing schema ["day"]
        (Map.singleton "delta.columnMapping.mode" "name") Nothing
      protocol = D.ProtocolAction 2 5 [] []
      snapshot = D.emptySnapshot {D.tsMetaData = Just metadata, D.tsProtocol = Just protocol}
      table = DIO.DeltaTable "" Nothing Nothing [] snapshot (Just 0)
      payload = "{\"numRecords\":2,\"minValues\":{\"p_day\":\"2026-05-01\",\"p_amount\":1},\"maxValues\":{\"p_day\":\"2026-05-01\",\"p_amount\":5},\"nullCount\":{\"p_day\":0,\"p_amount\":1}}" :: T.Text
      input = "{\"add\":{\"path\":\"data.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"partitionValues\":{\"p_day\":\"2026-05-02\"},\"stats\":" <> Aeson.encode payload <> "}}"
  Just (D.ActionAdd file) <- pure (D.parseLogLine input)
  Just (Right raw) <- pure (D.decodeAddStats file)
  HM.lookup "p_day" (D.asMaxValues raw) `shouldBe` Just (Aeson.String "2026-05-01")
  readStats <- either fail pure (Scan.tableFileStats table)
  let checkpointFile = file {D.addStats = Nothing, D.addParsedStats = Just raw}
  forM_ [file, checkpointFile] $ \candidate -> do
    Just stats <- either fail pure (readStats candidate)
    D.asNumRecords stats `shouldBe` Just 2
    D.asMinValues stats `shouldBe` HM.singleton "amount" (Aeson.Number 1)
    D.asMaxValues stats `shouldBe` HM.singleton "amount" (Aeson.Number 5)
    D.asNullCount stats `shouldBe` HM.singleton "amount" 1
    D.asNullCounts stats `shouldBe` HM.singleton "amount" (D.NullCount 1)


invalidStatsShapeTest :: IO ()
invalidStatsShapeTest = do
  let field name kind = Aeson.object
        ["name" Aeson..= (name :: T.Text), "type" Aeson..= kind,
         "nullable" Aeson..= True, "metadata" Aeson..= Aeson.object []]
      struct fields = Aeson.object ["type" Aeson..= ("struct" :: T.Text), "fields" Aeson..= fields]
      primitive name kind = field name (Aeson.String kind)
      schema = TE.decodeUtf8 . BL.toStrict . Aeson.encode $ struct
        [ primitive "day" "date", primitive "text" "string", primitive "flag" "boolean"
        , field "nested" (struct [primitive "n" "long", primitive "valid" "long"])
        ]
      metadata = D.MetaDataAction "table" Nothing Nothing Nothing schema [] mempty Nothing
      protocol = D.ProtocolAction 3 7 ["deletionVectors"] ["deletionVectors"]
      snapshot = D.emptySnapshot {D.tsMetaData = Just metadata, D.tsProtocol = Just protocol}
      table = DIO.DeltaTable "" Nothing Nothing [] snapshot (Just 0)
      payload = "{\"numRecords\":3,\"minValues\":{\"day\":\"2026-02-30\",\"text\":false,\"flag\":\"false\",\"nested\":[]},\"maxValues\":{\"day\":\"6000000-01-01\",\"text\":\"z\",\"flag\":true},\"nullCount\":{\"day\":4,\"text\":{\"key\":0},\"flag\":0,\"nested\":{\"n\":4,\"valid\":3}}}" :: T.Text
      input = "{\"add\":{\"path\":\"data.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"stats\":" <> Aeson.encode payload <> "}}"
  Just (D.ActionAdd file) <- pure (D.parseLogLine input)
  Just (Right raw) <- pure (D.decodeAddStats file)
  HM.lookup "day" (D.asNullCounts raw) `shouldBe` Just (D.NullCount 4)
  readStats <- either fail pure (Scan.tableFileStats table)
  Just stats <- either fail pure (readStats file)
  D.asMinValues stats `shouldBe` HM.empty
  D.asMaxValues stats `shouldBe` HM.fromList [("text", Aeson.String "z"), ("flag", Aeson.Bool True)]
  D.asNullCount stats `shouldBe` HM.singleton "flag" 0
  D.asNullCounts stats `shouldBe` HM.fromList
    [("flag", D.NullCount 0), ("nested", D.NestedNullCounts (HM.singleton "valid" (D.NullCount 3)))]
  let descriptor = Aeson.object
        [ "storageType" Aeson..= ("p" :: T.Text), "pathOrInlineDv" Aeson..= ("file:///bitmap.bin" :: T.Text)
        , "sizeInBytes" Aeson..= (32 :: Int), "cardinality" Aeson..= (1 :: Int)
        ]
      withVector = file {D.addDeletionVector = Just descriptor}
  Just live <- either fail pure (readStats withVector)
  D.asNullCounts live `shouldBe` HM.singleton "flag" (D.NullCount 0)
  let wide = withVector
        { D.addStats = Nothing
        , D.addParsedStats = Just (raw {D.asTightBounds = Just False})
        }
  Just physical <- either fail pure (readStats wide)
  D.asNullCounts physical `shouldBe` D.asNullCounts stats


allNullStatsBoundsTest :: IO ()
allNullStatsBoundsTest = do
  let field name kind = Aeson.object
        ["name" Aeson..= (name :: T.Text), "type" Aeson..= kind,
         "nullable" Aeson..= True, "metadata" Aeson..= Aeson.object []]
      struct fields = Aeson.object ["type" Aeson..= ("struct" :: T.Text), "fields" Aeson..= fields]
      integer name = field name (Aeson.String "long")
      schema = TE.decodeUtf8 . BL.toStrict . Aeson.encode $ struct
        [integer "all", integer "maybeAll", field "nested" (struct [integer "all", integer "some"])]
      metadata = D.MetaDataAction "table" Nothing Nothing Nothing schema [] mempty Nothing
      protocol = D.ProtocolAction 3 7 ["deletionVectors"] ["deletionVectors"]
      snapshot = D.emptySnapshot {D.tsMetaData = Just metadata, D.tsProtocol = Just protocol}
      table = DIO.DeltaTable "" Nothing Nothing [] snapshot (Just 0)
      payload = "{\"numRecords\":3,\"minValues\":{\"all\":1,\"maybeAll\":2,\"nested\":{\"all\":3,\"some\":4}},\"maxValues\":{\"all\":5,\"maybeAll\":6},\"nullCount\":{\"all\":3,\"maybeAll\":2,\"nested\":{\"all\":3,\"some\":1}}}" :: T.Text
      input = "{\"add\":{\"path\":\"data.parquet\",\"size\":1,\"modificationTime\":0,\"dataChange\":true,\"stats\":" <> Aeson.encode payload <> "}}"
      nested = ("nested", Aeson.object ["some" Aeson..= (4 :: Int)])
      withCount = Aeson.object
        [ "storageType" Aeson..= ("p" :: T.Text), "pathOrInlineDv" Aeson..= ("file:///bitmap.bin" :: T.Text)
        , "sizeInBytes" Aeson..= (32 :: Int), "cardinality" Aeson..= (1 :: Int)
        ]
  readStats <- either fail pure (Scan.tableFileStats table)
  Just (D.ActionAdd base) <- pure (D.parseLogLine input)
  Just (Right raw) <- pure (D.decodeAddStats base)
  let fromCheckpoint = base {D.addStats = Nothing, D.addDeletionVector = Just withCount}
      wide = fromCheckpoint {D.addParsedStats = Just (raw {D.asTightBounds = Just False})}
      tightCounts = HM.singleton "maybeAll" (D.NullCount 2)
          <> HM.singleton "nested" (D.NestedNullCounts (HM.singleton "some" (D.NullCount 1)))
      tight = fromCheckpoint {D.addParsedStats = Just raw}
  Just noVector <- either fail pure (readStats base)
  D.asMinValues noVector `shouldBe` HM.fromList [("maybeAll", Aeson.Number 2), nested]
  D.asMaxValues noVector `shouldBe` HM.singleton "maybeAll" (Aeson.Number 6)
  D.asNullCounts noVector `shouldBe` D.asNullCounts raw
  Just wideBounds <- either fail pure (readStats wide)
  D.asMinValues wideBounds `shouldBe` D.asMinValues noVector
  D.asMaxValues wideBounds `shouldBe` D.asMaxValues noVector
  Just tightBounds <- either fail pure (readStats tight)
  D.asMinValues tightBounds `shouldBe` HM.fromList
    [("all", Aeson.Number 1), ("nested", Aeson.object ["all" Aeson..= (3 :: Int), "some" Aeson..= (4 :: Int)])]
  D.asMaxValues tightBounds `shouldBe` HM.singleton "all" (Aeson.Number 5)
  D.asNullCounts tightBounds `shouldBe` tightCounts
  let rejectStats message file = case readStats file of
        Left err -> err `shouldSatisfy` isInfixOf message
        Right stats -> expectationFailure ("accepted invalid deletion-vector statistics: " ++ show stats)
      withoutCount = raw {D.asNumRecords = Nothing}
      overCount = raw {D.asNumRecords = Just 0}
  rejectStats "requires numRecords" fromCheckpoint
  forM_ [Just True, Just False] $ \tightness -> do
    rejectStats "requires numRecords" (fromCheckpoint {D.addParsedStats = Just (withoutCount {D.asTightBounds = tightness})})
    rejectStats "cardinality exceeds numRecords" (fromCheckpoint {D.addParsedStats = Just (overCount {D.asTightBounds = tightness})})
  rejectStats "storageType" (tight {D.addDeletionVector = Just (Aeson.object ["cardinality" Aeson..= (1 :: Int)])})
  let descriptor offset bytes count = Aeson.eitherDecode $
        "{\"storageType\":\"p\",\"pathOrInlineDv\":\"file:///bitmap.bin\",\"offset\":" <> offset
          <> ",\"sizeInBytes\":" <> bytes <> ",\"cardinality\":" <> count <> "}"
      withDescriptor value = tight {D.addDeletionVector = Just value}
  equivalent <- either fail pure (descriptor "10e-1" "32.0" "1.0e0")
  readStats (withDescriptor equivalent) `shouldBe` Right (Just tightBounds)
  fractional <- either fail pure (descriptor "1" "32" "1.5")
  rejectStats "cardinality" (withDescriptor fractional)
  overflow <- either fail pure (descriptor "1" "2147483648" "1")
  rejectStats "sizeInBytes" (withDescriptor overflow)


checkpointStatsCoverageTest :: IO ()
checkpointStatsCoverageTest = do
  let leaf = PN.NSRequired . PN.NSPrimitive
      record = PN.NSStruct . V.fromList
      row = PN.NRStruct . V.fromList
      schema = V.singleton $
        ( "add"
        , PN.NSOptional $ record
            [ ("path", leaf PN.LtString)
            , ("size", leaf PN.LtInt64)
            , ("modificationTime", leaf PN.LtInt64)
            , ("dataChange", leaf PN.LtBool)
            , ("stats", PN.NSOptional (PN.NSPrimitive PN.LtString))
            , ("stats_parsed", record [("numRecords", leaf PN.LtInt64)])
            ]
        )
      json = PN.NRLeaf (PN.LvString "{\"numRecords\":10}")
      add path jsonStats count = row
        [ PN.NRLeaf (PN.LvString path)
        , PN.NRLeaf (PN.LvInt64 1)
        , PN.NRLeaf (PN.LvInt64 0)
        , PN.NRLeaf (PN.LvBool True)
        , jsonStats
        , row [PN.NRLeaf (PN.LvInt64 count)]
        ]
      -- Use a sparse add mask and place the uncovered row in the second word.
      sparseCheckpoint lastAdd = PN.buildNestedFile
        (V.cons ("futureAction", PN.NSOptional (PN.NSPrimitive PN.LtString)) schema)
        (V.fromList
          [ V.generate 66 (\i -> if i `elem` [0, 1, 65] then PN.NRNull else PN.NRLeaf (PN.LvString "ignored"))
          , V.generate 66 (\i -> case i of
                0 -> add "a" json (-1)
                1 -> add "b" json (-1)
                65 -> lastAdd
                _ -> PN.NRNull)
          ])
      check lastAdd expected = do
        bytes <- either fail pure (sparseCheckpoint lastAdd)
        snapshot <- either fail pure (Checkpoint.snapshotFromCheckpoint bytes)
        let count file = D.decodeAddStats file >>= either (const Nothing) D.asNumRecords
        map count (Map.elems (D.tsFiles snapshot)) `shouldBe` map Just expected
  check (add "c" json (-1)) [10, 10, 10]
  check (add "c" PN.NRNull 20) [10, 10, 20]


checkpointStatsProjectionTest :: IO ()
checkpointStatsProjectionTest = do
  let leaf = PN.NSRequired . PN.NSPrimitive
      record = PN.NSStruct . V.fromList
      struct = PN.NRStruct . V.fromList
      deep = iterate (\child -> record [("child", child)]) (leaf PN.LtInt64) !! 130
      nestedValue = iterate (struct . pure) (PN.NRLeaf (PN.LvInt64 7)) !! 130
      fields =
        V.fromList
          [ ("futureAction", PN.NSOptional deep)
          ,
            ( "add"
            , PN.NSOptional $
                record
                  [ ("path", leaf PN.LtString)
                  , ("size", leaf PN.LtInt64)
                  , ("modificationTime", leaf PN.LtInt64)
                  , ("dataChange", leaf PN.LtBool)
                  , ("futureProperty", PN.NSOptional deep)
                  , ("stats_parsed", record
                      [ ("numRecords", leaf PN.LtInt64)
                      , ("futureStatistic", deep)
                      , ("minValues", record [("id", leaf PN.LtInt64)])
                      ])
                  ]
            )
          ]
      add count =
        struct
          [ PN.NRLeaf (PN.LvString "file.parquet")
          , PN.NRLeaf (PN.LvInt64 10)
          , PN.NRLeaf (PN.LvInt64 0)
          , PN.NRLeaf (PN.LvBool True)
          , nestedValue
          , struct [PN.NRLeaf (PN.LvInt64 count), nestedValue, struct [PN.NRLeaf (PN.LvInt64 2)]]
          ]
      checkpoint count = PN.buildNestedFile fields (V.fromList [V.fromList [nestedValue, PN.NRNull], V.fromList [PN.NRNull, add count]])
  bytes <- either fail pure (checkpoint 42)
  actions <- either fail pure (Checkpoint.decodeCheckpointFile bytes)
  case actions of
    [D.ActionOther "futureAction", D.ActionAdd file] -> case D.decodeAddStats file of
      Just (Right stats) -> do
        D.asNumRecords stats `shouldBe` Just 42
        D.asMinValues stats `shouldBe` HM.singleton "id" (Aeson.Number 2)
      other -> expectationFailure ("expected recognized statistics, got " ++ show other)
    _ -> expectationFailure "expected an unfamiliar action and an add with structured statistics"
  corrupt <- either fail pure (checkpoint (-1))
  case Checkpoint.decodeCheckpointFile corrupt of
    Left err -> err `shouldSatisfy` isInfixOf "negative statistics count"
    Right _ -> expectationFailure "checkpoint accepted a malformed recognized statistic"


checkpointPhysicalTypeTest :: IO ()
checkpointPhysicalTypeTest = do
  let leaf = PN.NSRequired . PN.NSPrimitive
      fields = V.singleton ("txn", PN.NSStruct (V.fromList [("appId", leaf PN.LtString), ("version", leaf PN.LtDouble)]))
      row = PN.NRStruct (V.fromList [PN.NRLeaf (PN.LvString "writer"), PN.NRLeaf (PN.LvDouble 1)])
  bytes <- either fail pure (PN.buildNestedFile fields (V.singleton (V.singleton row)))
  case Checkpoint.decodeCheckpointFile bytes of
    Left err -> err `shouldSatisfy` isInfixOf "unexpected physical type"
    Right actions -> expectationFailure ("expected a type error, got " ++ show actions)


checkpointMismatchedActionRowsTest :: IO ()
checkpointMismatchedActionRowsTest = do
  let required = PN.NSRequired . PN.NSPrimitive
      record = PN.NSOptional . PN.NSStruct . V.fromList
      fields = V.fromList
        [ ("txn", record [("appId", PN.NSOptional (PN.NSPrimitive PN.LtString)), ("version", required PN.LtInt64)])
        , ("protocol", record [("minReaderVersion", required PN.LtInt32), ("minWriterVersion", required PN.LtInt32)])
        ]
      row = PN.NRStruct . V.fromList
      protocol = row [PN.NRLeaf (PN.LvInt32 1), PN.NRLeaf (PN.LvInt32 2)]
  bytes <- either fail pure (PN.buildNestedFile fields (V.fromList
    [V.singleton (row [PN.NRNull, PN.NRLeaf (PN.LvInt64 7)]), V.singleton protocol]))
  file <- either fail pure (PR.loadParquetFile bytes)
  let group = V.head (PT.fmRowGroups (PR.pfFooter file))
  metadata <- maybe (fail "missing txn.appId chunk metadata") pure (PT.ccMetadata (V.head (PT.rgColumns group)))
  (header, body) <- either fail pure (PP.readPageHeaderAt bytes (fromIntegral (PT.cmDataPageOffset metadata)))
  case PP.phType header of
    PP.PtDataPageV2 page -> do
      -- Drop the marker leaf's definition from a present txn with a
      -- null appId to an absent txn, leaving txn.version defined.
      let start = body + fromIntegral (PP.dph2RepLevelsLen page)
          size = fromIntegral (PP.dph2DefLevelsLen page)
          definition = LE.encodeRLEHybrid 2 (VP.singleton 0)
      BS.length definition `shouldBe` size
      let corrupt = BS.take start bytes <> definition <> BS.drop (start + size) bytes
      Checkpoint.decodeCheckpointFile corrupt `shouldBe` Left "Delta.Checkpoint: invalid action row or missing required fields"
    _ -> expectationFailure "expected V2 marker page"


checkpointOrphanFieldsTest :: IO ()
checkpointOrphanFieldsTest = do
  let leaf = PN.NSRequired . PN.NSPrimitive
      optional = PN.NSOptional . PN.NSPrimitive
      record = PN.NSStruct . V.fromList
      row = PN.NRStruct . V.fromList
      string = PN.NRLeaf . PN.LvString
      long = PN.NRLeaf . PN.LvInt64
      integer = PN.NRLeaf . PN.LvInt32
      protocol = row [integer 1, integer 2]
      fields = V.fromList
        [ ("protocol", PN.NSOptional (record [("minReaderVersion", leaf PN.LtInt32), ("minWriterVersion", leaf PN.LtInt32)]))
        , ("add", PN.NSOptional (record
            [ ("path", optional PN.LtString), ("size", optional PN.LtInt64)
            , ("modificationTime", optional PN.LtInt64), ("dataChange", optional PN.LtBool)
            , ("stats", optional PN.LtString)
            , ("baseRowId", optional PN.LtInt64)
            , ("deletionVector", PN.NSOptional (record [("cardinality", optional PN.LtInt64)]))
            , ("partitionValues", PN.NSOptional (PN.NSMap (leaf PN.LtString) (optional PN.LtString)))
            , ("stats_parsed", PN.NSOptional (record [("numRecords", optional PN.LtInt64)]))
            ]))
        ]
      fixture actions = PN.buildNestedFile fields (V.fromList
        [V.cons protocol (V.replicate (V.length actions - 1) PN.NRNull), actions])
      reject name actions = do
        bytes <- either fail pure (fixture actions)
        file <- either fail pure (PR.loadParquetFile bytes)
        let group = V.head (PT.fmRowGroups (PR.pfFooter file))
        metadata <- maybe (fail "missing add.path chunk metadata") pure (PT.ccMetadata (PT.rgColumns group V.! 2))
        (header, body) <- either fail pure (PP.readPageHeaderAt bytes (fromIntegral (PT.cmDataPageOffset metadata)))
        case PP.phType header of
          PP.PtDataPageV2 page -> do
            -- Hide the parent on the marker leaf only. The optional
            -- leaf still advertises a value in the protocol action row.
            let start = body + fromIntegral (PP.dph2RepLevelsLen page)
                size = fromIntegral (PP.dph2DefLevelsLen page)
                definition = LE.encodeRLEHybrid 2 (VP.cons 0 (VP.replicate (V.length actions - 1) 2))
            BS.length definition `shouldBe` size
            let corrupt = BS.take start bytes <> definition <> BS.drop (start + size) bytes
            case Checkpoint.decodeCheckpointFile corrupt of
              Left err -> err `shouldSatisfy` isInfixOf "invalid action row"
              Right _ -> expectationFailure ("checkpoint silently discarded orphaned " ++ name)
          _ -> expectationFailure "expected V2 path page"
  valid <- either fail pure (fixture (V.singleton PN.NRNull))
  Checkpoint.decodeCheckpointFile valid `shouldBe` Right [D.ActionProtocol (D.ProtocolAction 1 2 [] [])]
  let withField i value = PN.NRStruct (V.replicate 9 PN.NRNull V.// [(i, value)])
      single name i value = reject name (V.singleton (withField i value))
      add stats = row
        [ string "part.parquet", long 1, long 0, PN.NRLeaf (PN.LvBool True)
        , stats, PN.NRNull, PN.NRNull, PN.NRNull, row [long 1]
        ]
      json = string "{\"numRecords\":1}"
      orphanStats = withField 8 (row [long 1])
  single "add.stats" 4 json
  single "add.baseRowId" 5 (long 9)
  single "add.deletionVector.cardinality" 6 (row [long 1])
  single "add.partitionValues" 7 (PN.NRMapEntries (V.singleton (string "region", PN.NRNull)))
  reject "add.stats_parsed in a group without adds" (V.singleton orphanStats)
  reject "add.stats_parsed in a group with JSON stats" (V.fromList [orphanStats, add json])
  reject "add.stats_parsed in a group with structured stats" (V.fromList [orphanStats, add PN.NRNull])


checkpointOverlappingActionsTest :: IO ()
checkpointOverlappingActionsTest = do
  let required = PN.NSRequired . PN.NSPrimitive
      record = PN.NSOptional . PN.NSStruct . V.fromList
      schema = V.fromList
        [ ("add", record [("path", required PN.LtString), ("size", required PN.LtInt64),
                            ("modificationTime", required PN.LtInt64), ("dataChange", required PN.LtBool)])
        , ("protocol", record [("minReaderVersion", required PN.LtInt32), ("minWriterVersion", required PN.LtInt32)])
        ]
      row = PN.NRStruct . V.fromList . map PN.NRLeaf
      add = row [PN.LvString "part.parquet", PN.LvInt64 1, PN.LvInt64 0, PN.LvBool False]
      protocol = row [PN.LvInt32 1, PN.LvInt32 2]
      adds = V.generate 131 (\i -> if i == 63 || i == 65 then PN.NRNull else add)
      protocols overlap = V.generate 131 (\i -> if i == 63 || i == 65 || (overlap && i == 64) then protocol else PN.NRNull)
      checkpoint overlap = either fail pure (PN.buildNestedFile schema (V.fromList [adds, protocols overlap]))
  valid <- checkpoint False
  actions <- either fail pure (Checkpoint.decodeCheckpointFile valid)
  length actions `shouldBe` 131
  conflict <- checkpoint True
  Checkpoint.decodeCheckpointFile conflict `shouldBe` Left "Delta.Checkpoint: invalid action row or missing required fields"


checkpointSparseColumnsTest :: IO ()
checkpointSparseColumnsTest = do
  let required = PN.NSRequired . PN.NSPrimitive
      record = PN.NSOptional . PN.NSStruct . V.fromList
      schema = V.fromList
        [ ("remove", record [("path", required PN.LtString), ("deletionTimestamp", required PN.LtInt64), ("dataChange", required PN.LtBool)])
        , ("protocol", record [("minReaderVersion", required PN.LtInt32), ("minWriterVersion", required PN.LtInt32)])
        ]
      isRemove i = i `elem` [0, 1, 2, 3, 62, 63, 64, 65, 128, 190, 191, 192]
      row fields = PN.NRStruct (V.fromList (map PN.NRLeaf fields))
      removes = V.generate 193 $ \i ->
        if isRemove i then row [PN.LvString "part.parquet", PN.LvInt64 (fromIntegral i), PN.LvBool (even i)] else PN.NRNull
      protocols = V.generate 193 $ \i ->
        if isRemove i then PN.NRNull else row [PN.LvInt32 1, PN.LvInt32 2]
      expected = V.toList $ V.generate 193 $ \i ->
        if isRemove i
          then D.ActionRemove (D.RemoveAction "part.parquet" (Just (fromIntegral i)) (even i) Nothing Nothing Map.empty Nothing Nothing Nothing)
          else D.ActionProtocol (D.ProtocolAction 1 2 [] [])
  bytes <- either fail pure (PN.buildNestedFile schema (V.fromList [removes, protocols]))
  Checkpoint.decodeCheckpointFile bytes `shouldBe` Right expected


checkpointAddPathsTest :: IO ()
checkpointAddPathsTest = do
  let required = PN.NSRequired . PN.NSPrimitive
      schema = V.singleton ("add", PN.NSStruct (V.fromList
        [("path", required PN.LtBinary), ("size", required PN.LtInt64),
         ("modificationTime", required PN.LtInt64), ("dataChange", required PN.LtBool)]))
      row path = PN.NRStruct (V.fromList (map PN.NRLeaf
        [PN.LvBinary path, PN.LvInt64 1, PN.LvInt64 0, PN.LvBool True]))
      checkpoint paths = either fail pure (PN.buildNestedFile schema (V.singleton (V.fromList (map row paths))))
      utf8 = BS.pack [0xc3, 0xa9]
  bytes <- checkpoint ["", "before", "", utf8, "after", ""]
  actions <- either fail pure (Checkpoint.decodeCheckpointFile bytes)
  let addPath (D.ActionAdd add) = pure (D.addPath add)
      addPath _ = fail "expected checkpoint add"
      reject paths = do
        corrupt <- checkpoint paths
        case Checkpoint.decodeCheckpointFile corrupt of
          Left err -> err `shouldSatisfy` isInfixOf "invalid UTF-8"
          Right _ -> expectationFailure "checkpoint accepted an invalid UTF-8 path"
  traverse addPath actions >>= (`shouldBe` ["", "before", "", "é", "after", ""])
  onlyEmpty <- checkpoint ["", ""] >>= either fail pure . Checkpoint.decodeCheckpointFile
  traverse addPath onlyEmpty >>= (`shouldBe` ["", ""])
  reject [BS.take 1 utf8, "", BS.drop 1 utf8]
  reject ["before", BS.singleton 0xff, "after"]


checkpointDeletionVectorFieldsTest :: IO ()
checkpointDeletionVectorFieldsTest = do
  let required = PN.NSRequired . PN.NSPrimitive
      optional = PN.NSOptional . PN.NSPrimitive
      record = PN.NSStruct . V.fromList
      vectorField = PN.NSOptional (record
        [ ("storageType", optional PN.LtString)
        , ("pathOrInlineDv", optional PN.LtString)
        , ("offset", optional PN.LtInt32)
        , ("sizeInBytes", optional PN.LtInt32)
        , ("cardinality", optional PN.LtInt64)
        ])
      shared = [("path", required PN.LtString), ("dataChange", required PN.LtBool)]
      added = [("size", required PN.LtInt64), ("modificationTime", required PN.LtInt64)]
      leaf = PN.NRLeaf
      descriptor storage encoded offset bytes rows =
        PN.NRStruct (V.fromList [storage, encoded, offset, bytes, rows])
      stored = leaf (PN.LvString "p")
      payload = leaf (PN.LvString "file:///vector.bin")
      size = leaf (PN.LvInt32 32)
      count = leaf (PN.LvInt64 1)
      check action extraFields extraValues = do
        let decodeWith shape dv = do
              let schema = V.singleton (action, record (shared ++ extraFields ++ [("deletionVector", shape)]))
                  row = PN.NRStruct (V.fromList ([leaf (PN.LvString "part.parquet"), leaf (PN.LvBool True)] ++ extraValues ++ [dv]))
              Checkpoint.decodeCheckpointFile <$> either fail pure (PN.buildNestedFile schema (V.singleton (V.singleton row)))
            decode = decodeWith vectorField
            reject result = result >>= \case
              Left _ -> pure ()
              Right actions -> expectationFailure ("malformed deletion vector was accepted: " ++ show actions)
        decode PN.NRNull >>= \case
          Right [_] -> pure ()
          other -> expectationFailure ("expected null deletion vector to be accepted, got " ++ show other)
        decode (descriptor stored payload PN.NRNull size count) >>= \case
          Right [_] -> pure ()
          other -> expectationFailure ("expected complete deletion vector to be accepted, got " ++ show other)
        mapM_ (reject . decode)
          [ descriptor PN.NRNull PN.NRNull PN.NRNull PN.NRNull PN.NRNull
          , descriptor stored PN.NRNull PN.NRNull size count
          , descriptor stored payload PN.NRNull size PN.NRNull
          , descriptor stored payload PN.NRNull (leaf (PN.LvInt32 (-1))) count
          , descriptor (leaf (PN.LvString "i")) payload (leaf (PN.LvInt32 1)) size count
          ]
        let partial = PN.NSOptional (record [("cardinality", optional PN.LtInt64)])
        decodeWith partial PN.NRNull >>= \case
          Right [_] -> pure ()
          other -> expectationFailure ("expected absent vector in a partial schema, got " ++ show other)
        reject (decodeWith partial (PN.NRStruct (V.singleton PN.NRNull)))
        reject (decodeWith (optional PN.LtString) stored)
  check "add" added [leaf (PN.LvInt64 1), leaf (PN.LvInt64 0)]
  check "remove" [] []


checkpointUnsignedFieldsTest :: IO ()
checkpointUnsignedFieldsTest = do
  let scalar = PN.NSRequired . PN.NSPrimitive
      fields action children = V.singleton (action, PN.NSStruct (V.fromList children))
      removeFields = fields "remove" [("path", scalar PN.LtString), ("dataChange", scalar PN.LtBool), ("size", scalar PN.LtInt64)]
      metadataFields = fields "checkpointMetadata" [("version", scalar PN.LtInt64)]
      remove size = [PN.LvString "part.parquet", PN.LvBool True, PN.LvInt64 size]
      decode schema values = do
        let row = PN.NRStruct (V.fromList (map PN.NRLeaf values))
        Checkpoint.decodeCheckpointFile <$> either fail pure (PN.buildNestedFile schema (V.singleton (V.singleton row)))
      reject schema values = decode schema values >>= \case
        Left err -> err `shouldSatisfy` isInfixOf "invalid action row"
        Right _ -> expectationFailure "checkpoint accepted negative unsigned fields"
  reject removeFields (remove (-1))
  reject metadataFields [PN.LvInt64 (-1)]
  decode metadataFields [PN.LvInt64 7] `shouldReturn` Right [D.ActionCheckpointMetadata 7]
  decode removeFields (remove 0) >>= \case
    Right [D.ActionRemove action] -> D.removeSize action `shouldBe` Just 0
    other -> expectationFailure ("expected zero-size remove, got " ++ show other)


extendedRemoveMetadataTest :: IO ()
extendedRemoveMetadataTest = do
  let json extra = "{\"remove\":{\"path\":\"part.parquet\",\"dataChange\":false" <> extra <> "}}"
      flag = ",\"extendedFileMetadata\":true"
      size = ",\"size\":0"
      parts = ",\"partitionValues\":{}"
      jsonReaders =
        [ D.parseLogFileChecked . BL.fromStrict
        , fmap reverse . D.foldCheckpointJson (flip (:)) []
        , fmap reverse . D.foldCompactedBytes (flip (:)) []
        ]
      remove name extended bytes = D.ActionRemove (D.RemoveAction name Nothing False extended
        bytes Map.empty Nothing Nothing Nothing)
      valid = Right [remove "part.parquet" (Just True) (Just 0)]
      legacy = Right [remove "part.parquet" Nothing Nothing]
      incomplete bytes = Right [remove "part.parquet" (Just False) bytes]
  forM_ jsonReaders $ \decode -> do
    decode (json (flag <> size <> parts)) `shouldBe` valid
    decode (json "") `shouldBe` legacy
    decode (json (flag <> size)) `shouldBe` incomplete (Just 0)
    decode (json (flag <> parts)) `shouldBe` incomplete Nothing
    decode (json (flag <> size <> ",\"partitionValues\":null")) `shouldBe` incomplete (Just 0)
  let leaf = PN.NSRequired . PN.NSPrimitive
      optional = PN.NSOptional . PN.NSPrimitive
      partitionField = ("partitionValues", PN.NSOptional (PN.NSMap (leaf PN.LtString) (optional PN.LtString)))
      schema includeMap =
        V.singleton ("remove", PN.NSStruct (V.fromList
          ( [("path", leaf PN.LtString), ("dataChange", leaf PN.LtBool),
             ("extendedFileMetadata", optional PN.LtBool), ("size", optional PN.LtInt64)]
             ++ if includeMap then [partitionField] else []
          )))
      flags = PN.NRLeaf (PN.LvBool True)
      zero = PN.NRLeaf (PN.LvInt64 0)
      empty = PN.NRMapEntries V.empty
      row name extended bytes partitions =
        PN.NRStruct (V.fromList ([PN.NRLeaf (PN.LvString name), PN.NRLeaf (PN.LvBool False), extended, bytes] ++ partitions))
      decode includeMap rows = Checkpoint.decodeCheckpointFile <$> either fail pure
        (PN.buildNestedFile (schema includeMap) (V.singleton (V.fromList rows)))
      old = row "old.parquet" PN.NRNull PN.NRNull [PN.NRNull]
      string = PN.NRLeaf . PN.LvString
      entries = PN.NRMapEntries (V.fromList [(string "region", string "eu"), (string "site", PN.NRNull)])
      fullMap = Map.fromList [("region", Just "eu"), ("site", Nothing)]
  decode True [row "part.parquet" flags zero [empty]] `shouldReturn` valid
  decode False [row "part.parquet" PN.NRNull PN.NRNull []] `shouldReturn` legacy
  decode False [row "part.parquet" flags zero []] `shouldReturn` incomplete (Just 0)
  decode True [row "part.parquet" flags PN.NRNull [empty]] `shouldReturn` incomplete Nothing
  decode True [row "part.parquet" flags zero [PN.NRNull]] `shouldReturn` incomplete (Just 0)
  let bad = row "part.parquet" flags zero [PN.NRNull]
      populated = row "full.parquet" flags zero [entries]
      lastRow = row "other.parquet" flags zero [empty]
      describeRemove (D.ActionRemove action) =
        Just (D.removePath action, D.removeExtendedFileMetadata action, D.removeSize action, D.removePartitionValues action)
      describeRemove _ = Nothing
  fmap (map describeRemove) <$> decode True (bad : populated : replicate 63 old ++ [lastRow])
    `shouldReturn` Right
      ( [Just ("part.parquet", Just False, Just 0, Map.empty), Just ("full.parquet", Just True, Just 0, fullMap)]
          ++ replicate 63 (Just ("old.parquet", Nothing, Nothing, Map.empty))
          ++ [Just ("other.parquet", Just True, Just 0, Map.empty)]
      )


checkpointDuplicateChunkTest :: IO ()
checkpointDuplicateChunkTest = do
  let required = PN.NSRequired . PN.NSPrimitive
      fields = V.singleton ("txn", PN.NSStruct (V.fromList [("appId", required PN.LtString), ("version", required PN.LtInt64)]))
      row = PN.NRStruct (V.fromList [PN.NRLeaf (PN.LvString "writer"), PN.NRLeaf (PN.LvInt64 7)])
  bytes <- either fail pure (PN.buildNestedFile fields (V.singleton (V.singleton row)))
  file <- either fail pure (PR.loadParquetFile bytes)
  trailer <- either fail pure (PF.readFooterTrailer bytes)
  let footer = PR.pfFooter file
      dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
      repeatChunk group = group {PT.rgColumns = PT.rgColumns group V.// [(1, V.head (PT.rgColumns group))]}
      repeated = footer {PT.fmRowGroups = V.map repeatChunk (PT.fmRowGroups footer)}
  case Checkpoint.decodeCheckpointFile (BS.take dataEnd bytes <> PF.writeFooter repeated) of
    Left err -> err `shouldSatisfy` isInfixOf "column chunk path differs from schema"
    Right _ -> expectationFailure "checkpoint accepted a duplicate column chunk"


checkpointDuplicateActionGroupTest :: IO ()
checkpointDuplicateActionGroupTest = do
  let leaf = PN.NSRequired . PN.NSPrimitive
      group name ty = PN.NSOptional (PN.NSStruct (V.singleton (name, leaf ty)))
      schema = V.fromList [("txn", group "appId" PN.LtString), ("txn", group "version" PN.LtInt64)]
      row value = V.singleton (PN.NRStruct (V.singleton (PN.NRLeaf value)))
  bytes <- either fail pure $
    PN.buildNestedFile schema (V.fromList [row (PN.LvString "writer"), row (PN.LvInt64 7)])
  case Checkpoint.decodeCheckpointFile bytes of
    Left err -> err `shouldSatisfy` isInfixOf "duplicate checkpoint action name"
    Right _ -> expectationFailure "checkpoint combined leaves from two distinct action groups"


checkpointChunkOrderTest :: IO ()
checkpointChunkOrderTest = do
  let required = PN.NSRequired . PN.NSPrimitive
      record = PN.NSStruct . V.fromList
      bounds = record [("id", required PN.LtInt64)]
      fields = V.singleton ("add", record
        [ ("path", required PN.LtString)
        , ("size", required PN.LtInt64)
        , ("modificationTime", required PN.LtInt64)
        , ("dataChange", required PN.LtBool)
        , ("stats_parsed", record [("minValues", bounds), ("maxValues", bounds)])
        ])
      struct = PN.NRStruct . V.fromList
      integer = PN.NRLeaf . PN.LvInt64
      row = struct
        [ PN.NRLeaf (PN.LvString "file.parquet"), integer 1, integer 0, PN.NRLeaf (PN.LvBool True)
        , struct [struct [integer 2], struct [integer 9]]
        ]
  bytes <- either fail pure (PN.buildNestedFile fields (V.singleton (V.singleton row)))
  file <- either fail pure (PR.loadParquetFile bytes)
  trailer <- either fail pure (PF.readFooterTrailer bytes)
  let footer = PR.pfFooter file
      dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
      swap group =
        let columns = PT.rgColumns group
        in group {PT.rgColumns = columns V.// [(4, columns V.! 5), (5, columns V.! 4)]}
      reordered = footer {PT.fmRowGroups = V.map swap (PT.fmRowGroups footer)}
  _ <- either fail pure (Checkpoint.decodeCheckpointFile bytes)
  case Checkpoint.decodeCheckpointFile (BS.take dataEnd bytes <> PF.writeFooter reordered) of
    Left err -> err `shouldSatisfy` isInfixOf "column chunk path differs from schema"
    Right actions -> expectationFailure ("checkpoint accepted swapped bounds: " ++ show actions)


checkpointChunkMetadataTest :: IO ()
checkpointChunkMetadataTest = do
  let required = PN.NSRequired . PN.NSPrimitive
      fields = V.singleton ("txn", PN.NSStruct (V.fromList
        [("appId", required PN.LtString), ("version", required PN.LtInt64)]))
      row = PN.NRStruct (V.fromList [PN.NRLeaf (PN.LvString "writer"), PN.NRLeaf (PN.LvInt64 7)])
  bytes <- either fail pure (PN.buildNestedFile fields (V.singleton (V.singleton row)))
  file <- either fail pure (PR.loadParquetFile bytes)
  trailer <- either fail pure (PF.readFooterTrailer bytes)
  let footer = PR.pfFooter file
      dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
      changeVersion leaf
        | PT.seName leaf == "version" = leaf {PT.seType = Just PT.PTInt32}
        | otherwise = leaf
      inconsistent = footer {PT.fmSchema = V.map changeVersion (PT.fmSchema footer)}
      externalize group = group {PT.rgColumns = V.imap externalChunk (PT.rgColumns group)}
      externalChunk i chunk
        | i == 1 = chunk {PT.ccFilePath = Just "external.parquet"}
        | otherwise = chunk
      external = footer {PT.fmRowGroups = V.map externalize (PT.fmRowGroups footer)}
      reject message malformed =
        case Checkpoint.decodeCheckpointFile (BS.take dataEnd bytes <> PF.writeFooter malformed) of
          Left err -> err `shouldSatisfy` isInfixOf message
          Right actions -> expectationFailure ("checkpoint accepted conflicting chunk metadata: " ++ show actions)
  _ <- either fail pure (Checkpoint.decodeCheckpointFile bytes)
  reject "column chunk physical type differs from schema" inconsistent
  reject "external Parquet column chunks are unsupported" external


checkpointEventCountTest :: IO ()
checkpointEventCountTest = do
  let required = PN.NSRequired . PN.NSPrimitive
      protocolFields = V.singleton
        ( "protocol"
        , PN.NSStruct (V.fromList
            [ ("minReaderVersion", required PN.LtInt32)
            , ("minWriterVersion", required PN.LtInt32)
            , ("readerFeatures", PN.NSList (required PN.LtString))
            , ("writerFeatures", PN.NSList (required PN.LtString))
            ])
        )
      row featureValues = PN.NRStruct (V.fromList
        [ PN.NRLeaf (PN.LvInt32 3)
        , PN.NRLeaf (PN.LvInt32 7)
        , PN.NRList featureValues
        , PN.NRList featureValues
        ])
      features = V.fromList [PN.NRLeaf (PN.LvString "one"), PN.NRLeaf (PN.LvString "two")]
      rows = V.fromList [V.singleton (row features), V.singleton (row V.empty)]
      futureFields = V.singleton ("futureAction", PN.NSStruct (V.singleton ("value", required PN.LtInt64)))
      futureRow = PN.NRStruct (V.singleton (PN.NRLeaf (PN.LvInt64 9)))
  mapM_ (check [1, 2, 3] protocolFields) (V.toList rows)
  check [0] futureFields (V.singleton futureRow)
  where
    check indices fields rows = do
      bytes <- either fail pure (PN.buildNestedFile fields (V.singleton rows))
      file <- either fail pure (PR.loadParquetFile bytes)
      trailer <- either fail pure (PF.readFooterTrailer bytes)
      let footer = PR.pfFooter file
          dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
          corrupt index group =
            group {PT.rgColumns = V.imap (alter index) (PT.rgColumns group)}
          alter index i chunk
            | i == index = chunk {PT.ccMetadata = increment <$> PT.ccMetadata chunk}
            | otherwise = chunk
          increment md = md {PT.cmNumValues = PT.cmNumValues md + 1}
          reject index = do
            let modified = footer {PT.fmRowGroups = V.map (corrupt index) (PT.fmRowGroups footer)}
            case Checkpoint.decodeCheckpointFile (BS.take dataEnd bytes <> PF.writeFooter modified) of
              Left err -> err `shouldSatisfy` isInfixOf "column value count mismatch"
              Right _ -> expectationFailure "checkpoint accepted an inconsistent footer event count"
      mapM_ reject indices


checkpointListLayoutsTest :: IO ()
checkpointListLayoutsTest = do
  let element = PN.NSRequired (PN.NSPrimitive PN.LtString)
      fields =
        V.singleton
          ( "protocol"
          , PN.NSStruct
              ( V.fromList
                  [ ("minReaderVersion", PN.NSRequired (PN.NSPrimitive PN.LtInt32))
                  , ("minWriterVersion", PN.NSRequired (PN.NSPrimitive PN.LtInt32))
                  , ("readerFeatures", PN.NSList element)
                  , ("writerFeatures", PN.NSList element)
                  ]
              )
          )
      protocol =
        PN.NRStruct
          ( V.fromList
              [ PN.NRLeaf (PN.LvInt32 3)
              , PN.NRLeaf (PN.LvInt32 7)
              , PN.NRList (V.singleton (PN.NRLeaf (PN.LvString "columnMapping")))
              , PN.NRList (V.singleton (PN.NRLeaf (PN.LvString "columnMapping")))
              ]
          )
      expected = Right [D.ActionProtocol (D.ProtocolAction 3 7 ["columnMapping"] ["columnMapping"])]
  bytes <- either fail pure (PN.buildNestedFile fields (V.singleton (V.singleton protocol)))
  Checkpoint.decodeCheckpointFile bytes `shouldBe` expected
  file <- either fail pure (PR.loadParquetFile bytes)
  trailer <- either fail pure (PF.readFooterTrailer bytes)
  let footer = PR.pfFooter file
      dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
      schema = PT.fmSchema footer
      legacyPath = V.fromList ["protocol", "readerFeatures", "items", "feature"]
      twoLevelPath = V.fromList ["protocol", "readerFeatures", "feature"]
      directPath = V.fromList ["protocol", "readerFeatures"]
      rewriteSchema columnPath nodes =
        let modify group =
              group
                { PT.rgColumns =
                    V.imap
                      ( \i chunk ->
                          if i == 2
                            then chunk {PT.ccMetadata = fmap (\m -> m {PT.cmPathInSchema = columnPath}) (PT.ccMetadata chunk)}
                            else chunk
                      )
                      (PT.rgColumns group)
                }
        in BS.take dataEnd bytes
             <> PF.writeFooter
               (footer {PT.fmSchema = nodes, PT.fmRowGroups = V.map modify (PT.fmRowGroups footer)})
      renamed = schema V.// [(5, (schema V.! 5) {PT.seName = "items"}), (6, (schema V.! 6) {PT.seName = "feature"})]
      leaf = (schema V.! 6) {PT.seName = "feature", PT.seRepetition = Just PT.Repeated}
      twoLevel = V.snoc (V.take 5 schema) leaf <> V.drop 7 schema
      direct = V.snoc (V.take 4 schema) (leaf {PT.seName = "readerFeatures"}) <> V.drop 7 schema
  mapM_
    (\fixture -> Checkpoint.decodeCheckpointFile fixture `shouldBe` expected)
    [rewriteSchema legacyPath renamed, rewriteSchema twoLevelPath twoLevel, rewriteSchema directPath direct]


checkpointNullPartitionNameTest :: IO ()
checkpointNullPartitionNameTest = do
  let string = PN.NSPrimitive PN.LtString
      fields = V.singleton
        ("metaData", PN.NSStruct (V.fromList
          [ ("id", PN.NSRequired string)
          , ("schemaString", PN.NSRequired string)
          , ("partitionColumns", PN.NSList (PN.NSOptional string))
          ]))
      str = PN.NRLeaf . PN.LvString
      row partitions = PN.NRStruct (V.fromList
        [ str "table"
        , str "{\"type\":\"struct\",\"fields\":[]}"
        , PN.NRList (V.fromList partitions)
        ])
      decode partitions =
        either fail (pure . Checkpoint.decodeCheckpointFile)
          (PN.buildNestedFile fields (V.singleton (V.singleton (row partitions))))
  let expectNames values names = do
        result <- decode values
        case result of
          Right [D.ActionMetaData metadata] -> D.mdPartitionColumns metadata `shouldBe` names
          _ -> expectationFailure ("expected one metadata action, got " ++ show result)
  expectNames [] []
  expectNames [str "region"] ["region"]
  forM_ [[PN.NRNull], [str "region", PN.NRNull, str "date"]] $ \values -> do
    result <- decode values
    case result of
      Left err -> err `shouldSatisfy` isInfixOf "null partition column name"
      Right actions -> expectationFailure ("checkpoint accepted a null partition name: " ++ show actions)


checkpointMapValuesTest :: IO ()
checkpointMapValuesTest = do
  let leaf = PN.NSRequired . PN.NSPrimitive
      string = leaf PN.LtString
      record = PN.NSStruct . V.fromList
      fields =
        V.singleton
          ( "add"
          , record
              [ ("path", string)
              , ("size", leaf PN.LtInt64)
              , ("modificationTime", leaf PN.LtInt64)
              , ("dataChange", leaf PN.LtBool)
              , ("partitionValues", PN.NSMap string string)
              , ("tags", PN.NSMap string (PN.NSOptional (PN.NSPrimitive PN.LtString)))
              ]
          )
      str = PN.NRLeaf . PN.LvString
      row path partitions tags =
        PN.NRStruct
          ( V.fromList
              [ str path
              , PN.NRLeaf (PN.LvInt64 1)
              , PN.NRLeaf (PN.LvInt64 0)
              , PN.NRLeaf (PN.LvBool True)
              , PN.NRMapEntries (V.fromList partitions)
              , PN.NRMapEntries (V.fromList tags)
              ]
          )
      empty = row "empty.parquet" [] []
      populated =
        row
          "data.parquet"
          [(str "region", str "eu")]
          [(str "missing", PN.NRNull), (str "present", str "value")]
  bytes <- either fail pure (PN.buildNestedFile fields (V.singleton (V.fromList [empty, populated])))
  actions <- either fail pure (Checkpoint.decodeCheckpointFile bytes)
  -- Delta readers can encounter null tag values in both encodings.
  let jsonAdd = "{\"add\":{\"path\":\"data.parquet\",\"size\":1,\"modificationTime\":0,\
        \\"dataChange\":true,\"partitionValues\":{\"region\":\"eu\"},\
        \\"tags\":{\"missing\":null,\"present\":\"value\"}}}"
      emptyTag = "{\"add\":{\"path\":\"empty.parquet\",\"size\":1,\"modificationTime\":0,\
        \\"dataChange\":true,\"tags\":{\"empty\":\"\"}}}"
  case actions of
    [D.ActionAdd a, D.ActionAdd b] -> do
      D.parseLogFileChecked jsonAdd `shouldBe` Right [D.ActionAdd b]
      D.parseLogLine jsonAdd `shouldBe` Just (D.ActionAdd b)
      D.addPartitionValues a `shouldBe` Map.empty
      D.addTags a `shouldBe` Map.empty
      D.addPartitionValues b `shouldBe` Map.singleton "region" (Just "eu")
      D.addTags b `shouldBe` Map.singleton "present" "value"
      case D.parseLogFileChecked emptyTag of
        Right [D.ActionAdd file] -> D.addTags file `shouldBe` Map.singleton "empty" ""
        _ -> expectationFailure "expected an add with an empty tag value"
      snapshot <- either fail pure (Checkpoint.snapshotFromCheckpoint bytes)
      D.tsFiles snapshot `shouldBe` Map.fromList [(D.addPath a, a), (D.addPath b, b)]
      file <- either fail pure (PR.loadParquetFile bytes)
      trailer <- either fail pure (PF.readFooterTrailer bytes)
      let footer = PR.pfFooter file
          dataEnd = BS.length bytes - BS.length (PF.ftBytes trailer) - 8
          rename name = case name of
            "key_value" -> "entries"
            "key" -> "first"
            "value" -> "second"
            _ -> name
          renameNode node = node {PT.seName = rename (PT.seName node)}
          legacyAnnotation node
            | PT.seLogicalType node == Just PT.LTMap =
                node {PT.seLogicalType = Nothing, PT.seConvertedType = Just PT.CTMapKeyValue}
            | otherwise = node
          renameChunk chunk = chunk {PT.ccMetadata = fmap renamePath (PT.ccMetadata chunk)}
          renamePath metadata = metadata {PT.cmPathInSchema = V.map rename (PT.cmPathInSchema metadata)}
          renameGroup group = group {PT.rgColumns = V.map renameChunk (PT.rgColumns group)}
          legacy = footer {PT.fmSchema = V.map renameNode (PT.fmSchema footer), PT.fmRowGroups = V.map renameGroup (PT.fmRowGroups footer)}
      Checkpoint.decodeCheckpointFile (BS.take dataEnd bytes <> PF.writeFooter legacy) `shouldBe` Right actions
      let legacyMapKeyValue = legacy {PT.fmSchema = V.map legacyAnnotation (PT.fmSchema legacy)}
      Checkpoint.decodeCheckpointFile (BS.take dataEnd bytes <> PF.writeFooter legacyMapKeyValue) `shouldBe` Right actions
      let group = V.head (PT.fmRowGroups footer)
          cols = PT.rgColumns group
          index = 6
      metadata <- maybe (fail "missing tags key metadata") pure (PT.ccMetadata (cols V.! index))
      let start = fromIntegral (PT.cmDataPageOffset metadata)
      (header, body) <- either fail pure (PP.readPageHeaderAt bytes start)
      case PP.phType header of
        PP.PtDataPageV2 page -> do
          -- Put a continuation after the empty row while keeping two rows
          -- and two entries. Comparing just the grouped key and value counts
          -- cannot distinguish this from the original layout.
          let reps = LE.encodeRLEHybrid 1 (VP.fromList [0, 1, 0])
              defs = LE.encodeRLEHybrid 1 (VP.fromList [0, 1, 1])
              rest = body + fromIntegral (PP.dph2RepLevelsLen page + PP.dph2DefLevelsLen page)
          BS.length (reps <> defs) `shouldBe` rest - body
          let corrupt = BS.take body bytes <> reps <> defs <> BS.drop rest bytes
          case Checkpoint.decodeCheckpointFile corrupt of
            Left err -> err `shouldSatisfy` isInfixOf "continues an absent entry"
            Right _ -> expectationFailure "checkpoint accepted a map entry continuing an empty row"
        _ -> expectationFailure "expected V2 map page"
    _ -> expectationFailure "expected two checkpoint add actions"


checkpointMapPresenceTest :: IO ()
checkpointMapPresenceTest = do
  let leaf = PN.NSRequired . PN.NSPrimitive
      string = leaf PN.LtString
      fields = V.singleton
        ("add", PN.NSStruct (V.fromList
          [ ("path", string)
          , ("size", leaf PN.LtInt64)
          , ("modificationTime", leaf PN.LtInt64)
          , ("dataChange", leaf PN.LtBool)
          , ("tags", PN.NSOptional (PN.NSMap string (PN.NSOptional (PN.NSPrimitive PN.LtString))))
          ]))
      row name tags = PN.NRStruct (V.fromList
        [ PN.NRLeaf (PN.LvString name), PN.NRLeaf (PN.LvInt64 1)
        , PN.NRLeaf (PN.LvInt64 0), PN.NRLeaf (PN.LvBool False), tags
        ])
      entry = (PN.NRLeaf (PN.LvString "tag"), PN.NRNull)
      defined = (PN.NRLeaf (PN.LvString "key"), PN.NRLeaf (PN.LvString "value"))
      verify rows levels expectedTags = do
        bytes <- either fail pure (PN.buildNestedFile fields (V.singleton rows))
        actions <- either fail pure (Checkpoint.decodeCheckpointFile bytes)
        mapM getTags actions `shouldBe` Just (replicate (V.length rows - 1) Map.empty <> [expectedTags])
        file <- either fail pure (PR.loadParquetFile bytes)
        let chunks = PT.rgColumns (V.head (PT.fmRowGroups (PR.pfFooter file)))
        metadata <- maybe (fail "missing tags value metadata") pure (PT.ccMetadata (chunks V.! 5))
        (header, body) <- either fail pure (PP.readPageHeaderAt bytes (fromIntegral (PT.cmDataPageOffset metadata)))
        case PP.phType header of
          PP.PtDataPageV2 page -> do
            let start = body + fromIntegral (PP.dph2RepLevelsLen page)
                size = fromIntegral (PP.dph2DefLevelsLen page)
                definitions = LE.encodeRLEHybrid 2 (VP.fromList levels)
            BS.length definitions `shouldBe` size
            case Checkpoint.decodeCheckpointFile (BS.take start bytes <> definitions <> BS.drop (start + size) bytes) of
              Left err -> err `shouldSatisfy` isInfixOf "map key and value levels disagree"
              Right _ -> expectationFailure "checkpoint accepted conflicting map definitions"
          _ -> expectationFailure "expected V2 tag value page"
      absent = row "absent.parquet" PN.NRNull
      empty = row "empty.parquet" (PN.NRMapEntries V.empty)
      present = row "present.parquet" (PN.NRMapEntries (V.fromList [entry, defined]))
      getTags (D.ActionAdd add) = Just (D.addTags add)
      getTags _ = Nothing
  verify (V.singleton absent) [1] Map.empty
  verify (V.fromList [absent, empty, present]) [1, 0, 2, 3] (Map.singleton "key" "value")


checkpointLegacyDictionaryTest :: IO ()
checkpointLegacyDictionaryTest = do
  let string = PN.NSRequired (PN.NSPrimitive PN.LtString)
      long = PN.NSOptional (PN.NSPrimitive PN.LtInt64)
      fields = V.singleton ("txn", PN.NSStruct (V.fromList [("appId", string), ("version", long)]))
      row name version = PN.NRStruct (V.fromList [PN.NRLeaf (PN.LvString name), PN.NRLeaf (PN.LvInt64 version)])
      rows = V.fromList [row "client-a" 4, row "client-b" 7]
  original <- either fail pure (PN.buildNestedFile fields (V.singleton rows))
  file <- either fail pure (PR.loadParquetFile original)
  let footer = PR.pfFooter file
      group = V.head (PT.fmRowGroups footer)
      columns = PT.rgColumns group
      dictionary = PW.buildDictionary (PW.ColByteArray (V.fromList ["client-a", "client-b"]))
      dictionaryPage = PW.encodeDictPage dictionary
      pages = dictionaryPage <> PW.encodeDictDataPage dictionary
  (_, headerSize) <- either fail pure (PP.readPageHeaderAt dictionaryPage 0)
  versionBytes <- either fail pure (PR.columnChunkSlice file 0 1)
  appMeta <- maybe (fail "missing appId chunk") pure (PT.ccMetadata (columns V.! 0))
  versionMeta <- maybe (fail "missing version chunk") pure (PT.ccMetadata (columns V.! 1))
  let shortened = fromIntegral (BS.length pages - headerSize)
      app =
        appMeta
          { PT.cmEncodings = V.empty
          , PT.cmDataPageOffset = 4
          , PT.cmDictionaryPageOffset = Nothing
          , PT.cmTotalCompressedSize = shortened
          , PT.cmTotalUncompressedSize = shortened
          }
      version = versionMeta {PT.cmDataPageOffset = fromIntegral (4 + BS.length pages)}
      chunks =
        V.fromList
          ( zipWith
              ( \chunk metadata ->
                  chunk
                    { PT.ccMetadata = Just metadata
                    , PT.ccColumnIndexOffset = Nothing
                    , PT.ccOffsetIndexOffset = Nothing
                    }
              )
              (V.toList columns)
              [app, version]
          )
      legacyFooter = footer {PT.fmCreatedBy = Just "parquet-mr", PT.fmRowGroups = V.singleton group {PT.rgColumns = chunks}}
      bytes = PF.parquetMagic <> pages <> versionBytes <> PF.writeFooter legacyFooter
  snapshot <- either fail pure (Checkpoint.snapshotFromCheckpoint bytes)
  D.tsAppIds snapshot `shouldBe` Map.fromList [("client-a", 4), ("client-b", 7)]

  -- Corrupt only the footer's null count; the page still contains versions.
  versionStats <- maybe (fail "missing version statistics") pure (PT.cmStatistics version)
  let falseNullCount metadata = metadata {PT.cmStatistics = Just versionStats {PT.statNullCount = Just 2}}
      incorrect = V.imap (\i chunk -> if i == 1 then chunk {PT.ccMetadata = falseNullCount <$> PT.ccMetadata chunk} else chunk) chunks
      incorrectFooter = legacyFooter {PT.fmRowGroups = V.singleton group {PT.rgColumns = incorrect}}
      incorrectBytes = PF.parquetMagic <> pages <> versionBytes <> PF.writeFooter incorrectFooter
  checked <- either fail pure (Checkpoint.snapshotFromCheckpoint incorrectBytes)
  D.tsAppIds checked `shouldBe` D.tsAppIds snapshot

  -- An extra dictionary payload byte must not silently survive replay.
  let dataPage = PW.encodeDictDataPage dictionary
  (header, bodyOffset) <- either fail pure (PP.readPageHeaderAt dictionaryPage 0)
  let oversizedHeader = header
        { PP.phCompressedPageSize = (+ 1) <$> PP.phCompressedPageSize header
        , PP.phUncompressedPageSize = (+ 1) <$> PP.phUncompressedPageSize header
        }
      oversizedDictionary = PW.encodePageHeader oversizedHeader <> BS.drop bodyOffset dictionaryPage <> BS.singleton 0xff
      oversizedPages = oversizedDictionary <> dataPage
      rewrite i chunk = case i of
        0 -> chunk {PT.ccMetadata = Just app
          { PT.cmDictionaryPageOffset = Just 4
          , PT.cmDataPageOffset = fromIntegral (4 + BS.length oversizedDictionary)
          , PT.cmTotalCompressedSize = fromIntegral (BS.length oversizedPages)
          , PT.cmTotalUncompressedSize = fromIntegral (BS.length oversizedPages)
          }}
        _ -> chunk {PT.ccMetadata = Just version {PT.cmDataPageOffset = fromIntegral (4 + BS.length oversizedPages)}}
      oversizedFooter = legacyFooter {PT.fmRowGroups = V.singleton group {PT.rgColumns = V.imap rewrite chunks}}
      corrupt = PF.parquetMagic <> oversizedPages <> versionBytes <> PF.writeFooter oversizedFooter
  case Checkpoint.snapshotFromCheckpoint corrupt of
    Left err -> err `shouldSatisfy` isInfixOf "PLAIN BYTE_ARRAY trailing bytes"
    Right _ -> expectationFailure "checkpoint accepted trailing bytes in a dictionary page"


checkpointUnknownActionTest :: IO ()
checkpointUnknownActionTest = do
  let leaf = PN.NSPrimitive PN.LtString
      record = PN.NSOptional . PN.NSStruct . V.fromList
      fields =
        V.fromList
          [ ("txn", record [("appId", PN.NSOptional leaf), ("version", PN.NSRequired (PN.NSPrimitive PN.LtInt64))])
          , ("futureAction", record [("payload", PN.NSOptional leaf)])
          ]
      txn = PN.NRStruct (V.fromList [PN.NRLeaf (PN.LvString "client"), PN.NRLeaf (PN.LvInt64 42)])
      missingAppId = PN.NRStruct (V.fromList [PN.NRNull, PN.NRLeaf (PN.LvInt64 42)])
      future = PN.NRStruct (V.singleton PN.NRNull)
      makeFile name first second =
        either fail pure $
          PN.buildNestedFile
            (fields V.// [(1, (name, snd (fields V.! 1)))])
            (V.fromList [V.fromList [txn, first], V.fromList [PN.NRNull, second]])
  bytes <- makeFile "futureAction" PN.NRNull future
  actions <- either fail pure (Checkpoint.decodeCheckpointFile bytes)
  actions `shouldBe` [D.ActionTxn (D.TxnAction "client" 42 Nothing), D.ActionOther "futureAction"]
  snapshot <- either fail pure (Checkpoint.snapshotFromCheckpoint bytes)
  D.tsAppIds snapshot `shouldBe` Map.singleton "client" 42
  let reject input = case Checkpoint.decodeCheckpointFile input of
        Left err -> err `shouldSatisfy` isInfixOf "invalid action row"
        Right _ -> expectationFailure "an invalid checkpoint row was accepted"
  makeFile "futureAction" PN.NRNull PN.NRNull >>= reject
  makeFile "futureAction" txn future >>= reject
  makeFile "futureAction" missingAppId future >>= reject
  let noMarker = record [("version", PN.NSRequired (PN.NSPrimitive PN.LtInt64))]
      missingMarker = PN.NRStruct (V.singleton (PN.NRLeaf (PN.LvInt64 42)))
  missingColumn <-
    either fail pure $
      PN.buildNestedFile
        (fields V.// [(0, ("txn", noMarker))])
        (V.fromList [V.singleton missingMarker, V.singleton future])
  reject missingColumn
  forbidden <- makeFile "commitInfo" PN.NRNull future
  case Checkpoint.decodeCheckpointFile forbidden of
    Left err -> err `shouldSatisfy` isInfixOf "commitInfo and cdc are invalid"
    Right _ -> expectationFailure "checkpoint accepted a commitInfo action"


checkpointUnknownActionSiblingsTest :: IO ()
checkpointUnknownActionSiblingsTest = do
  let leaf = PN.NSRequired . PN.NSPrimitive
      record = PN.NSOptional . PN.NSStruct . V.fromList
      schema = V.fromList
        [ ("txn", record [("appId", leaf PN.LtString), ("version", leaf PN.LtInt64)])
        , ("futureAction", record [("first", leaf PN.LtInt64), ("second", leaf PN.LtInt64)])
        ]
      row = PN.NRStruct . V.fromList . map PN.NRLeaf
      txn = row [PN.LvString "client", PN.LvInt64 42]
      future = row [PN.LvInt64 1, PN.LvInt64 2]
      rows = V.fromList [V.fromList [txn, PN.NRNull], V.fromList [PN.NRNull, future]]
  bytes <- either fail pure (PN.buildNestedFile schema rows)
  Checkpoint.decodeCheckpointFile bytes
    `shouldBe` Right [D.ActionTxn (D.TxnAction "client" 42 Nothing), D.ActionOther "futureAction"]
  file <- either fail pure (PR.loadParquetFile bytes)
  let columns = PT.rgColumns (V.head (PT.fmRowGroups (PR.pfFooter file)))
      reject index = do
        metadata <- maybe (fail "missing futureAction leaf metadata") pure (PT.ccMetadata (columns V.! index))
        (header, body) <- either fail pure (PP.readPageHeaderAt bytes (fromIntegral (PT.cmDataPageOffset metadata)))
        case PP.phType header of
          PP.PtDataPageV2 page -> do
            -- Keep the header's null count intact while moving one
            -- leaf's parent onto the row that already contains the txn.
            let start = body + fromIntegral (PP.dph2RepLevelsLen page)
                size = fromIntegral (PP.dph2DefLevelsLen page)
                definitions = LE.encodeRLEHybrid 1 (VP.fromList [1, 0])
            BS.length definitions `shouldBe` size
            let corrupt = BS.take start bytes <> definitions <> BS.drop (start + size) bytes
            case Checkpoint.decodeCheckpointFile corrupt of
              Left err -> err `shouldSatisfy` isInfixOf "unfamiliar action leaves disagree"
              Right _ -> expectationFailure "checkpoint accepted disagreeing unfamiliar action leaves"
          _ -> expectationFailure "expected V2 future action page"
  mapM_ reject [2, 3]


checkpointUnknownActionRepetitionTest :: IO ()
checkpointUnknownActionRepetitionTest = do
  let leaf = PN.NSRequired . PN.NSPrimitive
      record = PN.NSOptional . PN.NSStruct . V.fromList
      schema = V.fromList
        [ ("txn", record [("appId", leaf PN.LtString), ("version", leaf PN.LtInt64)])
        , ("futureAction", record [("payload", PN.NSList (leaf PN.LtInt64)), ("label", leaf PN.LtString)])
        ]
      row = PN.NRStruct . V.fromList
      txn = row [PN.NRLeaf (PN.LvString "client"), PN.NRLeaf (PN.LvInt64 42)]
      future = row
        [ PN.NRList (V.fromList [PN.NRLeaf (PN.LvInt64 1), PN.NRLeaf (PN.LvInt64 2)])
        , PN.NRLeaf (PN.LvString "two entries")
        ]
      rows = V.fromList [V.fromList [txn, PN.NRNull], V.fromList [PN.NRNull, future]]
  bytes <- either fail pure (PN.buildNestedFile schema rows)
  Checkpoint.decodeCheckpointFile bytes
    `shouldBe` Right [D.ActionTxn (D.TxnAction "client" 42 Nothing), D.ActionOther "futureAction"]
  file <- either fail pure (PR.loadParquetFile bytes)
  let columns = PT.rgColumns (V.head (PT.fmRowGroups (PR.pfFooter file)))
  metadata <- maybe (fail "missing futureAction chunk metadata") pure (PT.ccMetadata (columns V.! 2))
  (header, body) <- either fail pure (PP.readPageHeaderAt bytes (fromIntegral (PT.cmDataPageOffset metadata)))
  case PP.phType header of
    PP.PtDataPageV2 page -> do
      -- Move the first list element underneath the absent action on the
      -- txn row. The number of rows, values and nulls does not change.
      let repetition = LE.encodeRLEHybrid 1 (VP.fromList [0, 1, 0])
          size = fromIntegral (PP.dph2RepLevelsLen page)
      BS.length repetition `shouldBe` size
      let corrupt = BS.take body bytes <> repetition <> BS.drop (body + size) bytes
      case Checkpoint.decodeCheckpointFile corrupt of
        Left err -> err `shouldSatisfy` isInfixOf "repeated value continues an absent action"
        Right _ -> expectationFailure "checkpoint accepted a repeated value under an absent action"
    _ -> expectationFailure "expected V2 future action page"
