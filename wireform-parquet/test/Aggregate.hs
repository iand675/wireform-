{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for 'Parquet.Aggregate' — pure stats arithmetic over
synthetic 'FileMetadata' (no actual parquet files needed).

The interesting cases are:

  * @count(*)@ across multiple row groups.
  * @count(col)@ when every row group reports null counts.
  * @count(col)@ poisoned to 'Nothing' when one row group
    omits the null count.
  * @columnMin@ / @columnMax@ folding over per-row-group
    min / max statistics, including the legacy 'statMin' /
    'statMax' slots and the modern 'statMinValue' /
    'statMaxValue' slots.
  * @columnMin@ / @columnMax@ poisoned to 'Nothing' when one
    row group omits the stat.
-}
module Aggregate (run) where

import Columnar.Predicate qualified as Pred
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as V
import Data.Word ()
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Parquet.Aggregate qualified as Agg
import Parquet.Footer qualified as Footer
import Parquet.Predicate qualified as PPred
import Parquet.Types
import Thrift.Encode qualified as Thrift
import Wireform.Builder (int32LE, int64LE, toLazyByteString)


{- | Public entry: prints OK / FAIL lines like the rest of the
wireform-parquet test suite.
-}
run :: IO ()
run = do
  test
    "fileRowCount over 3 row groups"
    (Agg.fileRowCount (mkFm rowGroups123) == 600)

  test
    "columnNonNullCount sums (rg.numRows - null_count)"
    (Agg.columnNonNullCount (mkFm rowGroups123) 0 == Just (600 - (1 + 2 + 3)))
  test
    "columnNonNullCount poisons to Nothing when one rg lacks null_count"
    (Agg.columnNonNullCount (mkFm rowGroupsMissingNull) 0 == Nothing)

  test
    "columnMin folds modern statMinValue across rgs"
    ( Agg.columnMin (mkFm rowGroupsMinMaxModern) 0
        == Just (Pred.PVInt32 1)
    )
  test
    "columnMax folds modern statMaxValue across rgs"
    ( Agg.columnMax (mkFm rowGroupsMinMaxModern) 0
        == Just (Pred.PVInt32 99)
    )

  test
    "columnMin falls back to legacy statMin when statMinValue absent"
    ( Agg.columnMin (mkFm rowGroupsMinMaxLegacy) 0
        == Just (Pred.PVInt64 (-7))
    )
  test
    "columnMax falls back to legacy statMax when statMaxValue absent"
    ( Agg.columnMax (mkFm rowGroupsMinMaxLegacy) 0
        == Just (Pred.PVInt64 12345)
    )

  test
    "columnMin poisons to Nothing when one rg has no min"
    (Agg.columnMin (mkFm rowGroupsMissingMin) 0 == Nothing)
  test
    "columnMax poisons to Nothing when one rg has no max"
    (Agg.columnMax (mkFm rowGroupsMissingMax) 0 == Nothing)
  exactBoundsRegression test
  countRegression test
  countProperty
  where
    -- columnSum was removed from Parquet.Aggregate (Parquet's
    -- Statistics message doesn't carry per-row-group sums; ORC
    -- does, see ORC.Aggregate.columnSum).

    test :: String -> Bool -> IO ()
    test name True = putStrLn ("OK: " ++ name)
    test name False = error ("FAIL: " ++ name)


-- ============================================================
-- Synthetic FileMetadata builders
-- ============================================================

mkFm :: [RowGroup] -> FileMetadata
mkFm rgs =
  FileMetadata
    { fmVersion = 2
    , fmSchema =
        V.cons (SchemaElement "schema" Nothing Nothing (Just (fromIntegral (V.length leaves))) Nothing Nothing Nothing Nothing) leaves
    , fmNumRows = sum (map rgNumRows rgs)
    , fmRowGroups = V.fromList rgs
    , fmCreatedBy = Nothing
    , fmKeyValueMetadata = Nothing
    , fmColumnOrders = Just (V.replicate (case rgs of [] -> 0; first : _ -> V.length (rgColumns first)) TypeDefinedOrder)
    }
  where
    leaves = case rgs of
      [] -> V.empty
      first : _ ->
        V.map
          ( \column ->
              SchemaElement
                (maybe "value" (\metadata -> V.last (cmPathInSchema metadata)) (ccMetadata column))
                (Just Optional)
                (cmType <$> ccMetadata column)
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
          )
          (rgColumns first)


mkRg :: Int64 -> [(Maybe Int64, Maybe Statistics)] -> RowGroup
mkRg n cols =
  RowGroup
    { rgColumns = V.fromList (map mkCc cols)
    , rgTotalByteSize = 0
    , rgNumRows = n
    , rgSortingColumns = Nothing
    }


mkCc :: (Maybe Int64, Maybe Statistics) -> ColumnChunk
mkCc (mNumValues, mStats) =
  ColumnChunk
    { ccFilePath = Nothing
    , ccFileOffset = 0
    , ccMetadata =
        Just
          ColumnMetadata
            { cmType = PTInt32
            , cmEncodings = V.empty
            , cmPathInSchema = V.singleton "x"
            , cmCodec = Uncompressed
            , cmNumValues = maybe 0 id mNumValues
            , cmTotalUncompressedSize = 0
            , cmTotalCompressedSize = 0
            , cmDataPageOffset = 0
            , cmDictionaryPageOffset = Nothing
            , cmStatistics = mStats
            , cmBloomFilterOffset = Nothing
            , cmBloomFilterLength = Nothing
            }
    , ccOffsetIndexOffset = Nothing
    , ccOffsetIndexLength = Nothing
    , ccColumnIndexOffset = Nothing
    , ccColumnIndexLength = Nothing
    }


emptyStats :: Statistics
emptyStats =
  Statistics
    { statMinValueExact = Nothing
    , statMaxValueExact = Nothing
    , statNanCount = Nothing
    , statMin = Nothing
    , statMax = Nothing
    , statNullCount = Nothing
    , statDistinctCount = Nothing
    , statMinValue = Nothing
    , statMaxValue = Nothing
    }


statsWithNullCount :: Int64 -> Statistics
statsWithNullCount n = emptyStats {statNullCount = Just n}


statsInt32MinMax :: Int32 -> Int32 -> Statistics
statsInt32MinMax lo hi =
  emptyStats
    { statMinValue = Just (le32 lo)
    , statMaxValue = Just (le32 hi)
    , statMinValueExact = Just True
    , statMaxValueExact = Just True
    }


statsInt64LegacyMinMax :: Int64 -> Int64 -> Statistics
statsInt64LegacyMinMax lo hi =
  emptyStats
    { statMin = Just (le64 lo)
    , statMax = Just (le64 hi)
    }


le32 :: Int32 -> BS.ByteString
le32 = BL.toStrict . toLazyByteString . int32LE


le64 :: Int64 -> BS.ByteString
le64 = BL.toStrict . toLazyByteString . int64LE


-- 3 row groups of 100/200/300 rows, 1/2/3 nulls in column 0.
rowGroups123 :: [RowGroup]
rowGroups123 =
  [ mkRg 100 [(Just 100, Just (statsWithNullCount 1))]
  , mkRg 200 [(Just 200, Just (statsWithNullCount 2))]
  , mkRg 300 [(Just 300, Just (statsWithNullCount 3))]
  ]


rowGroupsMissingNull :: [RowGroup]
rowGroupsMissingNull =
  [ mkRg 100 [(Just 100, Just emptyStats)]
  , mkRg 200 [(Just 200, Just (statsWithNullCount 0))]
  ]


rowGroupsMinMaxModern :: [RowGroup]
rowGroupsMinMaxModern =
  -- min across [3,1,42] = 1; max across [50,99,80] = 99.
  -- Column 0 must report ParquetType PTInt32 in every chunk
  -- (mkCc's default).
  [ mkRg 100 [(Just 100, Just (overrideType PTInt32 (statsInt32MinMax 3 50)))]
  , mkRg 100 [(Just 100, Just (overrideType PTInt32 (statsInt32MinMax 1 80)))]
  , mkRg 100 [(Just 100, Just (overrideType PTInt32 (statsInt32MinMax 42 99)))]
  ]


rowGroupsMinMaxLegacy :: [RowGroup]
rowGroupsMinMaxLegacy =
  [ mkRgWithType 100 [(Just 100, Just (statsInt64LegacyMinMax (-7) 12345))]
  , mkRgWithType 100 [(Just 100, Just (statsInt64LegacyMinMax 0 99))]
  ]


rowGroupsMissingMin :: [RowGroup]
rowGroupsMissingMin =
  [ mkRg 100 [(Just 100, Just (overrideType PTInt32 (statsInt32MinMax 3 50)))]
  , mkRg 100 [(Just 100, Just emptyStats)]
  ]


rowGroupsMissingMax :: [RowGroup]
rowGroupsMissingMax =
  [ mkRg 100 [(Just 100, Just (overrideType PTInt32 (statsInt32MinMax 3 50)))]
  , mkRg 100 [(Just 100, Just emptyStats {statMinValue = Just (le32 0)})]
  ]


-- Force the column type on a synthetic ColumnChunk —
-- cmType gates which 'PValue' constructor decoders pick.
overrideType :: ParquetType -> Statistics -> Statistics
overrideType _ = id -- placeholder; type is set on the ColumnMetadata


-- Build a row group whose only column reports PTInt64 (so the
-- legacy stats decode correctly).
mkRgWithType :: Int64 -> [(Maybe Int64, Maybe Statistics)] -> RowGroup
mkRgWithType n cols =
  RowGroup
    { rgColumns = V.fromList (map (mkCcWithType PTInt64) cols)
    , rgTotalByteSize = 0
    , rgNumRows = n
    , rgSortingColumns = Nothing
    }


mkCcWithType :: ParquetType -> (Maybe Int64, Maybe Statistics) -> ColumnChunk
mkCcWithType ty values =
  let column = mkCc values
  in column {ccMetadata = (\metadata -> metadata {cmType = ty}) <$> ccMetadata column}


-- Compact bounds need not occur in the data, so they cannot answer MIN/MAX.
exactBoundsRegression :: (String -> Bool -> IO ()) -> IO ()
exactBoundsRegression test = do
  let compact =
        emptyStats
          { statMinValue = Just "B"
          , statMaxValue = Just "C"
          , statNullCount = Just 0
          , statMinValueExact = Just False
          , statMaxValueExact = Just False
          }
      metadata stats = mkFm [mkRgTyped PTByteArray 2 stats]
      file = metadata compact
      root = V.head (fmRowGroups file)
  test
    "compact bounds cannot supply exact aggregate values"
    (Agg.columnMin file 0 == Nothing && Agg.columnMax file 0 == Nothing)
  test
    "compact bounds still prove a value cannot occur"
    (PPred.evalRowGroup file (Pred.PCol "x" (Pred.PEq (Pred.PVBinary "A"))) root == Pred.PSkip)
  mapM_
    ( \flag -> do
        let unknown = metadata (compact {statMinValueExact = flag, statMaxValueExact = flag})
        test
          "unknown/false exactness cannot establish actual extrema"
          (Agg.columnMin unknown 0 == Nothing && Agg.columnMax unknown 0 == Nothing)
    )
    [Nothing, Just False]
  let onlyMin = metadata (compact {statMinValueExact = Just True})
      onlyMax = metadata (compact {statMaxValueExact = Just True})
  test
    "min/max exactness is checked independently"
    ( Agg.columnMin onlyMin 0 == Just (Pred.PVBinary "B")
        && Agg.columnMax onlyMin 0 == Nothing
        && Agg.columnMin onlyMax 0 == Nothing
        && Agg.columnMax onlyMax 0 == Just (Pred.PVBinary "C")
    )
  let legacy =
        (statsInt64LegacyMinMax 7 9)
          { statMinValue = Just (le64 0)
          , statMaxValue = Just (le64 10)
          , statMinValueExact = Just False
          , statMaxValueExact = Just False
          }
      legacyFile = mkFm [mkRgTyped PTInt64 2 legacy]
  test
    "inexact modern bounds may fall back to compatible legacy extrema"
    (Agg.columnMin legacyFile 0 == Just (Pred.PVInt64 7) && Agg.columnMax legacyFile 0 == Just (Pred.PVInt64 9))
  mapM_
    ( \flags -> do
        let stats = compact {statMinValueExact = fst flags, statMaxValueExact = snd flags}
            original = metadata stats
            decoded = Footer.readFooterRaw (Thrift.encodeCompact (Footer.fileMetadataToThrift original))
        test
          "exactness flags preserve missing, false and true through raw Thrift"
          (decoded == Right original)
    )
    [(Nothing, Nothing), (Just False, Just True), (Just True, Just False)]
  let allNull = mkRgTyped PTInt32 3 (emptyStats {statNullCount = Just 3})
      empty = mkRgTyped PTInt32 0 emptyStats
      exact = mkRgTyped PTInt32 2 (statsInt32MinMax 7 9)
  test
    "all-null and empty groups do not poison known extrema"
    ( Agg.columnMin (mkFm [allNull, exact, empty]) 0 == Just (Pred.PVInt32 7)
        && Agg.columnMax (mkFm [empty, exact, allNull]) 0 == Just (Pred.PVInt32 9)
    )
  test
    "entirely null or empty columns have no extrema"
    (Agg.columnMin (mkFm [allNull, empty]) 0 == Nothing && Agg.columnMax (mkFm [allNull, empty]) 0 == Nothing)
  let floating =
        emptyStats
          { statMinValue = Just (BS.pack [0, 0, 128, 63])
          , statMaxValue = Just (BS.pack [0, 0, 0, 64])
          , statMinValueExact = Just True
          , statMaxValueExact = Just True
          , statNullCount = Just 0
          }
      floatFile nans = mkFm [mkRgTyped PTFloat 2 (floating {statNanCount = nans})]
  mapM_
    ( \nans ->
        test
          "unknown/present NaNs require an explicit scan policy"
          (Agg.columnMin (floatFile nans) 0 == Nothing && Agg.columnMax (floatFile nans) 0 == Nothing)
    )
    [Nothing, Just 1]
  test
    "known NaN-free exact floating statistics answer min/max"
    ( Agg.columnMin (floatFile (Just 0)) 0 == Just (Pred.PVFloat 1)
        && Agg.columnMax (floatFile (Just 0)) 0 == Just (Pred.PVFloat 2)
    )


countRegression :: (String -> Bool -> IO ()) -> IO ()
countRegression test = do
  let group = mkRgTyped PTInt32 3 (statsWithNullCount 1)
      file = mkFm [group]
      mapColumn change = group {rgColumns = fmap (\column -> column {ccMetadata = change <$> ccMetadata column}) (rgColumns group)}
      withNulls n = mapColumn (\metadata -> metadata {cmStatistics = Just (statsWithNullCount n)})
  mapM_
    ( \nulls ->
        test
          "invalid null counts cannot produce a non-null row count"
          (Agg.columnNonNullCount (mkFm [withNulls nulls]) 0 == Nothing)
    )
    [-1, 4]
  test
    "flat events must include null rows"
    (Agg.columnNonNullCount (mkFm [mapColumn (\metadata -> metadata {cmNumValues = 2})]) 0 == Nothing)
  test
    "missing/negative column ordinals do not invent counts"
    (Agg.columnNonNullCount file (-1) == Nothing && Agg.columnNonNullCount file 1 == Nothing)
  let required = file {fmSchema = fmSchema file V.// [(1, (fmSchema file V.! 1) {seRepetition = Just Required})]}
  test "required paths cannot report null rows" (Agg.columnNonNullCount required 0 == Nothing)
  let repeated = file {fmSchema = fmSchema file V.// [(1, (fmSchema file V.! 1) {seRepetition = Just Repeated})]}
  test "repeated events do not establish a non-null row count" (Agg.columnNonNullCount repeated 0 == Nothing)
  let root = SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
      parent = SchemaElement "record" (Just Optional) Nothing (Just 1) Nothing Nothing Nothing Nothing
      leaf = SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
      nested =
        file
          { fmSchema = V.fromList [root, parent, leaf]
          , fmRowGroups =
              V.singleton
                (mapColumn (\metadata -> metadata {cmPathInSchema = V.fromList ["record", "x"]}))
          }
  test
    "nullable ancestors contribute null rows to a required leaf"
    (Agg.columnNonNullCount nested 0 == Just 2)
  let repeatedAncestor = nested {fmSchema = fmSchema nested V.// [(1, parent {seRepetition = Just Repeated})]}
  test
    "repeated ancestors also prevent leaf non-null row counts"
    (Agg.columnNonNullCount repeatedAncestor 0 == Nothing)
  mapM_
    ( \bad -> do
        test
          "invalid file/group row counts fail checked aggregation"
          (Agg.fileRowCountChecked bad == Nothing && Agg.columnNonNullCount bad 0 == Nothing)
    )
    [ file {fmNumRows = -1}
    , file {fmNumRows = 2}
    , file {fmNumRows = 4}
    , file {fmRowGroups = V.singleton (group {rgNumRows = -1})}
    , file {fmNumRows = maxBound, fmRowGroups = V.fromList [group {rgNumRows = maxBound}, group]}
    ]
  test
    "stale file totals preserve historical group-sum behavior"
    (Agg.fileRowCount (file {fmNumRows = 0}) == 3)
  mapM_
    ( \bad ->
        test
          "negative row counts reject at footer boundary"
          (case Footer.readFooterRaw (Thrift.encodeCompact (Footer.fileMetadataToThrift bad)) of Left _ -> True; Right _ -> False)
    )
    [file {fmNumRows = -1}, file {fmRowGroups = V.singleton (group {rgNumRows = -1})}]
  let stale = file {fmNumRows = 0}
  test
    "stale totals remain available for data decoding and checked validation"
    (Footer.readFooterRaw (Thrift.encodeCompact (Footer.fileMetadataToThrift stale)) == Right stale)
  let huge = mkRgTyped PTInt32 maxBound (statsWithNullCount 0)
  test
    "empty groups need no null statistics to establish count zero"
    (Agg.columnNonNullCount (mkFm [mkRg 0 [(Just 0, Nothing)]]) 0 == Just 0)
  test
    "maximum signed64 row count remains exact"
    ( Agg.fileRowCountChecked (mkFm [huge]) == Just maxBound
        && Agg.columnNonNullCount (mkFm [huge]) 0 == Just maxBound
    )


mkRgTyped :: ParquetType -> Int64 -> Statistics -> RowGroup
mkRgTyped ty rows stats =
  let group = mkRg rows [(Just rows, Just stats)]
  in group
       { rgColumns =
           V.map
             (\column -> column {ccMetadata = (\metadata -> metadata {cmType = ty}) <$> ccMetadata column})
             (rgColumns group)
       }


data CountCase = CountCase
  { countRows :: {-# UNPACK #-} !Int64
  , countNulls :: {-# UNPACK #-} !Int64
  }
  deriving (Show)


-- An Integer oracle catches machine-integer overflow across generated groups.
countProperty :: IO ()
countProperty = do
  passed <- H.check $ H.withTests 100 $ H.property $ do
    cases <- H.forAll (Gen.nonEmpty (Range.linear 1 20) generate)
    let groups = (\c -> mkRgTyped PTInt32 (countRows c) (statsWithNullCount (countNulls c))) <$> NE.toList cases
        rows = sum (toInteger . countRows <$> cases)
        present = sum ((\c -> toInteger (countRows c) - toInteger (countNulls c)) <$> cases)
        fits = rows <= toInteger (maxBound :: Int64)
        metadata = (mkFm groups) {fmNumRows = if fits then fromInteger rows else maxBound}
    H.cover 5 "bounded total" fits
    H.cover 5 "overflowing total" (not fits)
    Agg.fileRowCountChecked metadata H.=== (if fits then Just (fromInteger rows) else Nothing)
    Agg.columnNonNullCount metadata 0 H.=== (if fits then Just (fromInteger present) else Nothing)
  if passed then pure () else error "FAIL: aggregate count property"
  where
    generate = do
      rows <- Gen.frequency [(3, Gen.int64 (Range.linear 0 1000000)), (1, Gen.int64 (Range.constant 0 maxBound))]
      nulls <- Gen.int64 (Range.constant 0 rows)
      pure (CountCase rows nulls)
