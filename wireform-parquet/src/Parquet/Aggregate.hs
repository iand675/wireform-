{-# LANGUAGE LambdaCase #-}

{- | Aggregate pushdown for Parquet.

Many analytical queries reduce to one of:

  * @count(*)@ — number of rows in the whole file.
  * @count(col)@ — number of non-null rows in a column.
  * @min/max(col)@ — exact extrema of a column's defined values.

Counts and exact extrema can be answered from validated footer metadata
when the necessary statistics are available. Compact bounds are useful
for pruning, but cannot establish an exact aggregate. Floating-point
extrema require a known zero NaN count; scan when another NaN policy is
needed. Repeated leaves cannot supply a non-null /row/ count.

These functions trust the supplied statistics after checking their shape;
callers needing to verify a writer's claims must scan the data.
-}
module Parquet.Aggregate (
  -- * count(*)
  fileRowCount,
  fileRowCountChecked,
  rowGroupRowCount,

  -- * count(col)
  columnNonNullCount,

  -- * min / max
  columnMin,
  columnMax,

  -- * Note on @sum(col)@
  -- $sum
) where

-- \$sum
--
-- Parquet's @Statistics@ message intentionally doesn't carry
-- per-row-group sums (the spec authors decided sums are too
-- easy to overflow + lose precision to be safely cached). To
-- compute @sum(col)@ from a Parquet file you have to scan the
-- column values; there is no pushdown helper.
--
-- ORC /does/ carry @IntegerStatistics.sum@ — see
-- "ORC.Aggregate.columnSum" for the parallel helper that
-- works with ORC files.

import Columnar.Predicate qualified as Pred
import Control.Monad (guard)
import Data.Int (Int64)
import Data.Vector qualified as V
import Parquet.Levels qualified as Levels
import Parquet.Predicate qualified as PPred
import Parquet.Types qualified as P


{- | Sum of the row-group counts, retaining compatibility with older files
whose file-wide total is stale. Negative or overflowing group counts raise
an error; 'fileRowCountChecked' safely checks both the groups and file total.
-}
fileRowCount :: P.FileMetadata -> Int64
fileRowCount = maybe (error "Parquet.Aggregate: negative or overflowing row counts") id . rowGroupSum


-- | Reject negative, overflowing or inconsistent file/row-group counts.
fileRowCountChecked :: P.FileMetadata -> Maybe Int64
fileRowCountChecked fm = do
  total <- rowGroupSum fm
  guard (total == P.fmNumRows fm)
  Just total


rowGroupSum :: P.FileMetadata -> Maybe Int64
rowGroupSum fm = V.foldM addRows 0 (P.fmRowGroups fm)
  where
    addRows total group = do
      let rows = P.rgNumRows group
      guard (rows >= 0 && rows <= maxBound - total)
      Just (total + rows)


-- | Row count of one row group.
rowGroupRowCount :: P.RowGroup -> Int64
rowGroupRowCount = P.rgNumRows


{- | @count(col)@ — non-null rows for a non-repeated leaf, including
nullable ancestors. Returns 'Nothing' for repeated paths, missing counts,
or inconsistent metadata. Definition events equal rows only for paths
without repeated ancestors.
-}
columnNonNullCount :: P.FileMetadata -> Int -> Maybe Int64
columnNonNullCount fm colIdx = do
  _ <- fileRowCountChecked fm
  context <- PPred.statisticsContext fm colIdx
  V.foldM (step context) 0 (P.fmRowGroups fm)
  where
    step context acc group = do
      column <- P.rgColumns group V.!? colIdx
      metadata <- P.ccMetadata column
      guard (P.seType (PPred.statisticsField context) == Just (P.cmType metadata))
      (maxRep, maxDef) <-
        either
          (const Nothing)
          Just
          (Levels.maxLevelsForColumnPath (P.fmSchema fm) (P.cmPathInSchema metadata))
      guard (maxRep == 0 && P.cmNumValues metadata == P.rgNumRows group)
      if P.rgNumRows group == 0
        then Just acc
        else do
          statistics <- P.cmStatistics metadata
          nulls <- P.statNullCount statistics
          guard (nulls >= 0 && nulls <= P.rgNumRows group && (maxDef /= 0 || nulls == 0))
          let present = P.rgNumRows group - nulls
          guard (acc <= maxBound - present)
          Just (acc + present)


{- | Minimum value of a column across the whole file. Reads
each row group's @min_value@ (or legacy @min@) statistic
and folds them under the 'PValue' ordering. Returns
'Nothing' if /any/ contributing row group lacks min stats —
otherwise the result might be the min over a strict subset
of the row groups.
-}
columnMin :: P.FileMetadata -> Int -> Maybe Pred.PValue
columnMin = aggCmp takeMinStat (\acc v -> if Pred.pvLess v acc then v else acc)


{- | Maximum value of a column across the whole file. Same
soundness rule as 'columnMin': any row group without max
stats poisons the result to 'Nothing'.
-}
columnMax :: P.FileMetadata -> Int -> Maybe Pred.PValue
columnMax = aggCmp takeMaxStat (\acc v -> if Pred.pvLess acc v then v else acc)


{- | Common driver for column-wise stat aggregation. Polls the
supplied stat extractor on every row group; if any returns
'Nothing' the whole result is 'Nothing' (we can't be sound
about a min / max otherwise).
-}
aggCmp
  :: (PPred.StatisticsContext -> P.Statistics -> Maybe Pred.PValue)
  -> (Pred.PValue -> Pred.PValue -> Pred.PValue)
  -> P.FileMetadata
  -> Int
  -> Maybe Pred.PValue
aggCmp takeStat combine fm colIdx = do
  _ <- fileRowCountChecked fm
  context <- PPred.statisticsContext fm colIdx
  V.foldM (step context) Nothing (P.fmRowGroups fm) >>= id
  where
    step context acc group = do
      column <- P.rgColumns group V.!? colIdx
      metadata <- P.ccMetadata column
      guard (P.seType (PPred.statisticsField context) == Just (P.cmType metadata))
      (maxRep, _) <-
        either
          (const Nothing)
          Just
          (Levels.maxLevelsForColumnPath (P.fmSchema fm) (P.cmPathInSchema metadata))
      guard
        ( if maxRep == 0
            then P.cmNumValues metadata == P.rgNumRows group
            else P.cmNumValues metadata >= P.rgNumRows group
        )
      if P.rgNumRows group == 0
        then do
          guard (P.cmNumValues metadata == 0)
          Just acc
        else do
          statistics <- P.cmStatistics metadata
          guard (maybe True (\n -> n >= 0 && n <= P.cmNumValues metadata) (P.statNullCount statistics))
          if P.statNullCount statistics == Just (P.cmNumValues metadata)
            then Just acc
            else do
              guard (nanFree (PPred.statisticsField context) statistics)
              value <- takeStat context statistics
              Just (Just (maybe value (`combine` value) acc))

    nanFree field statistics
      | P.seType field `elem` [Just P.PTFloat, Just P.PTDouble]
          || P.seLogicalType field == Just P.LTFloat16 =
          P.statNanCount statistics == Just 0
      | otherwise = True


takeMinStat :: PPred.StatisticsContext -> P.Statistics -> Maybe Pred.PValue
takeMinStat context statistics =
  PPred.minimumStatistic
    context
    (if P.statMinValueExact statistics == Just True then statistics else statistics {P.statMinValue = Nothing})


takeMaxStat :: PPred.StatisticsContext -> P.Statistics -> Maybe Pred.PValue
takeMaxStat context statistics =
  PPred.maximumStatistic
    context
    (if P.statMaxValueExact statistics == Just True then statistics else statistics {P.statMaxValue = Nothing})
