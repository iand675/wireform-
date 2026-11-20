module Delta.ParquetSchema (
  ColumnLevels (..),
  schemaLeaves,
  validateChunks,
  sparkGregorianCutover,
  rebaseSparkDate,
) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Calendar (addDays, diffDays, fromGregorian)
import Data.Time.Calendar.Julian (toJulian)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Parquet.Levels (ColumnLevels (..), schemaLeaves)
import Parquet.Types qualified as P


sparkGregorianCutover :: Int32
sparkGregorianCutover = -141427


-- Spark keeps Gregorian dates from 1582-10-15 onward. Julian-only leap
-- days roll forward to March 1, as in Spark's conversion from the first
-- day of the month. The rebased value stays within the DATE's Int32 range.
rebaseSparkDate :: Int32 -> Int32
rebaseSparkDate day
  | day >= sparkGregorianCutover = day
  | day < -719164 = rebaseEarlyDate day
  | otherwise = lookupOffset 0 (U.length dateOffsets)
  where
    lookupOffset !lo !hi
      | hi - lo == 1 = day + snd (U.unsafeIndex dateOffsets lo)
      | otherwise =
          let mid = (lo + hi) `quot` 2
          in if day < fst (U.unsafeIndex dateOffsets mid)
               then lookupOffset lo mid
               else lookupOffset mid hi
{-# INLINE rebaseSparkDate #-}


-- Spark's Julian-to-Gregorian switch points before the cutover.
-- https://github.com/apache/spark/blob/master/sql/api/src/main/scala/org/apache/spark/sql/catalyst/util/RebaseDateTime.scala
dateOffsets :: U.Vector (Int32, Int32)
dateOffsets = U.fromList
  [ (-719164, 2), (-682945, 1), (-646420, 0), (-609895, -1), (-536845, -2)
  , (-500320, -3), (-463795, -4), (-390745, -5), (-354220, -6), (-317695, -7)
  , (-244645, -8), (-208120, -9), (-171595, -10)
  ]


rebaseEarlyDate :: Int32 -> Int32
rebaseEarlyDate day =
  let epoch = fromGregorian 1970 1 1
      (year, month, dayOfMonth) = toJulian (addDays (toInteger day) epoch)
      rebased = addDays (toInteger dayOfMonth - 1) (fromGregorian year month 1)
  in fromInteger (diffDays rebased epoch)
{-# NOINLINE rebaseEarlyDate #-}


-- Readers address chunks by ordinal. Both the path and the physical type
-- must agree with the schema leaf at that ordinal.
validateChunks
  :: V.Vector P.SchemaElement
  -> V.Vector (V.Vector Text, ColumnLevels)
  -> V.Vector P.ColumnChunk
  -> Either String ()
validateChunks schema leaves = validate
  where
    types = V.mapMaybe P.seType schema
    count = V.length leaves
    validate chunks
      | V.length chunks /= count || V.length types /= count = Left "row-group column chunk count differs from schema"
      | otherwise = go 0
      where
        go !i
          | i == count = Right ()
          | Just path <- P.ccFilePath chunk, path /= "" =
              Left "external Parquet column chunks are unsupported"
          | Just metadata <- P.ccMetadata chunk = checkMetadata i metadata
          | otherwise = Left "column chunk has no metadata"
          where
            chunk = V.unsafeIndex chunks i
        checkMetadata i metadata
          | P.cmPathInSchema metadata /= fst (V.unsafeIndex leaves i) =
              Left ("column chunk path differs from schema at ordinal " ++ show i)
          | P.cmType metadata /= V.unsafeIndex types i =
              Left ("column chunk physical type differs from schema at ordinal " ++ show i)
          | otherwise = go (i + 1)
