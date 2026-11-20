{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}

-- | Calendar metadata from Spark-written Parquet files.
module Delta.SparkCalendar (
  SparkCalendar,
  sparkCalendar,
  legacyDates,
  legacyTimestamp,
  TimestampRebase,
  needsTimestampRebase,
  rebaseTimestamp,
  rebaseTimestampBound,
) where

import Control.Applicative ((<|>))
import Data.Aeson (eitherDecodeStrict', withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Map.Strict qualified as M
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Delta.ParquetSchema (rebaseSparkDate, sparkGregorianCutover)
import Language.Haskell.TH.Syntax (addDependentFile, lift, makeRelativeToProject, runIO)
import Parquet.Types qualified as P


data SparkCalendar = SparkCalendar
  { legacyDates :: !Bool
  , legacyInt96 :: !Bool
  , writerZone :: !(Maybe BS.ByteString)
  }


sparkCalendar :: P.FileMetadata -> SparkCalendar
sparkCalendar footer =
  SparkCalendar
    (has "org.apache.spark.legacyDateTime" || before (3, 0))
    (has "org.apache.spark.legacyINT96" || before (3, 1))
    (lookupMetadata "org.apache.spark.timeZone")
  where
    entries = maybe V.empty id (P.fmKeyValueMetadata footer)
    lookupMetadata name = V.find ((== name) . P.kvKey) entries >>= P.kvValue
    has name = V.any ((== name) . P.kvKey) entries
    sparkVersion = do
      version <- lookupMetadata "org.apache.spark.version"
      (major, rest) <- BSC.readInt version
      minorVersion <- BS.stripPrefix "." rest
      (minor, _) <- BSC.readInt minorVersion
      pure (major, minor)
    before bound = maybe False (\version -> fst version >= 1 && version < bound) sparkVersion


data TimestampRebase
  = FixedOffset !Int64
  | NamedZone !BS.ByteString ~(Maybe (U.Vector (Int64, Int64)))
  | UnknownZone !(Maybe BS.ByteString)


-- The two footer markers are independent. Spark only rebases zoned
-- timestamps, with a separate policy for the historical INT96 carrier.
legacyTimestamp :: Bool -> SparkCalendar -> Maybe TimestampRebase
legacyTimestamp int96 calendar
  | if int96 then legacyInt96 calendar else legacyDates calendar =
      Just $ case writerZone calendar of
        Just zone
          | Just offset <- fixedOffset zone -> FixedOffset offset
          | otherwise -> NamedZone zone (M.lookup zone sparkOffsets)
        unknown -> UnknownZone unknown
  | otherwise = Nothing


-- Normalize Java's fixed timezone aliases. Named zones are resolved against
-- Spark's published historical offsets, including seconds in local mean time.
fixedOffset :: BS.ByteString -> Maybe Int64
fixedOffset zone
  | zone `elem` ["UTC", "GMT", "UT", "Z"] = Just 0
  | zone == "EST" = Just (-5 * 3600)
  | zone == "MST" = Just (-7 * 3600)
  | zone == "HST" = Just (-10 * 3600)
  | otherwise = do
      let offset = maybe zone id (BS.stripPrefix "UTC" zone <|> BS.stripPrefix "GMT" zone <|> BS.stripPrefix "UT" zone)
      (sign, raw) <- BSC.uncons offset
      if sign == '+' || sign == '-' then pure () else Nothing
      (hours, minutes, seconds) <- case BSC.split ':' raw of
        [h]
          | BS.length h == 4 -> (,,0) <$> number (BS.take 2 h) <*> number (BS.drop 2 h)
          | otherwise -> (,,) <$> number h <*> pure 0 <*> pure 0
        [h, m] -> (,,0) <$> number h <*> number m
        [h, m, s] -> (,,) <$> number h <*> number m <*> number s
        _ -> Nothing
      let magnitude = hours * 3600 + minutes * 60 + seconds
      if minutes < 60 && seconds < 60 && magnitude <= 18 * 3600
        then Just ((if sign == '-' then negate else id) magnitude)
        else Nothing
  where
    number bytes
      | BS.length bytes <= 2 && BS.all (\byte -> byte >= 48 && byte <= 57) bytes
      , Just (value, rest) <- BSC.readInt bytes
      , BS.null rest =
          Just (fromIntegral value)
      | otherwise = Nothing


data RebaseRecord = RebaseRecord !BS.ByteString !(U.Vector (Int64, Int64))


sparkOffsets :: M.Map BS.ByteString (U.Vector (Int64, Int64))
sparkOffsets =
  case eitherDecodeStrict' sparkOffsetBytes >>= parseEither (traverse record) of
    Left err -> error ("Delta.SparkCalendar: invalid bundled Spark offsets: " ++ err)
    Right records -> M.fromList (map (\(RebaseRecord zone offsets) -> (zone, offsets)) records)
  where
    record = withObject "Spark timestamp offsets" $ \o -> do
      zone <- o .: "tz"
      switches <- o .: "switches"
      diffs <- o .: "diffs"
      if null switches
        || length switches /= length diffs
        || not (and (zipWith (<) switches (drop 1 switches)))
        || last switches * microsPerSecond > lastNamedSwitch
        || last diffs /= 0
        then fail "invalid switch table"
        else pure (RebaseRecord (TE.encodeUtf8 zone) (U.fromList (zip switches diffs)))
{-# NOINLINE sparkOffsets #-}


sparkOffsetBytes :: BS.ByteString
sparkOffsetBytes =
  $( do
       path <- makeRelativeToProject "data/spark-julian-gregorian-rebase-micros.json"
       addDependentFile path
       runIO (BS.readFile path) >>= lift
   )


microsPerSecond, microsPerDay :: Int64
microsPerSecond = 1_000_000
microsPerDay = 86_400_000_000


-- The latest nonzero offset in the Spark table ends at 1900-01-01 UTC.
needsTimestampRebase :: TimestampRebase -> Int64 -> Bool
needsTimestampRebase policy micros = micros < lastNamedSwitch && micros < cutover
  where
    cutover = case policy of
      FixedOffset offset -> fromIntegral sparkGregorianCutover * microsPerDay - offset * microsPerSecond
      NamedZone _ (Just offsets) -> fst (U.unsafeLast offsets) * microsPerSecond
      _ -> lastNamedSwitch
{-# INLINE needsTimestampRebase #-}


lastNamedSwitch :: Int64
lastNamedSwitch = -2_208_988_800 * microsPerSecond


rebaseTimestamp :: TimestampRebase -> Int64 -> (# String | Int64 #)
rebaseTimestamp policy micros
  | not (needsTimestampRebase policy micros) = (# | micros #)
  | otherwise = case policy of
      FixedOffset offset -> rebaseAtOffsets offset 0 micros
      NamedZone _ (Just offsets)
        | micros >= fst (U.unsafeHead offsets) * microsPerSecond ->
            let seconds = micros `div` microsPerSecond
            in addOffset micros (offsetAt offsets seconds * microsPerSecond)
        | otherwise -> rebaseBeforeTable offsets micros
      NamedZone zone Nothing -> unknownWriterZone (Just zone)
      UnknownZone zone -> unknownWriterZone zone
  where
    unknownWriterZone zone =
      (# "Delta.Scan: Spark legacy timestamp requires a known writer timezone" ++ maybe "" ((": " ++) . BSC.unpack) zone | #)
{-# INLINE rebaseTimestamp #-}


-- Spark's first switch is Julian 0001-01-01 at local midnight. Before
-- then Java uses the same raw standard offset for the hybrid calendar
-- and the zone's earliest offset for proleptic Gregorian. The first
-- switch and its correction recover both offsets from the pinned table.
rebaseBeforeTable :: U.Vector (Int64, Int64) -> Int64 -> (# String | Int64 #)
rebaseBeforeTable offsets micros =
  let (first, correction) = U.unsafeHead offsets
      julianDayOne = -719164
      rawOffset = julianDayOne * 86400 - first
      zoneAdjustment = correction - 2 * 86400
  in rebaseAtOffsets rawOffset zoneAdjustment micros
{-# NOINLINE rebaseBeforeTable #-}


rebaseAtOffsets :: Int64 -> Int64 -> Int64 -> (# String | Int64 #)
rebaseAtOffsets rawOffset zoneAdjustment micros =
  let (day, fraction) = micros `divMod` microsPerDay
      localDay = day + (fraction + rawOffset * microsPerSecond) `div` microsPerDay
      days = fromIntegral (rebaseSparkDate (fromIntegral localDay)) - localDay
  in addOffset micros (days * microsPerDay + zoneAdjustment * microsPerSecond)
{-# INLINE rebaseAtOffsets #-}


-- Offset lookups use an allocation-free search over a small, ordered vector.
offsetAt :: U.Vector (Int64, Int64) -> Int64 -> Int64
offsetAt offsets seconds = search 0 (U.length offsets)
  where
    search !lo !hi
      | hi - lo == 1 = snd (U.unsafeIndex offsets lo)
      | otherwise =
          let mid = (lo + hi) `quot` 2
          in if seconds < fst (U.unsafeIndex offsets mid) then search lo mid else search mid hi
{-# INLINE offsetAt #-}


addOffset :: Int64 -> Int64 -> (# String | Int64 #)
addOffset value extra
  | extra > 0 && value > maxBound - extra = (# overflow | #)
  | extra < 0 && value < minBound - extra = (# overflow | #)
  | otherwise = (# | value + extra #)
  where
    overflow = "Delta.Scan: rebased Spark timestamp exceeds Int64 microseconds"
{-# INLINE addOffset #-}


-- Timestamp offsets can jump at calendar or timezone transitions. Use
-- the extreme value on the relevant side of each switch when converting
-- a file bound. Outside the table's coverage, the bound can't prove that
-- skipping a file is safe.
rebaseTimestampBound :: Bool -> TimestampRebase -> Int64 -> Maybe Int64
rebaseTimestampBound upper policy value
  | value >= lastNamedSwitch + (if upper then 2 * microsPerDay else 0) = Just value
  | otherwise = case rebaseTimestamp policy value of
      (# _ | #) -> Nothing
      (# | converted #) -> case policy of
        NamedZone _ (Just offsets)
          -- Preserve scan values outside the switch table, but omit
          -- these pruning bounds, including a margin across its start.
          | value < fst (U.unsafeHead offsets) * microsPerSecond + 2 * microsPerDay -> Nothing
          | otherwise -> Just (U.ifoldl' (extend offsets) converted offsets)
        FixedOffset offset
          | value < lastNamedSwitch -> fixedBound offset converted
        _ | upper && value >= lastNamedSwitch -> Nothing
        _ -> Just converted
  where
    -- A Julian leap day in a Gregorian common year can map to the
    -- same civil day as its successor. Compare the adjacent day's
    -- boundary, which is the only possible reversal in this calendar.
    fixedBound offset converted =
      let fraction = (value `mod` microsPerDay + offset * microsPerSecond) `mod` microsPerDay
          delta = if upper then negate (fraction + 1) else microsPerDay - fraction
          extreme = if upper then max else min
      in case addOffset value delta of
           (# _ | #) -> Just converted
           (# | adjacent #) -> case rebaseTimestamp policy adjacent of
             (# _ | #) -> Nothing
             (# | rebased #) -> Just (extreme converted rebased)
    extend offsets current index (seconds, nextOffset)
      | upper
      , index > 0
      , switch <= value =
          let previousOffset = snd (U.unsafeIndex offsets (index - 1))
          in max current (switch - 1 + previousOffset * microsPerSecond)
      | not upper, switch > value = min current (switch + nextOffset * microsPerSecond)
      | otherwise = current
      where
        switch = seconds * microsPerSecond
