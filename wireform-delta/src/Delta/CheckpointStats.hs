{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}

{- | Projection and decoding of the optional structured statistics in
Delta Parquet checkpoints. Keys are physical column names.
-}
module Delta.CheckpointStats (
  StatsProjection,
  isKnownStatistic,
  statsProjection,
  readStats,
) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Monad (unless)
import Control.Monad.ST (runST)
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.HashMap.Internal qualified as HMI
import Data.HashMap.Strict qualified as HM
import Data.Int (Int64)
import Data.List (find)
import Data.Scientific (scientific)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (addDays, defaultTimeLocale, formatTime, fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Storable qualified as VS
import Data.Word (Word64)
import Delta.Log (AddStats (..), DeltaField (..), DeltaSchema (..), DeltaType (..), NullCount (..), parseDeltaSchema)
import Delta.ParquetSchema (rebaseSparkDate)
import Delta.SparkCalendar (SparkCalendar, TimestampRebase, legacyDates, legacyTimestamp, rebaseTimestampBound, sparkCalendar)
import Delta.Z85 (encodeZ85)
import GHC.Exts (Int (I#), Int#)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Parquet.Arrow qualified as PA
import Parquet.Read qualified as PR
import Parquet.Timestamp qualified as Timestamp
import Parquet.Types qualified as P
import Parquet.Variant qualified as Variant
import Parquet.Variant.Arrow qualified as VA
import Parquet.Variant.Schema qualified as VSchem


data StatsProjection = StatsProjection !PA.ParquetArrowSubtree !A.Field !SparkCalendar


statsProjection :: PR.ParquetFile -> Either String (Maybe StatsProjection)
statsProjection parquet
  | not (V.any hasStats groups) = Right Nothing
  | otherwise = do
      selected <- PA.parquetArrowSubtreeChildrenWithLegacyVariants parquet statsPath isKnownStatistic (sparkVariantBounds footer)
      traverse parseProjection selected
  where
    footer = PR.pfFooter parquet
    groups = P.fmRowGroups footer
    statsPath = V.fromList ["add", "stats_parsed"]
    hasStats = V.any isStatsColumn . P.rgColumns
    isStatsColumn chunk = case P.ccMetadata chunk of
      Just m -> let path = P.cmPathInSchema m in V.length path >= 2 && path V.! 0 == "add" && path V.! 1 == "stats_parsed"
      Nothing -> False

    parseProjection projected =
      case A.arrowFields (PA.parquetArrowSubtreeSchema projected) V.!? 0 of
        Just add
          | A.fieldType add == A.AStruct
          , Just stats <- A.fieldChildren add V.!? 0
          , A.fieldType stats == A.AStruct -> Right (StatsProjection projected stats (sparkCalendar footer))
        _ -> Left "Delta.Checkpoint: stats_parsed must be a struct"


-- Spark records its original logical schema in the footer even when it
-- writes Variants as unannotated Parquet groups. In a checkpoint the
-- logical schema already uses the physical names of the table columns.
sparkVariantBounds :: P.FileMetadata -> A.Schema -> [[T.Text]]
sparkVariantBounds footer = case sparkSchema of
  Nothing -> const []
  Just schemaBytes -> \schema ->
    case candidatePaths schema of
      [] -> []
      paths -> case parseSparkSchema schemaBytes of
        Nothing -> []
        Just (DeltaSchema roots) -> filter (isVariant roots) paths
  where
    sparkSchema = do
      entries <- P.fmKeyValueMetadata footer
      entry <- V.find ((== "org.apache.spark.sql.parquet.row.metadata") . P.kvKey) entries
      P.kvValue entry
    parseSparkSchema bytes = do
      json <- either (const Nothing) Just (TE.decodeUtf8' bytes)
      either (const Nothing) Just (parseDeltaSchema json)
    candidatePaths schema = case child "add" (A.arrowFields schema) >>= child "stats_parsed" . A.fieldChildren of
      Just stats -> V.foldr collectBound [] (A.fieldChildren stats)
      Nothing -> []
    child name = V.find ((== name) . A.fieldName)
    collectBound field rest
      | name == "minValues" || name == "maxValues" =
          collect [name, "stats_parsed", "add"] (A.fieldChildren field) rest
      | otherwise = rest
      where
        name = A.fieldName field
    collect parent fields rest = V.foldr visit rest fields
      where
        visit field tailPaths
          | A.fieldType field /= A.AStruct || VSchem.isVariantField field = tailPaths
          | Just metadata <- child "metadata" children
          , A.fieldType metadata == A.ABinary
          , V.any (\f -> A.fieldName f == "value" || A.fieldName f == "typed_value") children =
              reverse path : collect path children tailPaths
          | otherwise = collect path children tailPaths
          where
            path = A.fieldName field : parent
            children = A.fieldChildren field
    isVariant fields (name : path) = case dfType <$> find ((== name) . dfName) fields of
      Just (DTStruct children) -> isVariant children path
      Just DTVariant -> null path
      _ -> False
    isVariant _ [] = False


-- Only the immediate children have protocol field names. Names within
-- bounds and counts are table columns and are kept intact.
isKnownStatistic :: T.Text -> Bool
isKnownStatistic name = name `elem` ["numRecords", "tightBounds", "minValues", "maxValues", "nullCount"]


readStats
  :: Maybe StatsProjection
  -> Int
  -> Bool
  -> (Int# -> Bool)
  -> (Int -> Bool)
  -> Either String (V.Vector (Maybe AddStats))
readStats Nothing _ _ _ _ = Right V.empty
readStats (Just (StatsProjection projected field calendar)) groupIndex allAdds hasAdd needsStats = do
  columns <- either (Left . show) Right (PA.readParquetArrowSubtree projected groupIndex)
  case columns V.!? 0 of
    Just (C.ColStruct _ addValidity addFields)
      | Just (_, C.ColStruct rows statsValidity statsFields) <- addFields V.!? 0 -> do
          -- Parquet's subtree decoder already checks that the projected
          -- siblings agree on their add ancestor. Check its mask against
          -- the action marker without rereading those columns.
          let checkRows !i@(I# index) =
                i == rows || ((not (C.isValidAt addValidity i) || hasAdd index) && checkRows (i + 1))
          unless (allAdds || checkRows 0) $
            Left "Delta.Checkpoint: invalid action row, structured stats outside add"
          let children = A.fieldChildren field
              column name = snd <$> V.find ((== name) . fst) statsFields
              bounds name = case (V.find ((== name) . A.fieldName) children, column name) of
                (Just f, Just c) -> objectBounds calendar (name == "maxValues") f c
                _ -> pure (\_ -> (# HM.empty | | #))
              counts = maybe (Right (\_ -> (# HM.empty | | #))) objectCounts (column "nullCount")
          minValues <- bounds "minValues"
          maxValues <- bounds "maxValues"
          nullCounts <- counts
          numRecords <- traverse countReader (column "numRecords")
          tightBounds <- traverse booleanReader (column "tightBounds")
          let recordCount i = case numRecords of
                Nothing -> (# Nothing | #)
                Just get -> case get i of
                  (# value | | #) -> (# Just value | #)
                  (# | _ | #) -> (# Nothing | #)
                  (# | | err #) -> (# | err #)
              sameRecordCount = maybe (\_ _ -> True) equalCells (column "numRecords")
              sameNullCounts = maybe (\_ _ -> True) equalCells (column "nullCount")
              -- Counts can stay constant even when the row's bounds change.
              readRow previous cached i =
                case records of
                  (# n | #) -> case readMap minValues i of
                    (# mn | #) -> case readMap maxValues i of
                      (# mx | #) -> case cached of
                        Just stats | sameNullCounts previous i ->
                          (# AddStats n mn mx (asNullCount stats) (asNullCounts stats) tb | #)
                        _ -> case readMap nullCounts i of
                          (# nc | #) -> (# AddStats n mn mx (HM.mapMaybe scalarCount nc) nc tb | #)
                          (# | err #) -> (# | err #)
                      (# | err #) -> (# | err #)
                    (# | err #) -> (# | err #)
                  (# | err #) -> (# | err #)
                where
                  records = case cached of
                    Just stats | sameRecordCount previous i -> (# asNumRecords stats | #)
                    _ -> recordCount i
                  tb = tightBounds >>= (\get -> get i)
              present i = needsStats i && C.isValidAt addValidity i && C.isValidAt statsValidity i
              sameMin = maybe (\_ _ -> True) equalCells (column "minValues")
              sameMax = maybe (\_ _ -> True) equalCells (column "maxValues")
              sameTight = maybe (\_ _ -> True) equalCells (column "tightBounds")
              -- Compare only the fields the decoder uses. Bounds are
              -- usually the first to differ between files.
              same previous i =
                sameMin previous i && sameMax previous i
                  && sameRecordCount previous i && sameNullCounts previous i && sameTight previous i
          generateStats rows present same readRow
    _ -> Left "Delta.Checkpoint: invalid add.stats_parsed projection"


type ReadCell a = Int -> (# a | () | String #)


readMap :: ReadCell (HM.HashMap T.Text a) -> Int -> (# HM.HashMap T.Text a | String #)
readMap readCell i = case readCell i of
  (# entries | | #) -> (# entries | #)
  (# | _ | #) -> (# HM.empty | #)
  (# | | err #) -> (# | err #)
{-# INLINE readMap #-}


data NamedReader a = NamedReader !T.Text {-# UNPACK #-} !HMI.Hash !(ReadCell a)


-- Field names are fixed by the projected schema, so hash each one once.
namedReader :: T.Text -> ReadCell a -> NamedReader a
namedReader name = NamedReader name (HMI.hash name)


-- Reuse adjacent equal statistics without allocating new maps or
-- converting numbers to JSON. Compare only valid cells. Unsupported
-- comparisons simply decode that row as usual.
generateStats
  :: Int
  -> (Int -> Bool)
  -> (Int -> Int -> Bool)
  -> (Int -> Maybe AddStats -> Int -> (# AddStats | String #))
  -> Either String (V.Vector (Maybe AddStats))
generateStats rows present same readRow = runST $ do
  out <- VM.unsafeNew rows
  let loop !i !previous !cached
        | i == rows = Right <$> V.unsafeFreeze out
        | not (present i) = VM.unsafeWrite out i Nothing >> loop (i + 1) previous cached
        | previous >= 0 && same previous i = VM.unsafeWrite out i cached >> loop (i + 1) previous cached
        | otherwise = case readRow previous cached i of
            (# | err #) -> pure (Left err)
            (# !stats | #) ->
              let !next = Just stats
              in VM.unsafeWrite out i next >> loop (i + 1) i next
  loop 0 (-1) Nothing
{-# INLINE generateStats #-}


equalStruct :: V.Vector (T.Text, C.ColumnArray) -> Int -> Int -> Bool
equalStruct columns =
  let tests = V.map (equalCells . snd) columns
      compareChildren i j !index =
        index == V.length tests || (V.unsafeIndex tests index i j && compareChildren i j (index + 1))
  in \i j -> compareChildren i j 0


equalCells :: C.ColumnArray -> Int -> Int -> Bool
equalCells column = case column of
  C.ColStruct _ valid children -> withValidity valid (equalStruct children)
  C.ColInt8 valid values -> equalPrimitive valid id values
  C.ColInt16 valid values -> equalPrimitive valid id values
  C.ColInt32 valid values -> equalPrimitive valid id values
  C.ColInt64 valid values -> equalPrimitive valid id values
  C.ColUInt8 valid values -> equalPrimitive valid id values
  C.ColUInt16 valid values -> equalPrimitive valid id values
  C.ColUInt32 valid values -> equalPrimitive valid id values
  C.ColUInt64 valid values -> equalPrimitive valid id values
  C.ColFloat valid values -> equalPrimitive valid castFloatToWord32 values
  C.ColDouble valid values -> equalPrimitive valid castDoubleToWord64 values
  C.ColDate32 valid values -> equalPrimitive valid id values
  C.ColDate64 valid values -> equalPrimitive valid id values
  C.ColTimestamp valid values -> equalPrimitive valid id values
  C.ColDecimal32 _ _ valid values -> equalPrimitive valid id values
  C.ColDecimal64 _ _ valid values -> equalPrimitive valid id values
  C.ColDecimal128 _ _ valid values -> equalPrimitive valid id values
  C.ColDecimal256 _ _ valid values -> equalPrimitive valid id values
  C.ColBool {} -> \i j -> C.boolAt column i == C.boolAt column j
  C.ColUtf8 {} -> equalBytes
  C.ColLargeUtf8 {} -> equalBytes
  C.ColUtf8View {} -> equalBytes
  _ -> withValidity (C.validity column) (\_ _ -> False)
  where
    equalBytes i j = C.anyBytesAt column i == C.anyBytesAt column j


equalPrimitive :: (VS.Storable a, Eq b) => Maybe C.Validity -> (a -> b) -> VS.Vector a -> Int -> Int -> Bool
equalPrimitive valid key values = withValidity valid $ \i j -> key (VS.unsafeIndex values i) == key (VS.unsafeIndex values j)
{-# INLINE equalPrimitive #-}


withValidity :: Maybe C.Validity -> (Int -> Int -> Bool) -> Int -> Int -> Bool
withValidity Nothing equal = equal
withValidity valid equal = \i j ->
  if C.isValidAt valid i
    then C.isValidAt valid j && equal i j
    else not (C.isValidAt valid j)
{-# INLINE withValidity #-}


objectBounds :: SparkCalendar -> Bool -> A.Field -> C.ColumnArray -> Either String (ReadCell (HM.HashMap T.Text JSON.Value))
objectBounds calendar upper field (C.ColStruct _ valid columns) = do
  readers <- boundReaders calendar upper namedReader field columns
  pure (readObject valid readers)
objectBounds _ _ _ _ = Left "Delta.Checkpoint: bounds must be structs"


boundReaders :: SparkCalendar -> Bool -> (T.Text -> ReadCell JSON.Value -> reader) -> A.Field -> V.Vector (T.Text, C.ColumnArray) -> Either String (V.Vector reader)
boundReaders calendar upper nameReader field columns
  | V.length children /= V.length columns = mismatch
  | otherwise = V.zipWithM compile children columns
  where
    children = A.fieldChildren field
    -- The subtree reader assembles columns in schema order.
    compile child (name, column)
      | A.fieldName child == name = nameReader name <$> valueReader calendar upper child column
      | otherwise = mismatch
    mismatch = Left "Delta.Checkpoint: statistics columns disagree with schema"


objectCounts :: C.ColumnArray -> Either String (ReadCell (HM.HashMap T.Text NullCount))
objectCounts (C.ColStruct _ valid columns) = do
  readers <- V.mapM (\(name, column) -> namedReader name <$> count column) columns
  pure (readObject valid readers)
  where
    count child@C.ColStruct {} = do
      nested <- objectCounts child
      pure $ \i -> case nested i of
        (# counts | | #) -> (# NestedNullCounts counts | | #)
        (# | _ | #) -> (# | () | #)
        (# | | err #) -> (# | | err #)
    count child = do
      readCount <- countReader child
      pure $ \i -> case readCount i of
        (# value | | #) -> (# NullCount value | | #)
        (# | _ | #) -> (# | () | #)
        (# | | err #) -> (# | | err #)
objectCounts _ = Left "Delta.Checkpoint: nullCount must be a struct"


readObject :: Maybe C.Validity -> V.Vector (NamedReader a) -> ReadCell (HM.HashMap T.Text a)
readObject validity readers
  | V.null readers = \i -> if C.isValidAt validity i then (# HM.empty | | #) else (# | () | #)
  | V.length readers == 1, NamedReader name hash readValue <- V.unsafeHead readers = \i ->
      if C.isValidAt validity i
        then case readValue i of
          (# value | | #) -> value `seq` (# HMI.insert' hash name value HM.empty | | #)
          (# | _ | #) -> (# HM.empty | | #)
          (# | | err #) -> (# | | err #)
        else (# | () | #)
  | otherwise = \i ->
      if C.isValidAt validity i then collectReaders readers i else (# | () | #)


collectReaders :: V.Vector (NamedReader a) -> ReadCell (HM.HashMap T.Text a)
collectReaders readers row = go 0 HM.empty
  where
    go !index !fields
      | index == V.length readers = (# fields | | #)
      | otherwise = case V.unsafeIndex readers index of
          NamedReader name hash readValue -> case readValue row of
            (# value | | #) -> value `seq` go (index + 1) (HMI.insert' hash name value fields)
            (# | _ | #) -> go (index + 1) fields
            (# | | err #) -> (# | | err #)


scalarCount :: NullCount -> Maybe Word64
scalarCount (NullCount n) = Just n
scalarCount (NestedNullCounts _) = Nothing


countReader :: C.ColumnArray -> Either String (ReadCell Word64)
countReader (C.ColInt64 valid values) = pure $ \i ->
  if not (C.isValidAt valid i)
    then (# | () | #)
    else
      let n = VS.unsafeIndex values i
      in if n < 0
           then (# | | "Delta.Checkpoint: negative statistics count" #)
           else
             let !count = fromIntegral n
             in (# count | | #)
countReader _ = Left "Delta.Checkpoint: statistics counts must be INT64"


booleanReader :: C.ColumnArray -> Either String (Int -> Maybe Bool)
booleanReader column@(C.ColBool _ _) = pure (C.boolAt column)
booleanReader _ = Left "Delta.Checkpoint: tightBounds must be BOOLEAN"


valueReader :: SparkCalendar -> Bool -> A.Field -> C.ColumnArray -> Either String (ReadCell JSON.Value)
valueReader calendar upper field column
  | VSchem.isVariantField field = do
      reader <- VA.compileVariantArrowReader field column
      pure $ \i -> case VA.variantArrowAt reader i of
        Right (Just value) -> case variantJSON value of
          Right encoded -> (# encoded | | #)
          Left err -> (# | | err #)
        Right Nothing -> (# | () | #)
        Left err -> (# | | err #)
  | otherwise = case column of
      C.ColStruct _ valid columns -> do
        readers <- boundReaders calendar upper (\name readCell -> (Key.fromText name, readCell)) field columns
        pure (nestedBound valid readers)
      C.ColBool _ _ -> pure $ \i -> case C.boolAt column i of
        Just value -> (# JSON.Bool value | | #)
        Nothing -> (# | () | #)
      C.ColInt8 valid values -> pure (primitive valid integral values)
      C.ColInt16 valid values -> pure (primitive valid integral values)
      C.ColInt32 valid values -> pure (primitive valid integral values)
      C.ColInt64 valid values -> pure (primitive valid integral values)
      C.ColUInt8 valid values -> pure (primitive valid integral values)
      C.ColUInt16 valid values -> pure (primitive valid integral values)
      C.ColUInt32 valid values -> pure (primitive valid integral values)
      C.ColUInt64 valid values -> pure (primitive valid integral values)
      C.ColFloat valid values -> pure (primitive valid real values)
      C.ColDouble valid values -> pure (primitive valid real values)
      C.ColDate32 valid values -> pure $
        if legacyDates calendar
          then primitive valid (JSON.String . dateText . fromIntegral . rebaseSparkDate) values
          else primitive valid (JSON.String . dateText . fromIntegral) values
      C.ColDate64 valid values -> pure (primitive valid (JSON.String . dateText . (`div` 86400000) . fromIntegral) values)
      C.ColTimestamp valid values -> case A.fieldType field of
        A.ATimestamp unit tz -> pure $ case tz >> legacyTimestamp False calendar of
          Nothing -> primitive valid (JSON.String . timestampText unit tz . toInteger) values
          Just rebase -> \i ->
            if C.isValidAt valid i
              then sparkBound upper rebase unit (toInteger (VS.unsafeIndex values i))
              else (# | () | #)
        _ -> Left "Delta.Checkpoint: timestamp statistics missing units"
      C.ColFixedSizeBinary 12 _ _ _ -> do
        encoding <- Timestamp.timestampEncoding field
        pure $ case encoding of
          Just Timestamp.JulianInt96 ->
            timestampBound (case legacyTimestamp True calendar of
              -- Spark's INT96 carries a zoned timestamp. Outside Spark,
              -- INT96 alone does not establish timezone semantics.
              Just rebase -> sparkBound upper rebase A.Nanosecond
              Nothing -> plainBound A.Nanosecond Nothing) Timestamp.decodeJulianInt96 column
          Just (Timestamp.SignedTimestamp96 adjusted unit) ->
            let arrowUnit = case unit of
                  P.LtMillis -> A.Millisecond
                  P.LtMicros -> A.Microsecond
                  P.LtNanos -> A.Nanosecond
                zone = if adjusted then Just "UTC" else Nothing
            in timestampBound (plainBound arrowUnit zone) Timestamp.decodeTimestamp96 column
          Nothing -> \_ -> (# | () | #)
      C.ColDecimal32 _ scale valid values -> pure (primitive valid (decimal scale . toInteger) values)
      C.ColDecimal64 _ scale valid values -> pure (primitive valid (decimal scale . toInteger) values)
      C.ColDecimal128 _ scale valid values -> pure (primitive valid (decimal scale . C.decimal128ToInteger) values)
      C.ColDecimal256 _ scale valid values -> pure (primitive valid (decimal scale . C.decimal256ToInteger) values)
      C.ColUtf8 {} -> pure textCell
      C.ColLargeUtf8 {} -> pure textCell
      C.ColUtf8View {} -> pure textCell
      -- Binary, arrays and maps are not Delta min/max statistics types.
      _ -> pure (\_ -> (# | () | #))
  where
    -- The Parquet-to-Arrow bridge already validated UTF-8 and offsets.
    textCell i = case C.anyTextAt column i of
      Just text -> (# JSON.String text | | #)
      Nothing -> (# | () | #)


nestedBound :: Maybe C.Validity -> V.Vector (Key.Key, ReadCell JSON.Value) -> ReadCell JSON.Value
nestedBound valid readers row
  | C.isValidAt valid row = go 0 KM.empty
  | otherwise = (# | () | #)
  where
    go !index !entries
      | index == V.length readers = (# JSON.Object entries | | #)
      | (key, readCell) <- V.unsafeIndex readers index = case readCell row of
          (# value | | #) -> value `seq` go (index + 1) (KM.insert key value entries)
          (# | _ | #) -> go (index + 1) entries
          (# | | err #) -> (# | | err #)


primitive :: VS.Storable a => Maybe C.Validity -> (a -> JSON.Value) -> VS.Vector a -> ReadCell JSON.Value
primitive valid convert values i
  | C.isValidAt valid i =
      let !value = convert (VS.unsafeIndex values i)
      in (# value | | #)
  | otherwise = (# | () | #)
{-# INLINE primitive #-}


plainBound :: A.TimeUnit -> Maybe T.Text -> Integer -> (# JSON.Value | () | String #)
plainBound unit zone value = (# JSON.String (timestampText unit zone value) | | #)


sparkBound :: Bool -> TimestampRebase -> A.TimeUnit -> Integer -> (# JSON.Value | () | String #)
sparkBound upper policy unit raw
  | value < toInteger (minBound :: Int64) || value > toInteger (maxBound :: Int64) = (# | () | #)
  | otherwise = case rebaseTimestampBound upper policy (fromInteger value) of
      Nothing -> (# | () | #)
      Just micros -> plainBound A.Microsecond (Just "UTC") (toInteger micros)
  where
    value = case unit of
      A.Second -> raw * 1000000 + if upper then 999999 else 0
      A.Millisecond -> raw * 1000 + if upper then 999 else 0
      A.Microsecond -> raw
      -- Exact Delta microseconds are the only representable scan values.
      A.Nanosecond -> raw `div` 1000


timestampBound
  :: (Integer -> (# JSON.Value | () | String #))
  -> (ByteString -> Either String Timestamp.Timestamp96)
  -> C.ColumnArray
  -> ReadCell JSON.Value
timestampBound render decode column i = case C.anyBytesAt column i of
  Nothing -> (# | () | #)
  Just bytes -> case decode bytes of
    Left err -> (# | | err #)
    Right value -> render (Timestamp.timestamp96ToInteger value)


integral :: Integral a => a -> JSON.Value
integral = JSON.Number . fromIntegral


real :: RealFloat a => a -> JSON.Value
real x
  | isNaN x = JSON.String "NaN"
  | isInfinite x = JSON.String (if x < 0 then "-Infinity" else "Infinity")
  | otherwise = JSON.toJSON (realToFrac x :: Double)


decimal :: Int -> Integer -> JSON.Value
decimal scale n = JSON.Number (scientific n (negate scale))


dateText :: Integer -> T.Text
dateText days = T.pack (formatTime defaultTimeLocale "%0Y-%m-%d" (addDays days (fromGregorian 1970 1 1)))


timestampText :: A.TimeUnit -> Maybe T.Text -> Integer -> T.Text
timestampText unit tz value =
  T.pack (formatTime defaultTimeLocale fmt (posixSecondsToUTCTime (fromRational (toRational value / divisor))))
  where
    fmt = case tz of
      Just _ -> "%0Y-%m-%dT%T%QZ"
      Nothing -> "%0Y-%m-%d %T%Q"
    divisor = case unit of
      A.Second -> 1
      A.Millisecond -> 1000
      A.Microsecond -> 1000000
      A.Nanosecond -> 1000000000


variantJSON :: Variant.Variant -> Either String JSON.Value
variantJSON value = do
  encoded <- Variant.encodeVariant value
  -- Spark's DeltaStatsJsonUtils reads the metadata header first. The
  -- protocol example uses the opposite order.
  pure (JSON.String (encodeZ85 (Variant.variantMetadata encoded <> Variant.variantValue encoded)))
