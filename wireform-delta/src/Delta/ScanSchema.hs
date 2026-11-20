{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Resolve Delta's logical schema against the physical schema of each file.
module Delta.ScanSchema (
  ScanSchema,
  scanSchema,
  outputSchema,
  selectFields,
  partitionValueReader,
  changeFileProjection,
  validateChangeSchema,
  validateHiddenColumns,
  legacyVariantPaths,
  fileProjection,
  logicalFileStats,
  logicalFileStatsForPruning,
) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Monad.ST (runST)
import Data.Aeson (Value (..), withArray, withObject, (.:), (.:?))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.Bits (countTrailingZeros, finiteBitSize, shiftL, shiftR)
import Data.ByteString qualified as BS
import Data.Char (digitToInt, isHexDigit)
import Data.HashMap.Strict qualified as HM
import Data.Int (Int8, Int16, Int32)
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, mapMaybe)
import Data.Scientific (Scientific, base10Exponent, coefficient, scientific, toBoundedInteger, toRealFloat)
import Data.Semigroup (stimes)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Encoding qualified as TE
import Data.Text.Internal qualified as TI
import Data.Text.Read qualified as TR
import Data.Time (Day, LocalTime, defaultTimeLocale, diffDays, formatTime, fromGregorian, fromGregorianValid, localTimeToUTC, parseTimeM, utc)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word64)
import Delta.DeletionVector (descriptorCardinality)
import Delta.Log
import Delta.ParquetSchema (rebaseSparkDate, sparkGregorianCutover)
import Delta.SparkCalendar (SparkCalendar, TimestampRebase, legacyDates, legacyTimestamp, needsTimestampRebase, rebaseTimestamp)
import Foreign.Storable (Storable)
import GHC.Exts (Int64#, timesWord2#, word64ToWord#, wordToWord64#)
import GHC.Float (float2Double)
import GHC.Int (Int64 (I64#))
import GHC.Word (Word64 (W64#))
import Parquet.Arrow (isKeyOnlyMapField)
import Parquet.Timestamp qualified as Timestamp
import Parquet.Variant qualified as Variant
import Parquet.Variant.Arrow qualified as VA
import Parquet.Variant.Schema qualified as VariantSchema
import Text.Read (readMaybe)


data Mode = DisplayNames | PhysicalNames | FieldIds
  deriving stock (Eq)


data SchemaFeatures = SchemaFeatures
  { typeWidening :: !Bool
  , timestampNtz :: !Bool
  , variantType :: !Bool
  , variantShredding :: !Bool
  }


data ScanSchema = ScanSchema !Mode !(V.Vector NamedField) ~(HM.HashMap Text NamedField) ~Bool


data NamedField = NamedField
  { logical :: !A.Field
  , physicalName :: !Text
  , fieldId :: !(Maybe Int32)
  , fieldShape :: !Shape
  , fieldWidening :: !BoundWidening
  , partition :: !Partition
  }


data Partition = DataField | LogPartition | WidenedDatePartition | IntervalPartition !SparkInterval
  deriving stock (Eq)


data Shape
  = Scalar ![A.ArrowType]
  | Struct !(V.Vector NamedField) ~(HM.HashMap Text NamedField)
  | List !Shape
  | Map !Shape !Shape
  | Variant !Bool


scanSchema :: Maybe ProtocolAction -> MetaDataAction -> Either String ScanSchema
scanSchema protocol metadata = do
  case mdFormat metadata of
    Just (provider, _) | provider /= "parquet" -> Left "Delta.Scan: table data is not Parquet"
    _ -> pure ()
  mode <-
    if maybe False supportsMapping protocol
      then case M.lookup "delta.columnMapping.mode" (mdConfiguration metadata) of
        Nothing -> Right DisplayNames
        Just "none" -> Right DisplayNames
        Just "name" -> Right PhysicalNames
        Just "id" -> Right FieldIds
        Just other -> Left ("Delta.Scan: unsupported column mapping mode " ++ T.unpack other)
      else pure DisplayNames
  let partitions = S.fromList (mdPartitionColumns metadata)
      readerFeatures = maybe [] (map canonicalProtocolFeature . pReaderFeatures) protocol
      writerFeatures = maybe [] (map canonicalProtocolFeature . pWriterFeatures) protocol
      hasFeatureLists = maybe False (\p -> pMinReaderVersion p == 3 && pMinWriterVersion p == 7) protocol
      supports feature = hasFeatureLists && feature `elem` readerFeatures && feature `elem` writerFeatures
      stableVariant = supports "variantType"
      stableShredding = supports "variantShredding"
      previewShredding = supports "variantShredding-preview"
      features = SchemaFeatures
        { typeWidening = any (`elem` ["typeWidening", "typeWidening-preview"]) readerFeatures
        , timestampNtz = supports "timestampNtz"
        -- The preview shredding protocol did not require a separate
        -- Variant feature declaration.
        , variantType = stableVariant || supports "variantType-preview" || previewShredding
        , variantShredding = stableShredding || previewShredding
        }
  DeltaSchema fields <- parseDeltaSchema (mdSchemaString metadata)
  compiled <- compileFields mode features partitions fields
  unless (S.isSubsetOf partitions (S.fromList (map dfName fields))) $
    Left "Delta.Scan: partition column is missing from the table schema"
  let ids = V.foldr fieldIds [] compiled
  unless (mode == DisplayNames || IS.size (IS.fromList ids) == length ids) $
    Left "Delta.Scan: duplicate Delta column mapping IDs"
  pure (ScanSchema mode compiled (indexStatsFields compiled) (V.any (containsVariant . fieldShape) compiled))
  where
    fieldIds f rest = maybe id ((:) . fromIntegral) (fieldId f) (shapeIds (fieldShape f) rest)
    shapeIds (Struct fields _) rest = V.foldr fieldIds rest fields
    shapeIds (List shape) rest = shapeIds shape rest
    shapeIds (Map key value) rest = shapeIds key (shapeIds value rest)
    shapeIds _ rest = rest


-- Older Spark versions could list columnMapping only among the
-- writer features of a (3, 7) table. Honor its physical mapping.
supportsMapping :: ProtocolAction -> Bool
supportsMapping p =
  pMinReaderVersion p == 2
    || (pMinReaderVersion p == 3 && "columnMapping" `elem` pWriterFeatures p)


data BoundWidening = NoBoundWidening | DateBoundWidening | FloatBoundWidening
  deriving stock (Eq)


-- The path is relative to the nearest named field. Each collection
-- consumes one component, leaving scalar source types at their leaf.
data HistoricalType = HistoricalType !A.ArrowType ![Text]


validateTypeChanges :: SchemaFeatures -> DeltaField -> Either String [HistoricalType]
validateTypeChanges features field =
  case HM.lookup "delta.typeChanges" (dfMetadata field) of
    Nothing -> pure []
    Just history -> do
      changes <- parseEither (withArray "delta.typeChanges" (traverse parseChange . V.toList)) history
      traverse validate changes
  where
    parseChange = withObject "type change" $ \o ->
      (,,) <$> (o .: "fromType" >>= parseDeltaType) <*> (o .: "toType" >>= parseDeltaType)
        <*> (maybe [] (T.splitOn ".") <$> o .:? "fieldPath")
    validate (from, to, path) = do
      current <- walk (dfType field) path
      (source, _) <- compileType DisplayNames historyFeatures "" False from []
      (target, shape) <- compileType DisplayNames historyFeatures "" False to []
      unless (from == DTVoid || from == to) $
        () <$ widen (A.fieldType source) (A.fieldType target)
      -- Earlier destinations can themselves have been widened. A void
      -- replacement can introduce a collection or struct whose children
      -- may subsequently evolve through ordinary schema changes.
      case shape of
        Scalar _ | to /= current && to /= DTVoid -> do
          (actual, _) <- compileType DisplayNames historyFeatures "" False current []
          case widen (A.fieldType target) (A.fieldType actual) of
            Right _ -> Right ()
            Left _ -> Left ("Delta.Scan: type change does not match current type of " ++ T.unpack location)
        _ -> Right ()
      pure (HistoricalType (A.fieldType source) path)
      where
        location = T.intercalate "." (dfName field : path)
    historyFeatures = features {typeWidening = False}
    walk ty [] = Right ty
    walk (DTArray ty _) ("element" : rest) = walk ty rest
    walk (DTMap ty _ _) ("key" : rest) = walk ty rest
    walk (DTMap _ ty _) ("value" : rest) = walk ty rest
    walk _ _ = Left "Delta.Scan: invalid delta.typeChanges fieldPath"


compileFields :: Mode -> SchemaFeatures -> S.Set Text -> [DeltaField] -> Either String (V.Vector NamedField)
compileFields mode features partitions definitions = do
  fields <- V.mapM compile (V.fromList definitions)
  when (mode /= DisplayNames) $ do
    let physical = V.foldl' (\seen f -> S.insert (physicalName f) seen) S.empty fields
    unless (S.size physical == V.length fields) $
      Left "Delta.Scan: duplicate physical sibling names"
  pure fields
  where
    compile df = do
      let name = dfName df
          isPartition = S.member name partitions
          -- Older files may be missing a named field. The public schema
          -- permits nulls for these fields, including required table fields.
          nullable = True
      changes <- if typeWidening features then validateTypeChanges features df else pure []
      (arrow, shape) <- compileType mode features name nullable (dfType df) changes
      (physical, ident) <- case mode of
        DisplayNames -> pure (name, Nothing)
        _ -> case (HM.lookup "delta.columnMapping.physicalName" (dfMetadata df), HM.lookup "delta.columnMapping.id" (dfMetadata df)) of
          (Just (String physical), Just (Number raw))
            | not (T.null physical), Just ident <- toBoundedInteger raw, ident > 0 -> pure (physical, Just ident)
          _ -> Left ("Delta.Scan: missing or invalid column mapping for " ++ T.unpack name)
      when (isPartition && case shape of Scalar _ -> False; _ -> True) $
        Left ("Delta.Scan: partition column is not scalar: " ++ T.unpack name)
      let boundWidening = case (A.fieldType arrow, shape) of
            (A.ATimestamp A.Microsecond Nothing, Scalar historical) | A.ADate A.DateDay `elem` historical -> DateBoundWidening
            (A.AFloatingPoint A.DoublePrecision, Scalar historical) | A.AFloatingPoint A.Single `elem` historical -> FloatBoundWidening
            _ -> NoBoundWidening
          source
            | not isPartition = DataField
            | DTSparkInterval interval <- dfType df = IntervalPartition interval
            | boundWidening == DateBoundWidening = WidenedDatePartition
            | otherwise = LogPartition
      pure (NamedField arrow physical ident shape boundWidening source)


compileType :: Mode -> SchemaFeatures -> Text -> Bool -> DeltaType -> [HistoricalType] -> Either String (A.Field, Shape)
compileType mode features name nullable ty changes = case ty of
  DTString -> scalar A.AUtf8
  DTLong -> scalar (A.AInt 64 True)
  DTInteger -> scalar (A.AInt 32 True)
  DTShort -> scalar (A.AInt 16 True)
  DTByte -> scalar (A.AInt 8 True)
  DTFloat -> scalar (A.AFloatingPoint A.Single)
  DTDouble -> scalar (A.AFloatingPoint A.DoublePrecision)
  DTBoolean -> scalar A.ABool
  DTBinary -> scalar A.ABinary
  DTDate -> scalar (A.ADate A.DateDay)
  DTTimestamp -> scalar (A.ATimestamp A.Microsecond (Just "UTC"))
  DTTimestampNtz
    | timestampNtz features -> scalar (A.ATimestamp A.Microsecond Nothing)
    | otherwise -> Left "Delta.Scan: timestamp_ntz requires the timestampNtz reader feature and writer feature (protocol 3/7)"
  DTSparkInterval (SparkInterval start _)
    | start <= Month -> scalar (A.AInterval A.YearMonth)
    | otherwise -> scalar (A.ADuration A.Microsecond)
  DTVoid -> scalar A.ANull
  DTDecimal p s -> scalar (A.ADecimal p s)
  DTVariant
    | variantType features -> (,Variant (variantShredding features)) <$> VariantSchema.variantField name nullable Nothing
    | otherwise -> Left "Delta.Scan: variant requires the variantType reader and writer feature"
  DTStruct fields -> do
    children <- compileFields mode features S.empty fields
    pure (field A.AStruct (logical <$> children), Struct children (indexStatsFields children))
  DTArray ty' containsNull -> do
    (element, shape) <- compileType mode features "element" containsNull ty' (childChanges "element")
    pure (field A.AList (V.singleton element), List shape)
  DTMap keyTy valueTy valueContainsNull -> do
    (key, keyShape) <- compileType mode features "key" False keyTy (childChanges "key")
    (value, valueShape) <- compileType mode features "value" valueContainsNull valueTy (childChanges "value")
    let entries = A.defaultField "entries" False A.AStruct (V.fromList [key, value])
    pure (field (A.AMap False) (V.singleton entries), Map keyShape valueShape)
  DTUnknown ty' -> Left ("Delta.Scan: unsupported Delta data type " ++ T.unpack ty')
  where
    field = A.defaultField name nullable
    scalar t = pure (A.defaultLeafField name nullable t, Scalar (mapMaybe exact changes))
    exact (HistoricalType historical []) = Just historical
    exact _ = Nothing
    childChanges child = mapMaybe descend changes
      where
        descend (HistoricalType historical (step : rest)) | step == child =
          Just (HistoricalType historical rest)
        descend _ = Nothing


outputSchema :: ScanSchema -> A.Schema
outputSchema (ScanSchema _ fields _ _) = A.defaultSchema (logical <$> fields)


-- | Project by display name before resolving any physical columns.
selectFields :: [Text] -> ScanSchema -> Either String ScanSchema
selectFields names (ScanSchema mode fields _ _) = do
  unless (S.size (S.fromList names) == length names) $
    Left "Delta.Scan: projection contains duplicate column names"
  selected <- V.mapM select (V.fromList names)
  pure (ScanSchema mode selected (indexStatsFields selected) (V.any (containsVariant . fieldShape) selected))
  where
    byName = V.foldl' (\index field -> HM.insert (A.fieldName (logical field)) field index) HM.empty fields
    select name = case HM.lookup name byName of
      Just field -> Right field
      Nothing -> Left ("Delta.Scan: projected column " ++ show name ++ " is missing from the snapshot schema")


containsVariant :: Shape -> Bool
containsVariant (Variant _) = True
containsVariant (Struct fields _) = V.any (containsVariant . fieldShape) fields
containsVariant (List child) = containsVariant child
containsVariant (Map key value) = containsVariant key || containsVariant value
containsVariant (Scalar _) = False


-- Spark may omit the Parquet annotation from a shredded Variant. Find its
-- physical path using the same name, ID and collection rules as projection.
legacyVariantPaths :: ScanSchema -> A.Schema -> [[Text]]
legacyVariantPaths (ScanSchema mode fields _ hasVariants) native
  | hasVariants = named [] fields (A.arrowFields native) []
  | otherwise = []
  where
    named path definitions siblings rest = V.foldr collect rest definitions
      where
        collect field tailPaths
          | let shape = fieldShape field
          , containsVariant shape
          , Just source <- V.find (matches field) siblings = visit path shape source tailPaths
          | otherwise = tailPaths
    matches field source =
      if mode == FieldIds
        then maybe False (\ident -> parquetId source == Just ident) (fieldId field)
        else physicalName field == A.fieldName source
    visit parent shape source rest =
      let path = A.fieldName source : parent
          children = A.fieldChildren source
      in case shape of
           Variant _
             | A.fieldType source == A.AStruct
             , not (VariantSchema.isVariantField source)
             , V.any ((== "typed_value") . A.fieldName) children -> reverse path : rest
           Struct fields' _
             | A.fieldType source == A.AStruct
             , not (VariantSchema.isVariantField source) -> named path fields' children rest
           List child
             | A.fieldType source == A.AList
             , Just element <- children V.!? 0 -> visit path child element rest
           Map key value
             | A.fieldType source == A.AMap False
             , Just entries <- children V.!? 0
             , [k, v] <- V.toList (A.fieldChildren entries) ->
                 let entryPath = A.fieldName entries : path
                 in visit entryPath key k (visit entryPath value v rest)
             | isKeyOnlyMapField source
             , Just k <- children V.!? 0 -> visit path key k rest
           _ -> rest


-- Row tracking column names are reserved even when a user field is missing
-- from this file or its value is synthesized from a partition.
validateHiddenColumns :: ScanSchema -> Text -> Text -> Either String ()
validateHiddenColumns (ScanSchema _ fields _ _) idName versionName =
  unless (V.all available fields) (Left "Delta.Scan: hidden row tracking column is also a data column")
  where
    reserved name = name == idName || name == versionName
    available field = not (reserved (A.fieldName (logical field)) || reserved (physicalName field))


-- A planned field may come from a projected Parquet column or from the log
-- (a partition constant or a null for a field absent from this file).
data Source
  = Synthesized !(Int -> Either String C.ColumnArray)
  | Projected !A.Field !(C.ColumnArray -> Either String C.ColumnArray)


fileProjection
  :: (LocalTime -> Maybe Int)
  -> ScanSchema
  -> AddAction
  -> SparkCalendar
  -> A.Schema
  -> Either String (A.Schema, Int -> V.Vector C.ColumnArray -> Either String (V.Vector C.ColumnArray))
fileProjection offset (ScanSchema mode fields _ _) file calendar native = do
  let fileFields = A.arrowFields native
  -- With no physical fields, there is nothing to resolve by ID. The
  -- file can still contribute rows, partition constants and nulls.
  when (mode == FieldIds && not (V.null fileFields) && not (V.any hasFieldId fileFields) && V.any ((== DataField) . partition) fields) $
    Left "Delta.Scan: Parquet file has no field IDs for Delta id mapping"
  siblings <- indexSiblings mode fileFields
  sources <- V.mapM (topField siblings) fields
  let (projected, restore) = assembleSources sources
  pure (native {A.arrowFields = projected}, restore)
  where
    topField siblings f = case partition f of
      DataField -> resolveNamed mode calendar siblings f
      _ -> Synthesized <$> readPartitionColumn offset file f


-- The CDF marker is an unmapped physical field even on tables using IDs.
changeFileProjection
  :: (LocalTime -> Maybe Int)
  -> ScanSchema
  -> AddAction
  -> SparkCalendar
  -> A.Schema
  -> Either String (A.Schema, Int -> V.Vector C.ColumnArray -> Either String (V.Vector C.ColumnArray))
changeFileProjection offset schema file calendar native = do
  let name = "_change_type"
      (markers, dataFields) = V.partition ((== name) . A.fieldName) (A.arrowFields native)
  marker <- case V.toList markers of
    [field] | A.fieldType field == A.AUtf8 -> Right field
    _ -> Left "Delta.Scan: CDC Parquet requires a string _change_type column"
  (projected, restore) <- fileProjection offset schema file calendar (native {A.arrowFields = dataFields})
  pure (projected {A.arrowFields = V.snoc (A.arrowFields projected) marker}, \n columns ->
    V.snoc <$> restore n (V.init columns) <*> pure (V.last columns))


-- A range spanning the move from unmapped to mapped storage would need
-- historical, per-file identities. Refuse that ambiguity rather than
-- masking the old values as columns that never existed.
validateChangeMapping :: Mode -> TableSnapshot -> MetaDataAction -> Either String ()
validateChangeMapping expected snapshot metadata =
  let mapping = M.lookup "delta.columnMapping.mode" (mdConfiguration metadata)
      actual = case (maybe False supportsMapping (tsProtocol snapshot), mapping) of
        (True, Just "name") -> PhysicalNames
        (True, Just "id") -> FieldIds
        _ -> DisplayNames
  in unless (actual == expected) (Left "Delta.Scan: change data feed range crosses a column mapping mode change")


-- A mapped feed can cross additions, but existing columns must keep
-- their logical and physical identity, type, and nullability. Compare
-- adjacent metadata so a change that is later reverted still matters.
validateChangeSchema :: ScanSchema -> TableSnapshot -> Maybe TableSnapshot -> Either String ()
validateChangeSchema (ScanSchema mode _ _ _) after before =
  case tsMetaData after of
    Nothing -> Left "Delta.Scan: change data feed requires table metadata"
    Just current -> do
      validateChangeMapping mode after current
      case before >>= (\snapshot -> (snapshot,) <$> tsMetaData snapshot) of
        Nothing -> Right ()
        Just (snapshot, prior) -> do
          validateChangeMapping mode snapshot prior
          unless (mdPartitionColumns prior == mdPartitionColumns current) $
            Left "Delta.Scan: change data feed range crosses a partition schema change"
          when (mdSchemaString prior /= mdSchemaString current
                && (mode /= DisplayNames || not (null (mdPartitionColumns current)))) $ do
            DeltaSchema old <- parseDeltaSchema (mdSchemaString prior)
            DeltaSchema new <- parseDeltaSchema (mdSchemaString current)
            let names = S.fromList (mdPartitionColumns current)
                partitions = M.fromList . map (\field -> (dfName field, (dfType field, dfNullable field)))
                  . filter (\field -> S.member (dfName field) names)
            if mode == DisplayNames
              then unless (partitions old == partitions new) $
                Left "Delta.Scan: change data feed range crosses a partition schema change"
              else compareFields [] old new
  where
    compareFields parent old new = mapM_ compareField old
      where
        names = HM.fromList (map (\field -> (dfName field, field)) new)
        compareField previous = do
          let name = dfName previous
              path = name : parent
          current <- maybe (incompatible path) Right (HM.lookup name names)
          unless (dfNullable previous == dfNullable current && sameMapping previous current) $
            incompatible path
          compareTypes path (dfType previous) (dfType current)

    sameMapping old new = all (\key -> HM.lookup key (dfMetadata old) == HM.lookup key (dfMetadata new))
      ["delta.columnMapping.id", "delta.columnMapping.physicalName", "delta.columnMapping.nested.ids"]

    compareTypes path (DTStruct old) (DTStruct new) = compareFields path old new
    compareTypes path (DTArray old oldNulls) (DTArray new newNulls)
      | oldNulls == newNulls = compareTypes ("element" : path) old new
    compareTypes path (DTMap oldKey oldValue oldNulls) (DTMap newKey newValue newNulls)
      | oldNulls == newNulls = do
          compareTypes ("key" : path) oldKey newKey
          compareTypes ("value" : path) oldValue newValue
    compareTypes path old new = unless (old == new) (incompatible path)

    incompatible path = Left ("Delta.Scan: change data feed range crosses a non-additive schema change at " ++ show (reverse path))


-- Resolve a partition by its display name once, and use the same parser
-- as the row scan for the singleton value used during file pruning.
partitionValueReader
  :: ScanSchema
  -> Text
  -> Maybe (A.Field, (LocalTime -> Maybe Int) -> AddAction -> Either String C.ColumnArray)
partitionValueReader (ScanSchema _ fields _ _) name = do
  field <- V.find (\f -> partition f /= DataField && A.fieldName (logical f) == name) fields
  pure (logical field, \offset file -> readPartitionColumn offset file field >>= ($ 1))


readPartitionColumn
  :: (LocalTime -> Maybe Int)
  -> AddAction
  -> NamedField
  -> Either String (Int -> Either String C.ColumnArray)
readPartitionColumn offset file field = do
  let name = physicalName field
  value <- maybe (Left ("Delta.Scan: missing partition value for " ++ T.unpack name)) Right (M.lookup name (addPartitionValues file))
  partitionColumn offset (partition field) (logical field) value


data Siblings = Siblings !(M.Map Text A.Field) !(M.Map Int32 A.Field)


indexSiblings :: Mode -> V.Vector A.Field -> Either String Siblings
indexSiblings mode = V.foldM' add (Siblings M.empty M.empty)
  where
    add (Siblings names ids) field =
      let names' = M.insert (A.fieldName field) field names
      in case if mode == FieldIds then parquetId field else Nothing of
           Nothing -> pure (Siblings names' ids)
           Just ident
             | M.member ident ids -> Left "Delta.Scan: duplicate Parquet field IDs among siblings"
             | otherwise -> pure (Siblings names' (M.insert ident field ids))


parquetId :: A.Field -> Maybe Int32
parquetId field = do
  (_, ident) <- V.find ((== "PARQUET:field_id") . fst) (A.fieldMetadata field)
  readMaybe (T.unpack ident)


hasFieldId :: A.Field -> Bool
hasFieldId field = maybe False (const True) (parquetId field) || V.any hasFieldId (A.fieldChildren field)


resolveNamed :: Mode -> SparkCalendar -> Siblings -> NamedField -> Either String Source
resolveNamed mode calendar (Siblings names ids) f =
  case if mode == FieldIds then fieldId f >>= (`M.lookup` ids) else M.lookup (physicalName f) names of
    Nothing -> Synthesized <$> nullColumn (logical f)
    Just source -> do
      (projected, convert) <- project mode calendar (logical f) (fieldShape f) source
      pure (Projected projected convert)


assembleSources
  :: V.Vector Source
  -> (V.Vector A.Field, Int -> V.Vector C.ColumnArray -> Either String (V.Vector C.ColumnArray))
assembleSources sources =
  let projected = V.mapMaybe (\case Projected field _ -> Just field; Synthesized _ -> Nothing) sources
      indexed = V.prescanl' (\n -> \case Projected {} -> n + 1; Synthesized _ -> n) 0 sources
      restore n values =
        V.zipWithM
          ( \i -> \case
              Synthesized make -> make n
              Projected _ convert ->
                maybe (Left "Delta.Scan: missing projected column") convert (values V.!? i)
          )
          indexed
          sources
  in (projected, restore)


project
  :: Mode
  -> SparkCalendar
  -> A.Field
  -> Shape
  -> A.Field
  -> Either String (A.Field, C.ColumnArray -> Either String C.ColumnArray)
project mode calendar target shape source = case shape of
  Struct fields _ | A.fieldType source == A.AStruct && not (VariantSchema.isVariantField source) -> do
    siblings <- indexSiblings mode (A.fieldChildren source)
    sources <- V.mapM (resolveNamed mode calendar siblings) fields
    let (children, restore) = assembleSources sources
        convert (C.ColStruct n valid columns) =
          restore n (snd <$> columns) >>= C.mkStruct n valid . V.zip (A.fieldName . logical <$> fields)
        convert _ = incompatible
    pure (source {A.fieldChildren = children}, convert)
  List element
    | A.fieldType source == A.AList
    , [targetChild] <- V.toList (A.fieldChildren target)
    , [sourceChild] <- V.toList (A.fieldChildren source) -> do
        (child, convertChild) <- project mode calendar targetChild element sourceChild
        let convert (C.ColList valid offsets values) = convertChild values >>= C.mkList valid offsets
            convert _ = incompatible
        pure (source {A.fieldChildren = V.singleton child}, convert)
  Map key value
    | A.fieldType source == A.AMap False
    , [targetEntry] <- V.toList (A.fieldChildren target)
    , [sourceEntry] <- V.toList (A.fieldChildren source)
    , [targetKey, targetValue] <- V.toList (A.fieldChildren targetEntry)
    , [sourceKey, sourceValue] <- V.toList (A.fieldChildren sourceEntry) -> do
        (keyField, convertKey) <- project mode calendar targetKey key sourceKey
        (valueField, convertValue) <- project mode calendar targetValue value sourceValue
        let convert (C.ColMap valid offsets keys values) =
              C.mkMap valid offsets <$> convertKey keys <*> convertValue values >>= id
            convert _ = incompatible
            entries = sourceEntry {A.fieldChildren = V.fromList [keyField, valueField]}
        pure (source {A.fieldChildren = V.singleton entries}, convert)
  Map key _
    | isKeyOnlyMapField source
    , [sourceKey] <- V.toList (A.fieldChildren source)
    , [targetEntry] <- V.toList (A.fieldChildren target)
    , [targetKey, targetValue] <- V.toList (A.fieldChildren targetEntry) -> do
        (keyField, convertKey) <- project mode calendar targetKey key sourceKey
        makeValues <- nullColumn targetValue
        let convert (C.ColList valid offsets keys) = do
              let n = C.columnLength keys
              when (n /= 0 && not (A.fieldNullable targetValue)) $
                Left "Delta.Scan: map has no physical values for a non-nullable value type"
              restoredKeys <- convertKey keys
              values <- makeValues n
              C.mkMap valid offsets restoredKeys values
            convert _ = incompatible
        pure (source {A.fieldChildren = V.singleton keyField}, convert)
  Variant shredding | VariantSchema.isVariantField source ->
    if V.any ((== "typed_value") . A.fieldName) (A.fieldChildren source)
      then if shredding
        then pure (source, toUnshredded source)
        else Left ("Delta.Scan: shredded Parquet Variant requires variantShredding for " ++ T.unpack (A.fieldName target))
      else pure (source, reorderVariant)
  Variant _
    | A.fieldType source == A.AStruct
    , V.length (A.fieldChildren source) == 2
    , V.all (\f -> A.fieldType f == A.ABinary && A.fieldName f `elem` ["metadata", "value"]) (A.fieldChildren source) ->
        pure (source, reorderVariant)
  Scalar _ | A.fieldType target == A.ANull -> pure (source, Right . C.ColNull . C.columnLength)
  Scalar _
    | A.AInterval A.YearMonth <- A.fieldType target
    , A.AInt 32 True <- A.fieldType source ->
        let convert (C.ColInt32 valid values) = C.mkPrim C.PIntervalYearMonth valid values
            convert _ = incompatible
        in pure (source, convert)
  Scalar _
    | A.ADuration A.Microsecond <- A.fieldType target
    , A.AInt 64 True <- A.fieldType source ->
        let convert (C.ColInt64 valid values) = C.mkPrim C.PDuration valid values
            convert _ = incompatible
        in pure (source, convert)
  Scalar _
    | A.ATimestamp A.Microsecond zone <- A.fieldType target
    , A.ATimestamp unit sourceZone <- A.fieldType source
    , (zone == Nothing) == (sourceZone == Nothing) ->
        pure (source, convertTimestamp (timestampCalendar False zone) unit)
  Scalar _
    | A.ATimestamp A.Microsecond zone <- A.fieldType target
    , A.AFixedSizeBinary 12 <- A.fieldType source -> do
        encoding <- Timestamp.timestampEncoding source
        case encoding of
          -- INT96 has no UTC flag. The Delta schema supplies the semantics.
          Just Timestamp.JulianInt96 ->
            pure (timestampProjection Nothing, convertTimestamp (timestampCalendar True zone) A.Microsecond)
          Just (Timestamp.SignedTimestamp96 adjusted _)
            | adjusted == maybe False (const True) zone ->
                pure (source, Timestamp.projectTimestampColumn source (timestampProjection zone))
            | otherwise -> Left "Delta.Scan: incompatible timestamp UTC/local semantics"
          Nothing -> incompatible
  Scalar _ | A.fieldType target == A.fieldType source -> pure (source, rebaseDateColumn)
  -- Some writers materialize void as Parquet UNKNOWN. Keep the physical
  -- projection so nested validity and collection offsets survive.
  _ | A.ANull <- A.fieldType source -> do
    make <- nullColumn target
    let convert (C.ColNull n) = make n
        convert _ = incompatible
    pure (source, convert)
  Scalar historical
    | A.fieldType source `elem` historical -> do
        convert <- widen (A.fieldType source) (A.fieldType target)
        pure (source, if rebasesDates then \column -> rebaseDateColumn column >>= convert else convert)
  _ -> incompatible
  where
    rebasesDates = legacyDates calendar && A.fieldType source == A.ADate A.DateDay
    rebaseDateColumn
      | rebasesDates = \case
          C.ColDate32 valid days
            | VS.any (< sparkGregorianCutover) days -> C.mkPrim C.PDate32 valid (VS.map rebaseSparkDate days)
          column -> Right column
      | otherwise = Right
    timestampCalendar int96 zone = zone >> legacyTimestamp int96 calendar
    timestampProjection zone =
      source {A.fieldType = A.ATimestamp A.Microsecond zone, A.fieldMetadata = V.empty}
    incompatible :: Either String a
    incompatible = Left ("Delta.Scan: incompatible physical type for " ++ T.unpack (A.fieldName target))


-- Translate statistics by physical names in both mapped modes. Parquet
-- field IDs are never used to interpret log statistics. Partition values
-- come from AddFile.partitionValues, so the statistics index omits them.
logicalFileStats :: ScanSchema -> AddAction -> Either String (Maybe AddStats)
logicalFileStats = logicalFileStatsWithTemporalBounds False


-- File predicates compare temporal values in Arrow's integer units. Use
-- those units directly instead of rendering and reparsing date/time text.
logicalFileStatsForPruning :: ScanSchema -> AddAction -> Either String (Maybe AddStats)
logicalFileStatsForPruning = logicalFileStatsWithTemporalBounds True


logicalFileStatsWithTemporalBounds :: Bool -> ScanSchema -> AddAction -> Either String (Maybe AddStats)
logicalFileStatsWithTemporalBounds numericTime (ScanSchema mode _ fields _) file = case addDeletionVector file of
  Nothing -> traverse (fmap (\stats -> remap (asNumRecords stats) stats)) (decodeAddStats file)
  Just descriptor -> do
    deleted <- descriptorCardinality descriptor
    stats <- maybe missingCount id (decodeAddStats file)
    physical <- maybe missingCount Right (asNumRecords stats)
    when (deleted > physical) $
      Left "Delta.Scan: deletion vector cardinality exceeds numRecords"
    let rows = if effectiveTightBounds stats then physical - deleted else physical
    pure (Just (remap (Just rows) stats))
  where
    missingCount :: Either String a
    missingCount = Left "Delta.Scan: a file with a deletion vector requires numRecords"
    remap rowLimit stats =
      stats
        { asMinValues = bounds False fields allNull (asMinValues stats)
        , asMaxValues = bounds True fields allNull (asMaxValues stats)
        , asNullCount = HM.mapMaybe scalarCount counts
        , asNullCounts = counts
        }
      where
        tight = effectiveTightBounds stats
        rawCounts = asNullCounts stats
        allNull = case rowLimit of
          Just rows | any (allNullCount rows) rawCounts -> Just (rows, rawCounts)
          _ -> Nothing
        counts = selectMaybe mode fields (nullCount mode tight rowLimit . fieldShape) rawCounts
    -- Bounds for all-null columns carry no information. Counts have
    -- to match live rows (tight) or file rows (wide) to establish this.
    bounds upper fs Nothing = selectMaybe mode fs (bound upper Nothing)
    bounds upper fs (Just (rows, counts)) = selectMaybe mode fs $ \field value ->
      case HM.lookup (physicalName field) counts of
        Just (NullCount n) | n == rows -> Nothing
        Just (NestedNullCounts children) -> bound upper (Just (rows, children)) field value
        _ -> bound upper Nothing field value
    bound upper allNull f (Object values) | Struct _ fs <- fieldShape f =
      let entries = bounds upper fs allNull (KM.toHashMapText values)
      in if HM.null entries then Nothing else Just (Object (KM.fromHashMapText entries))
    bound upper _ f value = scalarBound upper f value
    scalarBound upper f v@(String raw) = case A.fieldType (logical f) of
      A.AUtf8 -> Just v
      A.ADate A.DateDay
        | Just day <- parseDate raw
        , Just days <- checked @Int32 (daysSinceEpoch day) ->
            Just (if numericTime then Number (fromIntegral days) else v)
      A.ATimestamp A.Microsecond zone
        | fieldWidening f == DateBoundWidening
        , Just day <- parseDate raw
        , Just micros <- checked @Int64 (daysSinceEpoch day * 86400000000) ->
            if numericTime
              then Just (Number (fromIntegral micros))
              else Just (String (T.pack (formatTime defaultTimeLocale "%0Y-%m-%d 00:00:00" day)))
        | otherwise -> timestampStatBound numericTime upper zone raw
      _ | Variant True <- fieldShape f -> Just v
      _ -> Nothing
    scalarBound upper f v@(Number raw) = case A.fieldType (logical f) of
      A.AFloatingPoint precision ->
        Number <$> floatingBound upper precision (fieldWidening f == FloatBoundWidening) raw
      A.AInt 8 True | integerBoundFits @Int8 raw -> Just v
      A.AInt 16 True | integerBoundFits @Int16 raw -> Just v
      A.AInt 32 True | integerBoundFits @Int32 raw -> Just v
      A.AInt 64 True | integerBoundFits @Int64 raw -> Just v
      A.ADecimal precision scale | decimalBoundFits precision scale raw -> Just v
      _ -> Nothing
    scalarBound _ f v@(Bool _) | A.ABool <- A.fieldType (logical f) = Just v
    scalarBound _ _ _ = Nothing
    scalarCount (NullCount n) = Just n
    scalarCount (NestedNullCounts _) = Nothing


allNullCount :: Word64 -> NullCount -> Bool
allNullCount rows (NullCount n) = n == rows
allNullCount rows (NestedNullCounts counts) = any (allNullCount rows) counts


-- Decimal stats can be abbreviations for binary values. A widened column
-- can contain older Float32 files as well as files written as Double.
-- Enclose the stored decimal and both plausible binary interpretations.
floatingBound :: Bool -> A.Precision -> Bool -> Scientific -> Maybe Scientific
floatingBound upper precision widened raw
  | isInfinite restored = Nothing
  | upper = Just (max raw exact)
  | otherwise = Just (min raw exact)
  where
    single = float2Double (decimalToFloat singlePowers 16_777_216 raw)
    double = decimalToFloat doublePowers 9_007_199_254_740_992 raw
    restored = case precision of
      A.Single -> single
      _
        | widened && not (isInfinite single) -> if upper then max double single else min double single
        | otherwise -> double
    exact = floatAsScientific restored


-- A single IEEE operation rounds directly to the target precision when
-- the coefficient and the power of ten are both exactly representable.
decimalToFloat :: (RealFloat a, Storable a) => VS.Vector a -> Integer -> Scientific -> a
decimalToFloat exactPowers limit value
  | power >= negate bound, power <= bound, abs coeff <= limit =
      if power < 0
        then fromInteger coeff / VS.unsafeIndex exactPowers (negate power)
        else fromInteger coeff * VS.unsafeIndex exactPowers power
  | otherwise = toRealFloat value
  where
    bound = VS.length exactPowers - 1
    coeff = coefficient value
    power = base10Exponent value
{-# INLINE decimalToFloat #-}


singlePowers :: VS.Vector Float
singlePowers = VS.iterateN 11 (* 10) 1


doublePowers :: VS.Vector Double
doublePowers = VS.iterateN 23 (* 10) 1


floatAsScientific :: Double -> Scientific
floatAsScientific value
  | value == 0 = 0
  | power >= 0 = scientific (toInteger mantissa `shiftL` power) 0
  | otherwise = scientific (toInteger mantissa * (fivePowers V.! negate power)) power
  where
    -- Removing binary padding keeps the power within the IEEE Double
    -- range even when decodeFloat normalizes a subnormal significand.
    (raw, binaryPower) = decodeFloat value
    bits = fromInteger raw :: Int64
    padding = countTrailingZeros bits
    mantissa = bits `shiftR` padding
    power = binaryPower + padding


fivePowers :: V.Vector Integer
fivePowers = V.iterateN 1075 (* 5) 1


-- Most integer bounds already have exponent zero. Avoid normalizing their
-- coefficient or constructing and unwrapping a bounded integer.
integerBoundFits :: forall a. (Bounded a, Integral a) => Scientific -> Bool
integerBoundFits value
  | base10Exponent value == 0 =
      let n = coefficient value
      in toInteger (minBound @a) <= n && n <= toInteger (maxBound @a)
  | otherwise = isJust (toBoundedInteger @a value)
{-# INLINE integerBoundFits #-}


-- The bound must be representable at the column's declared precision and
-- scale. Keep the exponent out of a power of ten until it fits the precision.
decimalBoundFits :: Int -> Int -> Scientific -> Bool
decimalBoundFits precision scale value
  | n == 0 = True
  | powerOfTen >= precision - scale = False
  | otherwise = fits (abs n) (powerOfTen + scale)
  where
    n = coefficient value
    powerOfTen = base10Exponent value
    fits magnitude power
      | power >= 0 = magnitude < decimalPowers V.! (precision - power)
      | otherwise = case magnitude `quotRem` 10 of
          (rest, 0) -> fits rest (power + 1)
          _ -> False


-- Delta decimals have at most 38 digits.
decimalPowers :: V.Vector Integer
decimalPowers = V.iterateN 39 (* 10) 1


-- Spark also copies truncated JSON timestamps into Parquet statistics.
-- A millisecond-aligned maximum may have lost its submillisecond part.
timestampStatBound :: Bool -> Bool -> Maybe Text -> Text -> Maybe Value
timestampStatBound numericTime upperBound zone raw = do
  -- A zoned bound without an offset cannot safely prune a file. Keep
  -- explicit offsets and timezone-free timestamp_ntz bounds.
  micros <- parseTimestamp (const Nothing) True zoned raw
  _ <- checked @Int64 micros
  let padded = upperBound && micros `rem` 1000 == 0
      bound = if padded then min (toInteger (maxBound :: Int64)) (micros + 1000) else micros
      stamp = posixSecondsToUTCTime (fromInteger bound / 1000000)
      patternText = if zoned then "%0Y-%m-%dT%T%QZ" else "%0Y-%m-%d %T%Q"
  pure $ if numericTime
    then Number (fromInteger bound)
    else String (if padded then T.pack (formatTime defaultTimeLocale patternText stamp) else raw)
  where
    zoned = case zone of
      Just _ -> True
      Nothing -> False


-- Null counts mirror the schema. Wide counts describe only two useful
-- cases, all or none, relative to the physical file.
nullCount :: Mode -> Bool -> Maybe Word64 -> Shape -> NullCount -> Maybe NullCount
nullCount mode tight rows (Struct _ fields) (NestedNullCounts children)
  | HM.null kept = Nothing
  | otherwise = Just (NestedNullCounts kept)
  where
    kept = selectMaybe mode fields (nullCount mode tight rows . fieldShape) children
nullCount _ _ _ (Struct _ _) _ = Nothing
nullCount _ tight rows _ count@(NullCount n)
  | if tight then maybe True (n <=) rows else n == 0 || Just n == rows = Just count
  | otherwise = Nothing
nullCount _ _ _ _ (NestedNullCounts _) = Nothing


-- Compile the physical-name lookup once. Per-file statistics usually cover
-- only a small subset of the table's columns. Leave the index lazy so
-- ordinary row scans do not build it.
indexStatsFields :: V.Vector NamedField -> HM.HashMap Text NamedField
indexStatsFields = V.foldl' insertDataField HM.empty
  where
    insertDataField index field
      | partition field == DataField = HM.insert (physicalName field) field index
      | otherwise = index


selectMaybe :: Mode -> HM.HashMap Text NamedField -> (NamedField -> a -> Maybe a) -> HM.HashMap Text a -> HM.HashMap Text a
selectMaybe DisplayNames fields convert = HM.mapMaybeWithKey $ \name value ->
  HM.lookup name fields >>= \field -> convert field value
selectMaybe _ fields convert = HM.foldlWithKey' add HM.empty
  where
    add acc physical raw = case HM.lookup physical fields of
      Just field | Just value <- convert field raw ->
        HM.insert (A.fieldName (logical field)) value acc
      _ -> acc
{-# INLINE selectMaybe #-}


nullColumn :: A.Field -> Either String (Int -> Either String C.ColumnArray)
nullColumn field = do
  empty <- C.emptyColumnFor field
  pure $ \n ->
    if A.fieldType field == A.ANull
      then pure (C.ColNull n)
      else C.maskValidity (C.validityGenerate n (const False)) (C.fillerColumn n empty)


partitionColumn :: (LocalTime -> Maybe Int) -> Partition -> A.Field -> Maybe Text -> Either String (Int -> Either String C.ColumnArray)
partitionColumn _ _ field Nothing = nullColumn field
partitionColumn offset source field (Just raw) = case A.fieldType field of
  A.AUtf8 -> pure (repeatBytes C.mkUtf8 (TE.encodeUtf8 raw))
  A.ABinary -> repeatBytes C.mkBinary <$> require (binaryPartition raw)
  A.ABool -> case raw of
    "true" -> pure (\n -> C.mkBool Nothing (C.bitmapGenerate n (const True)))
    "false" -> pure (\n -> C.mkBool Nothing (C.bitmapGenerate n (const False)))
    _ -> invalid
  A.AInt 8 True -> number C.PInt8 (partitionInteger raw)
  A.AInt 16 True -> number C.PInt16 (partitionInteger raw)
  A.AInt 32 True -> number C.PInt32 (partitionInteger raw)
  A.AInt 64 True -> number C.PInt64 (partitionInteger raw)
  A.AFloatingPoint A.Single -> number C.PFloat (floatingPartition raw)
  A.AFloatingPoint A.DoublePrecision -> number C.PDouble (floatingPartition raw)
  A.ADecimal precision scale ->
    number (C.PDecimal128 precision scale) (C.decimal128FromInteger <$> unscaledDecimal precision scale raw)
  A.AInterval A.YearMonth | IntervalPartition interval <- source ->
    number C.PIntervalYearMonth (intervalPartition interval raw >>= checked)
  A.ADuration A.Microsecond | IntervalPartition interval <- source ->
    number C.PDuration (intervalPartition interval raw >>= checked)
  A.ADate A.DateDay -> do
    day <- require date
    number C.PDate32 (checked (daysSinceEpoch day))
  A.ATimestamp A.Microsecond timezone ->
    let legacyDate = if source == WidenedDatePartition && timezone == Nothing then ((* 86400000000) . daysSinceEpoch) <$> date else Nothing
        micros = parseTimestamp offset False (timezone /= Nothing) raw <|> legacyDate
    in number C.PTimestamp (micros >>= checked)
  _ -> invalid
  where
    date = parseDate (T.strip raw)
    invalid :: Either String a
    invalid = Left ("Delta.Scan: invalid partition value for " ++ T.unpack (A.fieldName field))
    require = maybe invalid Right
    number :: Storable a => C.PrimType a -> Maybe a -> Either String (Int -> Either String C.ColumnArray)
    number kind value = do
      x <- maybe invalid Right value
      pure (\n -> C.mkPrim kind Nothing (VS.replicate n x))
    repeatBytes make bytes n
      | width == 0 = make Nothing (VS.replicate (n + 1) 0) BS.empty
      | n > fromIntegral (maxBound :: Int32) `quot` width = Left "Delta.Scan: partition bytes exceed Arrow offsets"
      | n == 0 = make Nothing (VS.singleton 0) BS.empty
      | otherwise = make Nothing (VS.generate (n + 1) (fromIntegral . (* width))) (stimes n bytes)
      where
        width = BS.length bytes


-- Spark formats ANSI intervals as a signed magnitude with a qualifier.
-- A duration uses microseconds so even Long.MinValue is representable.
intervalPartition :: SparkInterval -> Text -> Maybe Integer
intervalPartition (SparkInterval start end) raw = do
  literal <- T.stripPrefix "INTERVAL '" (T.strip raw)
  let (amount, suffix) = T.breakOn "'" literal
      (negative, unsigned) = numberSign amount
      expected = if start == end then unit start else unit start <> " TO " <> unit end
  unless (suffix == "' " <> expected) Nothing
  magnitude <- readFields start unsigned
  pure (if negative then negate magnitude else magnitude)
  where
    readFields part input
      | part == Second = do
          let (whole, fraction) = T.breakOn "." input
          seconds <- integralField part whole
          if T.null fraction
            then pure (seconds * 1000000)
            else do
              let digits = T.drop 1 fraction
                  micros = T.take 6 digits
              unless (not (T.null digits) && T.all (== '0') (T.drop 6 digits)) Nothing
              subsecond <- unsignedNumber micros
              pure (seconds * 1000000 + subsecond * 10 ^ (6 - T.length micros))
      | part == end = (* multiplier part) <$> integralField part input
      | otherwise = do
          let delimiter = case part of
                Year -> "-"
                Day -> " "
                _ -> ":"
              (digits, tailText) = T.breakOn delimiter input
          field <- integralField part digits
          rest <- T.stripPrefix delimiter tailText >>= readFields (succ part)
          pure (field * multiplier part + rest)
    integralField part digits = do
      value <- unsignedNumber digits
      unless (part == start || value < fieldLimit part) Nothing
      pure value
    unsignedNumber digits =
      let significant = T.dropWhile (== '0') digits
      in if T.null digits || T.length significant > 19 || not (T.all decimalDigit significant)
           then Nothing
           else if T.null significant then Just 0 else readDigits significant
    fieldLimit Month = 12
    fieldLimit Hour = 24
    fieldLimit _ = 60
    multiplier Year = 12
    multiplier Month = 1
    multiplier Day = 86400000000
    multiplier Hour = 3600000000
    multiplier Minute = 60000000
    multiplier Second = 1000000
    unit Year = "YEAR"
    unit Month = "MONTH"
    unit Day = "DAY"
    unit Hour = "HOUR"
    unit Minute = "MINUTE"
    unit Second = "SECOND"


-- Some writers escape every byte as a literal \u00XX in the decoded
-- JSON string. Also accept already unescaped byte-valued strings.
binaryPartition :: Text -> Maybe BS.ByteString
binaryPartition raw
  | T.isPrefixOf "\\u00" raw
  , remainder == 0
  , (decoded, Just rest) <- BS.unfoldrN count escapedByte raw
  , T.null rest = Just decoded
  | T.all (<= '\xff') raw = Just (BS.pack (map (fromIntegral . fromEnum) (T.unpack raw)))
  | otherwise = Nothing
  where
    (count, remainder) = quotRem (T.length raw) 6
    escapedByte text = do
      (hi, tailDigits) <- T.stripPrefix "\\u00" text >>= T.uncons
      (lo, rest) <- T.uncons tailDigits
      unless (isHexDigit hi && isHexDigit lo) Nothing
      pure (fromIntegral (16 * digitToInt hi + digitToInt lo), rest)


parseDate :: Text -> Maybe Day
parseDate = parseTimeM False defaultTimeLocale "%F" . T.unpack . calendarYear


-- ISO extended positive years can carry a plus sign, which %F does not
-- accept. Leave any other leading character for the calendar parser.
calendarYear :: Text -> Text
calendarYear raw
  | Just digits <- T.stripPrefix "+" raw
  , Just (digit, _) <- T.uncons digits
  , decimalDigit digit = digits
  | otherwise = raw


daysSinceEpoch :: Day -> Integer
daysSinceEpoch day = diffDays day (fromGregorian 1970 1 1)


parseTimestamp :: (LocalTime -> Maybe Int) -> Bool -> Bool -> Text -> Maybe Integer
parseTimestamp localOffset allowIsoLocal zoned raw =
  (toInteger <$> standardTimestamp allowIsoLocal zoned raw)
    <|> parseExtendedTimestamp localOffset allowIsoLocal zoned raw


-- Four-digit ISO dates are common in both statistics and partition values.
-- Read their ASCII digits directly and keep arithmetic in Int64. The
-- calendar parser handles extended years and timestamps requiring a
-- caller-supplied local timezone.
standardTimestamp :: Bool -> Bool -> Text -> Maybe Int64
standardTimestamp allowIsoLocal zoned (TI.Text bytes start size)
  | size >= 19
  , byte 4 == 0x2d, byte 7 == 0x2d
  , separator == 0x20 || separator == 0x54
  , byte 13 == 0x3a, byte 16 == 0x3a
  , let century = pair 0
        year = pair 2
        month = pair 5
        day = pair 8
        hours = pair 11
        minutes = pair 14
        seconds = pair 17
  , century >= 0, year >= 0, month > 0, day > 0
  , hours >= 0, hours < 24
  , minutes >= 0, minutes < 60
  , seconds >= 0, seconds < 60
  , Just date <- fromGregorianValid (fromIntegral (century * 100 + year)) month day =
      let wholeSeconds = fromInteger (daysSinceEpoch date) * 86400
            + fromIntegral (hours * 3600 + minutes * 60 + seconds)
          finish fraction end = do
            offset <- if end == size
              then if not zoned && (separator == 0x20 || allowIsoLocal) then Just 0 else Nothing
              else if zoned then timestampOffset (TI.Text bytes (start + end) (size - end)) else Nothing
            pure ((wholeSeconds - fromIntegral offset) * 1000000 + fraction)
          fractional !index !places !micros
            | index == size || next > 9 = finish micros index
            | places == 0 = Nothing
            | otherwise = fractional (index + 1) (places `quot` 10) (micros + fromIntegral next * places)
            where
              next = digit index
      in if size > 19 && byte 19 == 0x2e
          then if size > 20 && digit 20 <= 9 then fractional 20 100000 0 else Nothing
          else finish 0 19
  | otherwise = Nothing
  where
    separator = byte 10
    byte index = TA.unsafeIndex bytes (start + index)
    digit index = byte index - 0x30
    pair :: Int -> Int
    pair index =
      let a = digit index
          b = digit (index + 1)
      in if a <= 9 && b <= 9 then 10 * fromIntegral a + fromIntegral b else -1


parseExtendedTimestamp :: (LocalTime -> Maybe Int) -> Bool -> Bool -> Text -> Maybe Integer
parseExtendedTimestamp localOffset allowIsoLocal zoned raw = do
  let (day, separatorAndTime) = T.break (\c -> c == ' ' || c == 'T') (T.strip raw)
  (separator, timeAndZone) <- T.uncons separatorAndTime
  let (time, zone) = T.break (\c -> c == 'Z' || c == '+' || c == '-') timeAndZone
      (_, fraction) = T.breakOn "." time
  -- Data.Time can discard digits after picoseconds during parsing.
  when (fraction == ".") Nothing
  unless (T.all (== '0') (T.drop 7 fraction)) Nothing
  local <- parseTimeM True defaultTimeLocale "%F %T%Q" (T.unpack (calendarYear day <> " " <> time))
  offset <- case zone of
    "" | separator == ' ' || (allowIsoLocal && not zoned) ->
      if zoned then localOffset local else Just 0
    _ | zoned -> timestampOffset zone
    _ -> Nothing
  unless (offset > -86400 && offset < 86400) Nothing
  let micros = floor (utcTimeToPOSIXSeconds (localTimeToUTC utc local) * 1000000)
  pure (micros - fromIntegral offset * 1000000)


timestampOffset :: Text -> Maybe Int
timestampOffset "Z" = Just 0
timestampOffset raw = do
  (sign, digits) <- T.uncons raw
  unless (sign == '+' || sign == '-') Nothing
  let (hourDigits, suffix) = T.splitAt 2 digits
  hours <- twoDigits hourDigits
  (minutes, seconds) <- case T.stripPrefix ":" suffix of
    Just rest -> do
      let (minuteDigits, tailDigits) = T.splitAt 2 rest
      minute <- twoDigits minuteDigits
      -- Spark can preserve the seconds of historical timezone offsets.
      second <- if T.null tailDigits then Just 0 else T.stripPrefix ":" tailDigits >>= twoDigits
      pure (minute, second)
    Nothing -> (,0) <$> twoDigits suffix
  unless (hours < 24 && minutes < 60) Nothing
  unless (seconds < 60) Nothing
  let magnitude = (hours * 60 + minutes) * 60 + seconds
  pure (if sign == '-' then negate magnitude else magnitude)
  where
    twoDigits value
      | T.length value == 2 && T.all decimalDigit value = readDigits value
      | otherwise = Nothing


-- Expose only the bounded exponent and significant digits. Keeping the sign
-- separate preserves negative zero for floating-point partitions.
data PartitionNumber = PartitionNumber !Bool !Integer !Text


partitionNumber :: Text -> Maybe PartitionNumber
partitionNumber raw = do
  let (negative, unsigned) = numberSign (T.strip raw)
      (mantissa, exponentPart) = T.break (\c -> c == 'e' || c == 'E') unsigned
      (whole, fractionPart) = T.breakOn "." mantissa
      fraction = T.drop 1 fractionPart
  when (T.null whole && T.null fraction) Nothing
  unless (T.all decimalDigit whole && T.all decimalDigit fraction) Nothing
  power <- if T.null exponentPart then Just 0 else decimalExponent (T.drop 1 exponentPart)
  pure (PartitionNumber negative (power - toInteger (T.length fraction)) (T.dropWhile (== '0') (whole <> fraction)))
  where
    decimalExponent value = do
      let (negative, digits) = numberSign value
          significant = T.dropWhile (== '0') digits
      when (T.null digits) Nothing
      unless (T.all decimalDigit digits) Nothing
      -- A larger exponent cannot cancel the digits of any Text value.
      magnitude <-
        if T.null significant then Just 0
        else if T.compareLength significant 19 == GT then Just (10 ^ (20 :: Int))
        else readDigits significant
      pure (if negative then negate magnitude else magnitude)


numberSign :: Text -> (Bool, Text)
numberSign value = case T.uncons value of
  Just ('-', rest) -> (True, rest)
  Just ('+', rest) -> (False, rest)
  _ -> (False, value)


-- Delta integer partitions are signed decimal strings. Bound the magnitude
-- during parsing so an overflowing value cannot build a large Integer.
partitionInteger :: forall a. (Bounded a, Integral a) => Text -> Maybe a
partitionInteger raw
  | T.null digits = Nothing
  | otherwise = go 0 digits
  where
    (negative, digits) = numberSign raw
    limit = fromInteger (if negative then negate (toInteger (minBound @a)) else toInteger (maxBound @a)) :: Word64
    (cutoff, lastDigit) = limit `quotRem` 10
    go !n remaining = case T.uncons remaining of
      Nothing ->
        let value = fromIntegral n
        in Just (if not negative then value else if n == limit then minBound else negate value)
      Just (c, rest)
        | decimalDigit c
        , let digit = fromIntegral (fromEnum c - fromEnum '0')
        , n < cutoff || (n == cutoff && digit <= lastDigit) -> go (n * 10 + digit) rest
      _ -> Nothing
{-# INLINE partitionInteger #-}


decimalDigit :: Char -> Bool
decimalDigit c = c >= '0' && c <= '9'


readDigits :: Integral a => Text -> Maybe a
readDigits = either (const Nothing) (Just . fst) . TR.decimal


-- Significant digits must fit the declared precision before we form a
-- power of ten.
unscaledDecimal :: Int -> Int -> Text -> Maybe Integer
unscaledDecimal precision scale raw = do
  PartitionNumber negative power significant <- partitionNumber raw
  let trimmed = T.dropWhileEnd (== '0') significant
  if T.null significant
    then Just 0
    else do
      let digits = toInteger (T.length trimmed)
          places = power + toInteger scale + toInteger (T.length significant) - digits
      unless (places >= 0 && places + digits <= toInteger precision) Nothing
      coefficient <- readDigits trimmed
      let magnitude = coefficient * 10 ^ (fromInteger places :: Int)
      pure (if negative then negate magnitude else magnitude)


floatingPartition :: RealFloat a => Text -> Maybe a
floatingPartition raw =
  let (negative, unsigned) = numberSign (T.strip raw)
  in case T.toLower unsigned of
    "nan" -> Just (0 / 0)
    "inf" -> Just (signed negative (1 / 0))
    "infinity" -> Just (signed negative (1 / 0))
    _ -> do
      PartitionNumber _ power digits <- partitionNumber raw
      let order = power + toInteger (T.length digits) - 1
      magnitude <-
        if T.null digits || order < -324 then Just 0
        else if order > 308 then Just (1 / 0)
        else do
          -- Binary64 rounding midpoints need at most 768 decimal digits.
          -- A sticky digit retains the side of a midpoint for longer inputs.
          let (prefix, rest) = T.splitAt 768 digits
              exp10 = fromInteger order + 1 - T.length prefix
          coeff <- readDigits prefix
          pure $ toRealFloat $
            if T.any (/= '0') rest then scientific (coeff * 10 + 1) (exp10 - 1)
            else scientific coeff exp10
      pure (signed negative magnitude)
  where
    signed negative value = if negative then negate value else value


checked :: forall a. (Bounded a, Integral a) => Integer -> Maybe a
checked n
  | n < toInteger (minBound `asTypeOf` value) || n > toInteger (maxBound `asTypeOf` value) = Nothing
  | otherwise = Just value
  where
    value = fromInteger n


convertTimestamp :: Maybe TimestampRebase -> A.TimeUnit -> C.ColumnArray -> Either String C.ColumnArray
convertTimestamp policy unit column@(C.ColTimestamp valid values) = case unit of
  A.Microsecond -> case policy of
    Just rebase | VS.any (needsTimestampRebase rebase) values ->
      buildTimestamps valid values (withCalendar rebase successMicros)
    _ -> Right column
  A.Second -> case policy of
    Nothing -> buildTimestamps valid values (scaleMicros 1000000)
    Just rebase -> buildTimestamps valid values (withCalendar rebase (scaleMicros 1000000))
  A.Millisecond -> case policy of
    Nothing -> buildTimestamps valid values (scaleMicros 1000)
    Just rebase -> buildTimestamps valid values (withCalendar rebase (scaleMicros 1000))
  A.Nanosecond -> case policy of
    Nothing -> buildTimestamps valid values exactMicros
    Just rebase -> buildTimestamps valid values (withCalendar rebase exactMicros)
  where
    exactMicros n = case n `quotRem` 1000 of
      (value, 0) -> successMicros value
      _ -> (# timestampOverflow | #)
convertTimestamp _ _ _ = Left "Delta.Scan: expected timestamp"


buildTimestamps
  :: Storable a
  => Maybe C.Validity
  -> VS.Vector a
  -> (a -> (# String | Int64# #))
  -> Either String C.ColumnArray
buildTimestamps valid values convert = convertMicros valid convert values >>= C.mkPrim C.PTimestamp valid
{-# INLINE buildTimestamps #-}


withCalendar
  :: TimestampRebase
  -> (Int64 -> (# String | Int64# #))
  -> Int64
  -> (# String | Int64# #)
withCalendar rebase convert n = case convert n of
  (# err | #) -> (# err | #)
  (# | micros #) -> case rebaseTimestamp rebase (I64# micros) of
    (# err | #) -> (# err | #)
    (# | rebased #) -> successMicros rebased
{-# INLINE withCalendar #-}


successMicros :: Int64 -> (# String | Int64# #)
successMicros (I64# micros) = (# | micros #)
{-# INLINE successMicros #-}


timestampOverflow :: String
timestampOverflow = "Delta.Scan: timestamp cannot be represented in microseconds"


scaleMicros :: Int64 -> Int64 -> (# String | Int64# #)
scaleMicros factor =
  let !lower = minBound `quot` factor
      !upper = maxBound `quot` factor
  in \n -> if n < lower || n > upper then (# timestampOverflow | #) else successMicros (n * factor)
{-# INLINE scaleMicros #-}


convertMicros
  :: Storable a
  => Maybe C.Validity
  -> (a -> (# String | Int64# #))
  -> VS.Vector a
  -> Either String (VS.Vector Int64)
convertMicros valid convert values = runST $ do
  let size = VS.length values
  output <- VSM.unsafeNew size
  let loop !i
        | i == size = Right <$> VS.unsafeFreeze output
        | not (C.isValidAt valid i) = VSM.unsafeWrite output i 0 >> loop (i + 1)
        | otherwise = case convert (VS.unsafeIndex values i) of
            (# err | #) -> pure (Left err)
            (# | n #) -> VSM.unsafeWrite output i (I64# n) >> loop (i + 1)
  loop 0
{-# INLINE convertMicros #-}


widen :: A.ArrowType -> A.ArrowType -> Either String (C.ColumnArray -> Either String C.ColumnArray)
widen source target = case (source, target) of
  (A.AInt s True, A.AInt t True) | s < t -> case t of
    16 -> Right (integerColumn C.PInt16)
    32 -> Right (integerColumn C.PInt32)
    64 -> Right (integerColumn C.PInt64)
    _ -> unsupported
  (A.AInt s True, A.AFloatingPoint A.DoublePrecision) | s <= 32 -> Right (integerColumn C.PDouble)
  (A.AFloatingPoint A.Single, A.AFloatingPoint A.DoublePrecision) -> Right $ \case
    C.ColFloat valid values -> C.mkPrim C.PDouble valid (VS.map float2Double values)
    _ -> Left "Delta.Scan: expected Float32"
  (A.ADate A.DateDay, A.ATimestamp A.Microsecond Nothing) -> Right $ \case
    C.ColDate32 valid values -> do
      let convert date = scaleMicros 86400000000 (fromIntegral date)
      times <- convertMicros valid convert values
      C.mkPrim C.PTimestamp valid times
    _ -> Left "Delta.Scan: expected Date32"
  (A.ADecimal p s, A.ADecimal p' s') | s' >= s && p' - s' >= p - s ->
    Right (widenDecimal p' s' (s' - s))
  (A.AInt s True, A.ADecimal p scale)
    | p - scale >= (if s == 64 then 20 else 10) ->
        Right (integralDecimal p scale)
  _ -> unsupported
  where
    unsupported = Left ("Delta.Scan: unsupported type change " ++ show source ++ " to " ++ show target)


integerColumn :: (Num a, Storable a) => C.PrimType a -> C.ColumnArray -> Either String C.ColumnArray
integerColumn target = \case
  C.ColInt8 valid values -> C.mkPrim target valid (VS.map fromIntegral values)
  C.ColInt16 valid values -> C.mkPrim target valid (VS.map fromIntegral values)
  C.ColInt32 valid values -> C.mkPrim target valid (VS.map fromIntegral values)
  _ -> Left "Delta.Scan: expected signed integer"


integralDecimal :: Int -> Int -> C.ColumnArray -> Either String C.ColumnArray
integralDecimal p s = \case
  C.ColInt8 valid values -> decimal valid values
  C.ColInt16 valid values -> decimal valid values
  C.ColInt32 valid values -> decimal valid values
  C.ColInt64 valid values -> decimal valid values
  _ -> Left "Delta.Scan: expected signed integer for decimal widening"
  where
    scale = rescaleDecimal s
    decimal :: (Integral a, Storable a) => Maybe C.Validity -> VS.Vector a -> Either String C.ColumnArray
    decimal valid values = C.mkPrim (C.PDecimal128 p s) valid (VS.map (scale . fromSigned . fromIntegral) values)
    fromSigned :: Int64 -> C.Decimal128
    fromSigned n = C.Decimal128 (fromIntegral n) (if n < 0 then maxBound else 0)


widenDecimal :: Int -> Int -> Int -> C.ColumnArray -> Either String C.ColumnArray
widenDecimal precision scale extraPlaces = \case
  C.ColDecimal128 _ _ valid values -> C.mkPrim (C.PDecimal128 precision scale) valid (rescale values)
  _ -> Left "Delta.Scan: expected Decimal128"
  where
    rescale = if extraPlaces == 0 then id else VS.map (rescaleDecimal extraPlaces)


-- Multiplication modulo 2^128 preserves the signed two's complement result.
-- The widening check guarantees that a source value fits the target precision.
rescaleDecimal :: Int -> C.Decimal128 -> C.Decimal128
rescaleDecimal places
  | places == 0 = id
  | finiteBitSize (0 :: Word) == 64 = multiply factor
  | otherwise = C.decimal128FromInteger . (* multiplier) . C.decimal128ToInteger
  where
    !multiplier = 10 ^ places
    !factor = C.decimal128FromInteger multiplier
    multiply (C.Decimal128 factorLow@(W64# y) factorHigh) (C.Decimal128 valueLow@(W64# x) valueHigh) =
      case timesWord2# (word64ToWord# x) (word64ToWord# y) of
        (# carry, low #) ->
          let !high = W64# (wordToWord64# carry) + valueLow * factorHigh + valueHigh * factorLow
          in C.Decimal128 (W64# (wordToWord64# low)) high
{-# INLINE rescaleDecimal #-}


reorderVariant :: C.ColumnArray -> Either String C.ColumnArray
reorderVariant (C.ColStruct n valid columns) = do
  let find name = maybe (Left "Delta.Scan: missing Variant storage field") Right (V.find ((== name) . fst) columns)
  metadata <- find "metadata"
  value <- find "value"
  C.mkStruct n valid (V.fromList [metadata, value])
reorderVariant _ = Left "Delta.Scan: invalid Variant storage"


toUnshredded :: A.Field -> C.ColumnArray -> Either String C.ColumnArray
toUnshredded source column = do
  reader <- VA.compileVariantArrowReader source column
  let n = C.columnLength column
  values <- V.generateM n $ \i -> traverse Variant.encodeVariant =<< VA.variantArrowAt reader i
  let metadata = C.fromByteStrings (maybe BS.empty Variant.variantMetadata <$> values)
      value = C.fromByteStrings (maybe BS.empty Variant.variantValue <$> values)
  C.mkStruct n (C.validity column) (V.fromList [("metadata", metadata), ("value", value)])
