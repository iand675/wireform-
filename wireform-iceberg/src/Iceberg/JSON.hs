{-# LANGUAGE BangPatterns #-}
-- | JSON serialization for Apache Iceberg table metadata.
--
-- Per the Iceberg specification, table metadata is stored as JSON.
-- This module provides encoding and decoding between 'TableMetadata'
-- and 'Aeson.Value'.
module Iceberg.JSON
  ( metadataToJSON
  , metadataFromJSON
    -- * Subcomponent codecs
    --
    -- These are exported so 'Iceberg.RestCatalog' (and other callers
    -- that work with table metadata fragments) don't have to duplicate
    -- the JSON shape conventions.
  , schemaToJSON
  , schemaFromJSON
  , snapshotToJSON
  , snapshotFromJSON
  , partitionSpecToJSON
  , partitionSpecFromJSON
  , sortOrderToJSON
  , sortOrderFromJSON
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Iceberg.Types

-- | Encode 'TableMetadata' to its JSON representation.
metadataToJSON :: TableMetadata -> Aeson.Value
metadataToJSON tm = Aeson.Object $ KM.fromList
  [ ("format-version",       Aeson.Number (fromIntegral (tmFormatVersion tm)))
  , ("table-uuid",           Aeson.String (tmTableUuid tm))
  , ("location",             Aeson.String (tmLocation tm))
  , ("last-sequence-number", Aeson.Number (fromIntegral (tmLastSequenceNumber tm)))
  , ("last-updated-ms",      Aeson.Number (fromIntegral (tmLastUpdatedMs tm)))
  , ("last-column-id",       Aeson.Number (fromIntegral (tmLastColumnId tm)))
  , ("current-schema-id",    Aeson.Number (fromIntegral (tmCurrentSchemaId tm)))
  , ("schemas",              Aeson.Array (V.map schemaToJSON (tmSchemas tm)))
  , ("current-snapshot-id",  maybe Aeson.Null (Aeson.Number . fromIntegral) (tmCurrentSnapshotId tm))
  , ("snapshots",            Aeson.Array (V.map snapshotToJSON (tmSnapshots tm)))
  , ("partition-specs",      Aeson.Array (V.map partitionSpecToJSON (tmPartitionSpecs tm)))
  , ("default-spec-id",      Aeson.Number (fromIntegral (tmDefaultSpecId tm)))
  , ("sort-orders",          Aeson.Array (V.map sortOrderToJSON (tmSortOrders tm)))
  , ("default-sort-order-id", Aeson.Number (fromIntegral (tmDefaultSortOrderId tm)))
  , ("properties",           mapToJSON (tmProperties tm))
  , ("snapshot-log",         Aeson.Array (V.map snapshotLogEntryToJSON (tmSnapshotLog tm)))
  , ("snapshot-refs",        snapshotRefsToJSON (tmSnapshotRefs tm))
  ]

-- | Decode 'TableMetadata' from its JSON representation.
metadataFromJSON :: Aeson.Value -> Either String TableMetadata
metadataFromJSON (Aeson.Object obj) = do
  fmtVer   <- reqInt "format-version" obj
  uuid     <- reqStr "table-uuid" obj
  loc      <- reqStr "location" obj
  lastSeq  <- reqInt64 "last-sequence-number" obj
  lastUpd  <- reqInt64 "last-updated-ms" obj
  lastCol  <- reqInt "last-column-id" obj
  curSch   <- reqInt "current-schema-id" obj
  schemas  <- reqArray "schemas" obj >>= V.mapM schemaFromJSON
  curSnap  <- optInt64 "current-snapshot-id" obj
  snaps    <- reqArray "snapshots" obj >>= V.mapM snapshotFromJSON
  pspecs   <- reqArray "partition-specs" obj >>= V.mapM partitionSpecFromJSON
  defSpec  <- reqInt "default-spec-id" obj
  sords    <- reqArray "sort-orders" obj >>= V.mapM sortOrderFromJSON
  defSort  <- reqInt "default-sort-order-id" obj
  props    <- mapFromJSON "properties" obj
  slog     <- reqArray "snapshot-log" obj >>= V.mapM snapshotLogEntryFromJSON
  refs     <- snapshotRefsFromJSON obj
  Right TableMetadata
    { tmFormatVersion      = fmtVer
    , tmTableUuid          = uuid
    , tmLocation           = loc
    , tmLastSequenceNumber = lastSeq
    , tmLastUpdatedMs      = lastUpd
    , tmLastColumnId       = lastCol
    , tmCurrentSchemaId    = curSch
    , tmSchemas            = schemas
    , tmCurrentSnapshotId  = curSnap
    , tmSnapshots          = snaps
    , tmPartitionSpecs     = pspecs
    , tmDefaultSpecId      = defSpec
    , tmSortOrders         = sords
    , tmDefaultSortOrderId = defSort
    , tmProperties         = props
    , tmSnapshotLog        = slog
    , tmSnapshotRefs       = refs
    }
metadataFromJSON _ = Left "table metadata must be a JSON object"

-- ============================================================
-- Schema
-- ============================================================

schemaToJSON :: Schema -> Aeson.Value
schemaToJSON s = Aeson.Object $ KM.fromList
  [ ("schema-id", Aeson.Number (fromIntegral (schemaId s)))
  , ("type", Aeson.String "struct")
  , ("fields", Aeson.Array (V.map structFieldToJSON (schemaFields s)))
  ]

schemaFromJSON :: Aeson.Value -> Either String Schema
schemaFromJSON (Aeson.Object obj) = do
  sid    <- reqInt "schema-id" obj
  fields <- reqArray "fields" obj >>= V.mapM structFieldFromJSON
  Right Schema { schemaId = sid, schemaFields = fields }
schemaFromJSON _ = Left "schema must be a JSON object"

-- ============================================================
-- StructField
-- ============================================================

structFieldToJSON :: StructField -> Aeson.Value
structFieldToJSON sf = Aeson.Object $ KM.fromList $
  [ ("id",       Aeson.Number (fromIntegral (sfId sf)))
  , ("name",     Aeson.String (sfName sf))
  , ("required", Aeson.Bool (sfRequired sf))
  , ("type",     icebergTypeToJSON (sfType sf))
  ] ++ maybe [] (\d -> [("doc", Aeson.String d)]) (sfDoc sf)

structFieldFromJSON :: Aeson.Value -> Either String StructField
structFieldFromJSON (Aeson.Object obj) = do
  fid  <- reqInt "id" obj
  name <- reqStr "name" obj
  req  <- reqBool "required" obj
  ty   <- case KM.lookup "type" obj of
            Just v  -> icebergTypeFromJSON v
            Nothing -> Left "field missing 'type'"
  let doc = case KM.lookup "doc" obj of
              Just (Aeson.String d) -> Just d
              _                     -> Nothing
  Right StructField { sfId = fid, sfName = name, sfRequired = req, sfType = ty, sfDoc = doc }
structFieldFromJSON _ = Left "struct field must be a JSON object"

-- ============================================================
-- IcebergType
-- ============================================================

icebergTypeToJSON :: IcebergType -> Aeson.Value
icebergTypeToJSON TBoolean     = Aeson.String "boolean"
icebergTypeToJSON TInt         = Aeson.String "int"
icebergTypeToJSON TLong        = Aeson.String "long"
icebergTypeToJSON TFloat       = Aeson.String "float"
icebergTypeToJSON TDouble      = Aeson.String "double"
icebergTypeToJSON TDate        = Aeson.String "date"
icebergTypeToJSON TTime        = Aeson.String "time"
icebergTypeToJSON TTimestamp   = Aeson.String "timestamp"
icebergTypeToJSON TTimestampTz = Aeson.String "timestamptz"
icebergTypeToJSON TString      = Aeson.String "string"
icebergTypeToJSON TUuid        = Aeson.String "uuid"
icebergTypeToJSON (TFixed n)   = Aeson.String (T.pack ("fixed[" ++ show n ++ "]"))
icebergTypeToJSON TBinary      = Aeson.String "binary"
icebergTypeToJSON (TDecimal p s) = Aeson.String (T.pack ("decimal(" ++ show p ++ ", " ++ show s ++ ")"))
icebergTypeToJSON (TStruct fields) = Aeson.Object $ KM.fromList
  [ ("type", Aeson.String "struct")
  , ("fields", Aeson.Array (V.map structFieldToJSON fields))
  ]
icebergTypeToJSON (TList elemId elemTy) = Aeson.Object $ KM.fromList
  [ ("type", Aeson.String "list")
  , ("element-id", Aeson.Number (fromIntegral elemId))
  , ("element", icebergTypeToJSON elemTy)
  , ("element-required", Aeson.Bool False)
  ]
icebergTypeToJSON (TMap keyId keyTy valId valTy) = Aeson.Object $ KM.fromList
  [ ("type", Aeson.String "map")
  , ("key-id", Aeson.Number (fromIntegral keyId))
  , ("key", icebergTypeToJSON keyTy)
  , ("value-id", Aeson.Number (fromIntegral valId))
  , ("value", icebergTypeToJSON valTy)
  , ("value-required", Aeson.Bool False)
  ]

icebergTypeFromJSON :: Aeson.Value -> Either String IcebergType
icebergTypeFromJSON (Aeson.String s) = case s of
  "boolean"     -> Right TBoolean
  "int"         -> Right TInt
  "long"        -> Right TLong
  "float"       -> Right TFloat
  "double"      -> Right TDouble
  "date"        -> Right TDate
  "time"        -> Right TTime
  "timestamp"   -> Right TTimestamp
  "timestamptz" -> Right TTimestampTz
  "string"      -> Right TString
  "uuid"        -> Right TUuid
  "binary"      -> Right TBinary
  other
    | T.isPrefixOf "fixed[" other -> parseFixed other
    | T.isPrefixOf "decimal(" other -> parseDecimal other
    | otherwise -> Left $ "unknown iceberg type: " ++ T.unpack other
icebergTypeFromJSON (Aeson.Object obj) = do
  typStr <- reqStr "type" obj
  case typStr of
    "struct" -> do
      fields <- reqArray "fields" obj >>= V.mapM structFieldFromJSON
      Right (TStruct fields)
    "list" -> do
      elemId <- reqInt "element-id" obj
      elemTy <- case KM.lookup "element" obj of
                  Just v  -> icebergTypeFromJSON v
                  Nothing -> Left "list missing 'element'"
      Right (TList elemId elemTy)
    "map" -> do
      keyId <- reqInt "key-id" obj
      keyTy <- case KM.lookup "key" obj of
                 Just v  -> icebergTypeFromJSON v
                 Nothing -> Left "map missing 'key'"
      valId <- reqInt "value-id" obj
      valTy <- case KM.lookup "value" obj of
                 Just v  -> icebergTypeFromJSON v
                 Nothing -> Left "map missing 'value'"
      Right (TMap keyId keyTy valId valTy)
    other -> Left $ "unknown complex iceberg type: " ++ T.unpack other
icebergTypeFromJSON _ = Left "iceberg type must be a string or object"

parseFixed :: Text -> Either String IcebergType
parseFixed t =
  let inner = T.dropEnd 1 (T.drop 6 t)
  in case reads (T.unpack inner) of
       [(n, "")] -> Right (TFixed n)
       _         -> Left $ "invalid fixed type: " ++ T.unpack t

parseDecimal :: Text -> Either String IcebergType
parseDecimal t =
  let inner = T.dropEnd 1 (T.drop 8 t)
      parts = T.splitOn ", " inner
  in case parts of
       [pTxt, sTxt] ->
         case (reads (T.unpack pTxt), reads (T.unpack sTxt)) of
           ([(p, "")], [(s, "")]) -> Right (TDecimal p s)
           _ -> Left $ "invalid decimal type: " ++ T.unpack t
       _ -> Left $ "invalid decimal type: " ++ T.unpack t

-- ============================================================
-- Snapshot
-- ============================================================

snapshotToJSON :: Snapshot -> Aeson.Value
snapshotToJSON s = Aeson.Object $ KM.fromList $
  [ ("snapshot-id",      Aeson.Number (fromIntegral (snapId s)))
  , ("sequence-number",  Aeson.Number (fromIntegral (snapSequenceNumber s)))
  , ("timestamp-ms",     Aeson.Number (fromIntegral (snapTimestampMs s)))
  , ("manifest-list",    Aeson.String (snapManifestList s))
  , ("summary",          mapToJSON (snapSummary s))
  ] ++ maybe [] (\pid -> [("parent-snapshot-id", Aeson.Number (fromIntegral pid))]) (snapParentId s)

snapshotFromJSON :: Aeson.Value -> Either String Snapshot
snapshotFromJSON (Aeson.Object obj) = do
  sid    <- reqInt64 "snapshot-id" obj
  seqn   <- reqInt64 "sequence-number" obj
  ts     <- reqInt64 "timestamp-ms" obj
  ml     <- reqStr "manifest-list" obj
  summ   <- mapFromJSON "summary" obj
  let pid = case KM.lookup "parent-snapshot-id" obj of
              Just (Aeson.Number n) -> case toBoundedInteger n of
                Just i  -> Just (i :: Int64)
                Nothing -> Nothing
              _ -> Nothing
  Right Snapshot
    { snapId = sid, snapParentId = pid, snapSequenceNumber = seqn
    , snapTimestampMs = ts, snapManifestList = ml, snapSummary = summ
    }
snapshotFromJSON _ = Left "snapshot must be a JSON object"

-- ============================================================
-- PartitionSpec
-- ============================================================

partitionSpecToJSON :: PartitionSpec -> Aeson.Value
partitionSpecToJSON ps = Aeson.Object $ KM.fromList
  [ ("spec-id", Aeson.Number (fromIntegral (psSpecId ps)))
  , ("fields",  Aeson.Array (V.map partitionFieldToJSON (psFields ps)))
  ]

partitionSpecFromJSON :: Aeson.Value -> Either String PartitionSpec
partitionSpecFromJSON (Aeson.Object obj) = do
  sid <- reqInt "spec-id" obj
  fs  <- reqArray "fields" obj >>= V.mapM partitionFieldFromJSON
  Right PartitionSpec { psSpecId = sid, psFields = fs }
partitionSpecFromJSON _ = Left "partition spec must be a JSON object"

partitionFieldToJSON :: PartitionField -> Aeson.Value
partitionFieldToJSON pf = Aeson.Object $ KM.fromList
  [ ("source-id",  Aeson.Number (fromIntegral (pfSourceId pf)))
  , ("field-id",   Aeson.Number (fromIntegral (pfFieldId pf)))
  , ("name",       Aeson.String (pfName pf))
  , ("transform",  transformToJSON (pfTransform pf))
  ]

partitionFieldFromJSON :: Aeson.Value -> Either String PartitionField
partitionFieldFromJSON (Aeson.Object obj) = do
  sid  <- reqInt "source-id" obj
  fid  <- reqInt "field-id" obj
  name <- reqStr "name" obj
  tr   <- case KM.lookup "transform" obj of
            Just v  -> transformFromJSON v
            Nothing -> Left "partition field missing 'transform'"
  Right PartitionField { pfSourceId = sid, pfFieldId = fid, pfName = name, pfTransform = tr }
partitionFieldFromJSON _ = Left "partition field must be a JSON object"

-- ============================================================
-- Transform
-- ============================================================

transformToJSON :: Transform -> Aeson.Value
transformToJSON Identity     = Aeson.String "identity"
transformToJSON (Bucket n)   = Aeson.String (T.pack ("bucket[" ++ show n ++ "]"))
transformToJSON (Truncate n) = Aeson.String (T.pack ("truncate[" ++ show n ++ "]"))
transformToJSON Year         = Aeson.String "year"
transformToJSON Month        = Aeson.String "month"
transformToJSON Day          = Aeson.String "day"
transformToJSON Hour         = Aeson.String "hour"
transformToJSON Void         = Aeson.String "void"

transformFromJSON :: Aeson.Value -> Either String Transform
transformFromJSON (Aeson.String s) = case s of
  "identity" -> Right Identity
  "year"     -> Right Year
  "month"    -> Right Month
  "day"      -> Right Day
  "hour"     -> Right Hour
  "void"     -> Right Void
  other
    | T.isPrefixOf "bucket[" other ->
        case reads (T.unpack (T.dropEnd 1 (T.drop 7 other))) of
          [(n, "")] -> Right (Bucket n)
          _         -> Left $ "invalid bucket transform: " ++ T.unpack other
    | T.isPrefixOf "truncate[" other ->
        case reads (T.unpack (T.dropEnd 1 (T.drop 9 other))) of
          [(n, "")] -> Right (Truncate n)
          _         -> Left $ "invalid truncate transform: " ++ T.unpack other
    | otherwise -> Left $ "unknown transform: " ++ T.unpack other
transformFromJSON _ = Left "transform must be a string"

-- ============================================================
-- SortOrder
-- ============================================================

sortOrderToJSON :: SortOrder -> Aeson.Value
sortOrderToJSON so = Aeson.Object $ KM.fromList
  [ ("order-id", Aeson.Number (fromIntegral (soOrderId so)))
  , ("fields",   Aeson.Array (V.map sortFieldToJSON (soFields so)))
  ]

sortOrderFromJSON :: Aeson.Value -> Either String SortOrder
sortOrderFromJSON (Aeson.Object obj) = do
  oid <- reqInt "order-id" obj
  fs  <- reqArray "fields" obj >>= V.mapM sortFieldFromJSON
  Right SortOrder { soOrderId = oid, soFields = fs }
sortOrderFromJSON _ = Left "sort order must be a JSON object"

sortFieldToJSON :: SortField -> Aeson.Value
sortFieldToJSON sf = Aeson.Object $ KM.fromList
  [ ("source-id",  Aeson.Number (fromIntegral (sortSourceId sf)))
  , ("transform",  transformToJSON (sortTransform sf))
  , ("direction",  directionToJSON (sortDirection sf))
  , ("null-order", nullOrderToJSON (sortNullOrder sf))
  ]

sortFieldFromJSON :: Aeson.Value -> Either String SortField
sortFieldFromJSON (Aeson.Object obj) = do
  sid <- reqInt "source-id" obj
  tr  <- case KM.lookup "transform" obj of
           Just v  -> transformFromJSON v
           Nothing -> Left "sort field missing 'transform'"
  dir <- case KM.lookup "direction" obj of
           Just v  -> directionFromJSON v
           Nothing -> Left "sort field missing 'direction'"
  no  <- case KM.lookup "null-order" obj of
           Just v  -> nullOrderFromJSON v
           Nothing -> Left "sort field missing 'null-order'"
  Right SortField { sortSourceId = sid, sortTransform = tr, sortDirection = dir, sortNullOrder = no }
sortFieldFromJSON _ = Left "sort field must be a JSON object"

directionToJSON :: SortDirection -> Aeson.Value
directionToJSON Asc  = Aeson.String "asc"
directionToJSON Desc = Aeson.String "desc"

directionFromJSON :: Aeson.Value -> Either String SortDirection
directionFromJSON (Aeson.String "asc")  = Right Asc
directionFromJSON (Aeson.String "desc") = Right Desc
directionFromJSON _ = Left "direction must be 'asc' or 'desc'"

nullOrderToJSON :: NullOrder -> Aeson.Value
nullOrderToJSON NullsFirst = Aeson.String "nulls-first"
nullOrderToJSON NullsLast  = Aeson.String "nulls-last"

nullOrderFromJSON :: Aeson.Value -> Either String NullOrder
nullOrderFromJSON (Aeson.String "nulls-first") = Right NullsFirst
nullOrderFromJSON (Aeson.String "nulls-last")  = Right NullsLast
nullOrderFromJSON _ = Left "null-order must be 'nulls-first' or 'nulls-last'"

-- ============================================================
-- SnapshotLogEntry
-- ============================================================

snapshotLogEntryToJSON :: SnapshotLogEntry -> Aeson.Value
snapshotLogEntryToJSON sle = Aeson.Object $ KM.fromList
  [ ("timestamp-ms", Aeson.Number (fromIntegral (sleTimestampMs sle)))
  , ("snapshot-id",  Aeson.Number (fromIntegral (sleSnapshotId sle)))
  ]

snapshotLogEntryFromJSON :: Aeson.Value -> Either String SnapshotLogEntry
snapshotLogEntryFromJSON (Aeson.Object obj) = do
  ts  <- reqInt64 "timestamp-ms" obj
  sid <- reqInt64 "snapshot-id" obj
  Right SnapshotLogEntry { sleTimestampMs = ts, sleSnapshotId = sid }
snapshotLogEntryFromJSON _ = Left "snapshot log entry must be a JSON object"

-- ============================================================
-- SnapshotRef
-- ============================================================

snapshotRefToJSON :: SnapshotRef -> Aeson.Value
snapshotRefToJSON sr = Aeson.Object $ KM.fromList $
  [ ("snapshot-id", Aeson.Number (fromIntegral (srSnapshotId sr)))
  , ("type",        Aeson.String (srType sr))
  ] ++ maybe [] (\v -> [("max-ref-age-ms", Aeson.Number (fromIntegral v))]) (srMaxRefAgeMs sr)
    ++ maybe [] (\v -> [("max-snapshot-age-ms", Aeson.Number (fromIntegral v))]) (srMaxSnapshotAgeMs sr)
    ++ maybe [] (\v -> [("min-snapshots-to-keep", Aeson.Number (fromIntegral v))]) (srMinSnapshotsToKeep sr)

snapshotRefFromJSON :: Aeson.Value -> Either String SnapshotRef
snapshotRefFromJSON (Aeson.Object obj) = do
  sid     <- reqInt64 "snapshot-id" obj
  typ     <- reqStr "type" obj
  maxRef  <- optInt64 "max-ref-age-ms" obj
  maxSnap <- optInt64 "max-snapshot-age-ms" obj
  minKeep <- optInt32 "min-snapshots-to-keep" obj
  Right SnapshotRef
    { srSnapshotId = sid
    , srType = typ
    , srMaxRefAgeMs = maxRef
    , srMaxSnapshotAgeMs = maxSnap
    , srMinSnapshotsToKeep = minKeep
    }
snapshotRefFromJSON _ = Left "snapshot ref must be a JSON object"

snapshotRefsToJSON :: Map.Map Text SnapshotRef -> Aeson.Value
snapshotRefsToJSON m = Aeson.Object $ KM.fromList $
  map (\(k, v) -> (Key.fromText k, snapshotRefToJSON v)) (Map.toList m)

snapshotRefsFromJSON :: KM.KeyMap Aeson.Value -> Either String (Map.Map Text SnapshotRef)
snapshotRefsFromJSON obj = case KM.lookup "snapshot-refs" obj of
  Just (Aeson.Object m) ->
    Map.fromList <$> mapM (\(k, v) -> do
      ref <- snapshotRefFromJSON v
      Right (Key.toText k, ref)
    ) (KM.toList m)
  Just Aeson.Null -> Right Map.empty
  Nothing         -> Right Map.empty
  _               -> Left "snapshot-refs must be an object"

-- ============================================================
-- Helpers
-- ============================================================

mapToJSON :: Map.Map Text Text -> Aeson.Value
mapToJSON m = Aeson.Object $ KM.fromList
  [(Key.fromText k, Aeson.String v) | (k, v) <- Map.toList m]

mapFromJSON :: Text -> KM.KeyMap Aeson.Value -> Either String (Map.Map Text Text)
mapFromJSON key obj = case KM.lookup (Key.fromText key) obj of
  Just (Aeson.Object m) ->
    Map.fromList <$> mapM (\(k, v) -> case v of
      Aeson.String s -> Right (Key.toText k, s)
      _ -> Left $ "map values must be strings in " ++ T.unpack key
      ) (KM.toList m)
  Just Aeson.Null -> Right Map.empty
  Nothing -> Right Map.empty
  _ -> Left $ T.unpack key ++ " must be an object"

reqStr :: Text -> KM.KeyMap Aeson.Value -> Either String Text
reqStr k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.String s) -> Right s
  _ -> Left $ "missing or non-string field: " ++ T.unpack k

reqBool :: Text -> KM.KeyMap Aeson.Value -> Either String Bool
reqBool k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Bool b) -> Right b
  _ -> Left $ "missing or non-boolean field: " ++ T.unpack k

reqInt :: Text -> KM.KeyMap Aeson.Value -> Either String Int
reqInt k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Number n) -> case toBoundedInteger n of
    Just i  -> Right i
    Nothing -> Left $ "field out of Int range: " ++ T.unpack k
  _ -> Left $ "missing or non-numeric field: " ++ T.unpack k

reqInt64 :: Text -> KM.KeyMap Aeson.Value -> Either String Int64
reqInt64 k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Number n) -> case toBoundedInteger n of
    Just i  -> Right i
    Nothing -> Left $ "field out of Int64 range: " ++ T.unpack k
  _ -> Left $ "missing or non-numeric field: " ++ T.unpack k

optInt64 :: Text -> KM.KeyMap Aeson.Value -> Either String (Maybe Int64)
optInt64 k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Number n) -> case toBoundedInteger n of
    Just i  -> Right (Just i)
    Nothing -> Left $ "field out of Int64 range: " ++ T.unpack k
  Just Aeson.Null -> Right Nothing
  Nothing         -> Right Nothing
  _ -> Left $ "non-numeric field: " ++ T.unpack k

optInt32 :: Text -> KM.KeyMap Aeson.Value -> Either String (Maybe Int32)
optInt32 k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Number n) -> case toBoundedInteger n of
    Just i  -> Right (Just i)
    Nothing -> Left $ "field out of Int32 range: " ++ T.unpack k
  Just Aeson.Null -> Right Nothing
  Nothing         -> Right Nothing
  _ -> Left $ "non-numeric field: " ++ T.unpack k

reqArray :: Text -> KM.KeyMap Aeson.Value -> Either String (V.Vector Aeson.Value)
reqArray k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Array arr) -> Right arr
  _ -> Left $ "missing or non-array field: " ++ T.unpack k
