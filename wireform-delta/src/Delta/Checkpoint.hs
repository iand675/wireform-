{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Decoder for Delta Lake @*.checkpoint.parquet@ files.

A Delta checkpoint Parquet file folds the entire active log
of a table into a single columnar snapshot. Each row carries
exactly one of the action variants (@add@ / @remove@ /
@metaData@ / @protocol@ / @txn@ / …) under a top-level
struct column whose name matches the action; all other
action structs on that row are null. By reading these
columns, consumers can reconstruct a 'Delta.Log.TableSnapshot'
without walking the @NNNN.json@ commit files from version 0.

This module decodes @add@, @remove@, @metaData@, @protocol@,
and @txn@. Domain metadata and sidecar rows are surfaced as
'ActionOther'.

/Implementation notes:/

  * The Parquet schema has duplicate leaf names across struct
    parents (@add.path@, @remove.path@, @sidecar.path@, …).
    We resolve each leaf by its full schema path
    (@'cmPathInSchema'@) rather than by leaf name, sidestepping
    the @parquetFileArrowSchema@ flattening that would
    conflate them.
  * Per-leaf reading goes through the existing
    @readGenericXxxOptionalColumnChunk@ family, which handles
    PLAIN, PLAIN_DICTIONARY and RLE_DICTIONARY in a single
    pass. Delta checkpoints written by delta-rs / Spark are
    dictionary-encoded for the high-cardinality string
    columns and PLAIN for the low-cardinality bool / int
    ones; both shapes thread through the generic reader.
  * For each row we look at the @add.path@ / @remove.path@ /
    @metaData.id@ / @protocol.minReaderVersion@ leaves: if
    the row's value is 'Just _' the corresponding struct is
    present, so we attribute the row to that variant.
-}
module Delta.Checkpoint (
  -- * Decode
  decodeCheckpointFile,
  readCheckpointFile,

  -- * Replay
  checkpointToActions,
  snapshotFromCheckpoint,
) where

import Control.Exception (SomeException, try)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson (fromString)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word64)
import Delta.Log (
  AddAction (..),
  DeltaAction (..),
  MetaDataAction (..),
  ProtocolAction (..),
  RemoveAction (..),
  TableSnapshot,
  TxnAction (..),
  snapshotFromActions,
 )
import Parquet.Levels qualified as PL
import Parquet.Read qualified as PR
import Parquet.Types qualified as P


-- ============================================================
-- Decode
-- ============================================================

{- | Decode every action row in a Delta checkpoint Parquet
payload into a flat list of 'DeltaAction's. Rows whose
variant we don't recognise (@domainMetadata@,
@sidecar@) are surfaced as 'ActionOther' rather than
dropped, so callers can see they exist.
-}
decodeCheckpointFile :: ByteString -> Either String [DeltaAction]
decodeCheckpointFile bs = do
  pf <- PR.loadParquetFile bs
  let rgs = V.toList (P.fmRowGroups (PR.pfFooter pf))
  rgActions <- mapM (rowGroupActions pf) rgs
  Right (concat rgActions)


{- | Convenience: read the checkpoint file off disk and decode
it. Wraps the underlying bytes-loading exception in 'Either'
so callers don't have to install their own handler.
-}
readCheckpointFile :: FilePath -> IO (Either String [DeltaAction])
readCheckpointFile fp = do
  res <- try (BS.readFile fp) :: IO (Either SomeException ByteString)
  case res of
    Left e -> pure (Left ("Delta.Checkpoint: " ++ show e))
    Right bs -> pure (decodeCheckpointFile bs)


-- ============================================================
-- Replay
-- ============================================================

{- | Re-emit the action stream in row order. Useful when the
caller wants to feed a checkpoint into the same fold they
use for a JSON commit walk.
-}
checkpointToActions :: ByteString -> Either String [DeltaAction]
checkpointToActions = decodeCheckpointFile


{- | Apply every checkpoint row to 'snapshotFromActions',
yielding a 'TableSnapshot' at the checkpoint version.
-}
snapshotFromCheckpoint :: ByteString -> Either String TableSnapshot
snapshotFromCheckpoint = fmap snapshotFromActions . decodeCheckpointFile


-- ============================================================
-- Internal: per-row-group walker
-- ============================================================

{- | Decode every row in one row-group into a list of
'DeltaAction's, in row order. Reads only the leaves we
actually need; the rest of the column chunks are never
touched.
-}
rowGroupActions
  :: PR.ParquetFile
  -> P.RowGroup
  -> Either String [DeltaAction]
rowGroupActions pf rg = do
  let n = fromIntegral (P.rgNumRows rg) :: Int
  -- 'add' columns
  addPath_ <- readByteArrayOpt pf rg ["add", "path"]
  addSize_ <- readInt64Opt pf rg ["add", "size"]
  addModTime_ <- readInt64Opt pf rg ["add", "modificationTime"]
  addDataChg_ <- readBoolOpt pf rg ["add", "dataChange"]
  addStats_ <- readByteArrayOpt pf rg ["add", "stats"]
  addPartK_ <-
    readByteArrayRep
      pf
      rg
      ["add", "partitionValues", "key_value", "key"]
  -- partitionValues' value leaf is Parquet-optional: the Delta
  -- protocol allows null partition values for null partition
  -- columns. Read with a parent_empty_def of maxDef-2 so the
  -- map row stays present (with a 'Nothing' value) instead of
  -- collapsing to "empty map".
  addPartV_ <-
    readByteArrayRepOptElem
      pf
      rg
      ["add", "partitionValues", "key_value", "value"]
  addTagsK_ <- readByteArrayRep pf rg ["add", "tags", "key_value", "key"]
  -- tags is map<string, string> (required value).
  addTagsV_ <- readByteArrayRep pf rg ["add", "tags", "key_value", "value"]
  addDv <- readDeletionVectorCols pf rg "add"
  -- 'remove' columns
  remPath_ <- readByteArrayOpt pf rg ["remove", "path"]
  remDelTs_ <- readInt64Opt pf rg ["remove", "deletionTimestamp"]
  remDataChg_ <- readBoolOpt pf rg ["remove", "dataChange"]
  remSize_ <- readInt64Opt pf rg ["remove", "size"]
  remPartK_ <-
    readByteArrayRep
      pf
      rg
      ["remove", "partitionValues", "key_value", "key"]
  remPartV_ <-
    readByteArrayRepOptElem
      pf
      rg
      ["remove", "partitionValues", "key_value", "value"]
  remExt_ <- readBoolOpt pf rg ["remove", "extendedFileMetadata"]
  remDv <- readDeletionVectorCols pf rg "remove"
  -- 'metaData' columns
  metaId_ <- readByteArrayOpt pf rg ["metaData", "id"]
  metaName_ <- readByteArrayOpt pf rg ["metaData", "name"]
  metaDesc_ <- readByteArrayOpt pf rg ["metaData", "description"]
  metaSchema_ <- readByteArrayOpt pf rg ["metaData", "schemaString"]
  metaProv_ <- readByteArrayOpt pf rg ["metaData", "format", "provider"]
  metaOptK_ <-
    readByteArrayRep
      pf
      rg
      ["metaData", "format", "options", "key_value", "key"]
  metaOptV_ <-
    readByteArrayRep
      pf
      rg
      ["metaData", "format", "options", "key_value", "value"]
  metaPart_ <-
    readByteArrayRep
      pf
      rg
      ["metaData", "partitionColumns", "list", "element"]
  metaConfK_ <-
    readByteArrayRep
      pf
      rg
      ["metaData", "configuration", "key_value", "key"]
  metaConfV_ <-
    readByteArrayRep
      pf
      rg
      ["metaData", "configuration", "key_value", "value"]
  metaCt_ <- readInt64Opt pf rg ["metaData", "createdTime"]
  -- 'protocol' columns
  protMinR_ <- readInt32Opt pf rg ["protocol", "minReaderVersion"]
  protMinW_ <- readInt32Opt pf rg ["protocol", "minWriterVersion"]
  protRdrFt_ <-
    readByteArrayRep
      pf
      rg
      ["protocol", "readerFeatures", "list", "element"]
  protWrtFt_ <-
    readByteArrayRep
      pf
      rg
      ["protocol", "writerFeatures", "list", "element"]
  txnApp_ <- readByteArrayOpt pf rg ["txn", "appId"]
  txnVersion_ <- readInt64Opt pf rg ["txn", "version"]
  txnLastUpdated_ <- readInt64Opt pf rg ["txn", "lastUpdated"]
  -- 'domainMetadata' / 'sidecar' presence
  domDom_ <- readByteArrayOpt pf rg ["domainMetadata", "domain"]
  sideP_ <- readByteArrayOpt pf rg ["sidecar", "path"]

  pure
    [ rowAction
        i
        AddCols
          { addPath_ = addPath_
          , addSize_ = addSize_
          , addModTime_ = addModTime_
          , addDataChg_ = addDataChg_
          , addStats_ = addStats_
          , addPartK_ = addPartK_
          , addPartV_ = addPartV_
          , addTagsK_ = addTagsK_
          , addTagsV_ = addTagsV_
          , addDv_ = addDv
          }
        RemCols
          { remPath_ = remPath_
          , remDelTs_ = remDelTs_
          , remDataChg_ = remDataChg_
          , remSize_ = remSize_
          , remPartK_ = remPartK_
          , remPartV_ = remPartV_
          , remExt_ = remExt_
          , remDv_ = remDv
          }
        MetaCols
          { metaId_ = metaId_
          , metaName_ = metaName_
          , metaDesc_ = metaDesc_
          , metaSchema_ = metaSchema_
          , metaProv_ = metaProv_
          , metaOptK_ = metaOptK_
          , metaOptV_ = metaOptV_
          , metaPart_ = metaPart_
          , metaConfK_ = metaConfK_
          , metaConfV_ = metaConfV_
          , metaCt_ = metaCt_
          }
        ProtCols
          { protMinR_ = protMinR_
          , protMinW_ = protMinW_
          , protRdrFt_ = protRdrFt_
          , protWrtFt_ = protWrtFt_
          }
        OtherCols
          { txnApp_ = txnApp_
          , txnVersion_ = txnVersion_
          , txnLastUpdated_ = txnLastUpdated_
          , domDom_ = domDom_
          , sideP_ = sideP_
          }
    | i <- [0 .. n - 1]
    ]


-- ============================================================
-- Per-action column bundles (so 'rowAction' takes a manageable
-- number of arguments instead of 30+ positional ones)
-- ============================================================

data AddCols = AddCols
  { addPath_ :: !(V.Vector (Maybe ByteString))
  , addSize_ :: !(V.Vector (Maybe Int64))
  , addModTime_ :: !(V.Vector (Maybe Int64))
  , addDataChg_ :: !(V.Vector (Maybe Bool))
  , addStats_ :: !(V.Vector (Maybe ByteString))
  , addPartK_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , addPartV_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , addTagsK_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , addTagsV_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , addDv_ :: !DeletionVectorCols
  }


data RemCols = RemCols
  { remPath_ :: !(V.Vector (Maybe ByteString))
  , remDelTs_ :: !(V.Vector (Maybe Int64))
  , remDataChg_ :: !(V.Vector (Maybe Bool))
  , remSize_ :: !(V.Vector (Maybe Int64))
  , remPartK_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , remPartV_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , remExt_ :: !(V.Vector (Maybe Bool))
  , remDv_ :: !DeletionVectorCols
  }


data MetaCols = MetaCols
  { metaId_ :: !(V.Vector (Maybe ByteString))
  , metaName_ :: !(V.Vector (Maybe ByteString))
  , metaDesc_ :: !(V.Vector (Maybe ByteString))
  , metaSchema_ :: !(V.Vector (Maybe ByteString))
  , metaProv_ :: !(V.Vector (Maybe ByteString))
  , metaOptK_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , metaOptV_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , metaPart_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , metaConfK_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , metaConfV_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , metaCt_ :: !(V.Vector (Maybe Int64))
  }


data ProtCols = ProtCols
  { protMinR_ :: !(V.Vector (Maybe Int32))
  , protMinW_ :: !(V.Vector (Maybe Int32))
  , protRdrFt_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  , protWrtFt_ :: !(V.Vector (V.Vector (Maybe ByteString)))
  }


data OtherCols = OtherCols
  { txnApp_ :: !(V.Vector (Maybe ByteString))
  , txnVersion_ :: !(V.Vector (Maybe Int64))
  , txnLastUpdated_ :: !(V.Vector (Maybe Int64))
  , domDom_ :: !(V.Vector (Maybe ByteString))
  , sideP_ :: !(V.Vector (Maybe ByteString))
  }


{- | Look at row @i@ across the per-leaf vectors and emit
exactly one 'DeltaAction'. The first non-null variant wins;
the spec guarantees only one is set per row.
-}
rowAction
  :: Int
  -> AddCols
  -> RemCols
  -> MetaCols
  -> ProtCols
  -> OtherCols
  -> DeltaAction
rowAction i ac rc mc pc oc
  | Just path <- atIdx (addPath_ ac) i =
      ActionAdd
        AddAction
          { addPath = decodeText path
          , addSize = fromIntegralMaybe (atIdx (addSize_ ac) i)
          , addModificationTime = fromIntegralMaybe (atIdx (addModTime_ ac) i)
          , addDataChange = fromMaybe True (atIdx (addDataChg_ ac) i)
          , addStats = fmap decodeText (atIdx (addStats_ ac) i)
          , addPartitionValues = decodeOptStringMap (addPartK_ ac) (addPartV_ ac) i
          , addTags = decodeStringMap (addTagsK_ ac) (addTagsV_ ac) i
          , addDeletionVector = decodeDeletionVector (addDv_ ac) i
          }
  | Just path <- atIdx (remPath_ rc) i =
      ActionRemove
        RemoveAction
          { removePath = decodeText path
          , removeDeletionTimestamp = fmap fromIntegral (atIdx (remDelTs_ rc) i)
          , removeDataChange = fromMaybe True (atIdx (remDataChg_ rc) i)
          , removeExtendedFileMetadata = atIdx (remExt_ rc) i
          , removeSize = fmap fromIntegral (atIdx (remSize_ rc) i)
          , removePartitionValues = decodeOptStringMap (remPartK_ rc) (remPartV_ rc) i
          , removeDeletionVector = decodeDeletionVector (remDv_ rc) i
          }
  | Just metaIdBs <- atIdx (metaId_ mc) i =
      ActionMetaData
        MetaDataAction
          { mdId = decodeText metaIdBs
          , mdName = fmap decodeText (atIdx (metaName_ mc) i)
          , mdDescription = fmap decodeText (atIdx (metaDesc_ mc) i)
          , mdFormat = case atIdx (metaProv_ mc) i of
              Just provider ->
                Just
                  ( decodeText provider
                  , decodeStringMap (metaOptK_ mc) (metaOptV_ mc) i
                  )
              Nothing -> Nothing
          , mdSchemaString = maybe "" decodeText (atIdx (metaSchema_ mc) i)
          , mdPartitionColumns = decodeStringList (metaPart_ mc) i
          , mdConfiguration = decodeStringMap (metaConfK_ mc) (metaConfV_ mc) i
          , mdCreatedTime = fmap fromIntegral (atIdx (metaCt_ mc) i)
          }
  | Just minR <- atIdx (protMinR_ pc) i =
      ActionProtocol
        ProtocolAction
          { pMinReaderVersion = fromIntegral minR
          , pMinWriterVersion = maybe 0 fromIntegral (atIdx (protMinW_ pc) i)
          , pReaderFeatures = decodeStringList (protRdrFt_ pc) i
          , pWriterFeatures = decodeStringList (protWrtFt_ pc) i
          }
  | Just appId <- atIdx (txnApp_ oc) i =
      case atIdx (txnVersion_ oc) i of
        Just version -> ActionTxn (TxnAction (decodeText appId) version (atIdx (txnLastUpdated_ oc) i))
        Nothing -> ActionOther "txn"
  | Just _ <- atIdx (domDom_ oc) i = ActionOther "domainMetadata"
  | Just _ <- atIdx (sideP_ oc) i = ActionOther "sidecar"
  | otherwise = ActionOther "<empty-row>"


-- ============================================================
-- Map / list value-shape helpers
-- ============================================================

{- | Decode a row of @map<string, string>@ into 'Map.Map'.
Both keys and values are required-of-required (their @Maybe@
wrapper from the per-row inner vector should always be
'Just' if we see them at all). 'Nothing' keys silently drop
the entry; 'Nothing' values map to the empty string (Spark /
delta-rs never emit them, so this branch is defensive).
-}
decodeStringMap
  :: V.Vector (V.Vector (Maybe ByteString)) -- keys per row
  -> V.Vector (V.Vector (Maybe ByteString)) -- values per row
  -> Int
  -> Map.Map Text Text
decodeStringMap keysCol valsCol i =
  let !keys = fromMaybe V.empty (keysCol V.!? i)
      !vals = fromMaybe V.empty (valsCol V.!? i)
  in Map.fromList $ V.toList $ V.zipWith pair keys vals
  where
    pair (Just k) (Just v) = (decodeText k, decodeText v)
    pair (Just k) Nothing = (decodeText k, "")
    pair Nothing _ = ("", "") -- dropped via Map.fromList collisions


{- | Like 'decodeStringMap' but the value branch is itself a
nullable (the Delta @partitionValues@ schema lets values be
@null@ for partitions like @region=NULL@). The result map
carries 'Maybe Text' values.
-}
decodeOptStringMap
  :: V.Vector (V.Vector (Maybe ByteString))
  -> V.Vector (V.Vector (Maybe ByteString))
  -> Int
  -> Map.Map Text (Maybe Text)
decodeOptStringMap keysCol valsCol i =
  let !keys = fromMaybe V.empty (keysCol V.!? i)
      !vals = fromMaybe V.empty (valsCol V.!? i)
  in Map.fromList $ V.toList $ V.zipWith pair keys vals
  where
    pair (Just k) (Just v) = (decodeText k, Just (decodeText v))
    pair (Just k) Nothing = (decodeText k, Nothing)
    pair Nothing _ = ("", Nothing)


decodeStringList
  :: V.Vector (V.Vector (Maybe ByteString))
  -> Int
  -> [Text]
decodeStringList col i =
  let !row = fromMaybe V.empty (col V.!? i)
  in [decodeText t | Just t <- V.toList row]


data DeletionVectorCols = DeletionVectorCols
  { dvStorageType :: !(V.Vector (Maybe ByteString))
  , dvPathOrInline :: !(V.Vector (Maybe ByteString))
  , dvOffset :: !(V.Vector (Maybe Int32))
  , dvSize :: !(V.Vector (Maybe Int32))
  , dvCardinality :: !(V.Vector (Maybe Int64))
  }


readDeletionVectorCols :: PR.ParquetFile -> P.RowGroup -> Text -> Either String DeletionVectorCols
readDeletionVectorCols pf rg action =
  DeletionVectorCols
    <$> readByteArrayOpt pf rg (path "storageType")
    <*> readByteArrayOpt pf rg (path "pathOrInlineDv")
    <*> readInt32Opt pf rg (path "offset")
    <*> readInt32Opt pf rg (path "sizeInBytes")
    <*> readInt64Opt pf rg (path "cardinality")
  where
    path leaf = [action, "deletionVector", leaf]


-- Reassemble the same descriptor object as the JSON log reader.
decodeDeletionVector :: DeletionVectorCols -> Int -> Maybe Aeson.Value
decodeDeletionVector cols i
  | Nothing <- storageType
  , Nothing <- pathOrInline
  , Nothing <- offset
  , Nothing <- size
  , Nothing <- cardinality =
      Nothing
  | otherwise =
      Just . Aeson.Object . KM.fromList . catMaybes $
        [ field "storageType" (Aeson.String . decodeText) storageType
        , field "pathOrInlineDv" (Aeson.String . decodeText) pathOrInline
        , field "offset" (Aeson.Number . fromIntegral) offset
        , field "sizeInBytes" (Aeson.Number . fromIntegral) size
        , field "cardinality" (Aeson.Number . fromIntegral) cardinality
        ]
  where
    storageType = atIdx (dvStorageType cols) i
    pathOrInline = atIdx (dvPathOrInline cols) i
    offset = atIdx (dvOffset cols) i
    size = atIdx (dvSize cols) i
    cardinality = atIdx (dvCardinality cols) i
    field key wrap value = (,) (Aeson.fromString key) . wrap <$> value


atIdx :: V.Vector (Maybe a) -> Int -> Maybe a
atIdx v i = case v V.!? i of
  Just (Just x) -> Just x
  _ -> Nothing


decodeText :: ByteString -> Text
decodeText = TE.decodeUtf8


{- | Project @Maybe Int64@ values to a non-negative @Word64@,
defaulting to 0 when absent. Negative @Int64@s collapse to
0 too (Delta size / modificationTime fields can't legally
be negative).
-}
fromIntegralMaybe :: Maybe Int64 -> Word64
fromIntegralMaybe = maybe 0 (\n -> if n < 0 then 0 else fromIntegral n)


-- ============================================================
-- Per-leaf readers (path-based, encoding-agnostic)
-- ============================================================

{- | Look up the column chunk in @rg@ whose @cmPathInSchema@
matches @path@. The Delta checkpoint's schema has duplicate
leaf names ('add.path', 'remove.path'), so we /must/ match
on the full path rather than just the leaf-name.
-}
findChunk
  :: P.RowGroup
  -> [Text]
  -> Either String P.ColumnChunk
findChunk rg path =
  let target = V.fromList path
  in case V.find
       ( \cc -> case P.ccMetadata cc of
           Just cm -> P.cmPathInSchema cm == target
           Nothing -> False
       )
       (P.rgColumns rg) of
       Just cc -> Right cc
       Nothing ->
         Left
           ( "Delta.Checkpoint: column not found: "
               ++ show path
           )


{- | Compute (maxRep, maxDef) for a leaf path, then read it as
@V.Vector (Maybe a)@ by dispatching on the physical type.
Missing leaves (e.g. an older Delta protocol without the
@sidecar@ column) come back as the zero-length all-Nothing
vector so per-row presence checks stay sound.
-}
readByteArrayOpt
  :: PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String (V.Vector (Maybe ByteString))
readByteArrayOpt = readLeaf PR.readGenericByteArrayOptionalColumnChunk


readInt64Opt
  :: PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String (V.Vector (Maybe Int64))
readInt64Opt = readLeaf PR.readGenericInt64OptionalColumnChunk


readInt32Opt
  :: PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String (V.Vector (Maybe Int32))
readInt32Opt = readLeaf PR.readGenericInt32OptionalColumnChunk


readBoolOpt
  :: PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String (V.Vector (Maybe Bool))
readBoolOpt = readLeaf PR.readGenericBoolOptionalColumnChunk


{- | Read a leaf at @max_rep > 0@ (a list / map element) into
per-row inner vectors of @Maybe ByteString@. If the leaf
isn't on disk (older Delta protocol) we substitute an
all-empty per-row vector so callers can still index by row.

This variant assumes the list element itself is Parquet-
/required/, which is the common case for typed Delta lists
(partitionColumns, readerFeatures, writerFeatures, map keys).
For map values that are Parquet-/optional/ — the Delta
@partitionValues@ value leaf, where the protocol allows
@null@ to mark a partition column whose value was null —
use 'readByteArrayRepOptElem' so element-null entries
surface as 'Nothing' rather than getting dropped.
-}
readByteArrayRep
  :: PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String (V.Vector (V.Vector (Maybe ByteString)))
readByteArrayRep =
  readLeaf PR.readGenericByteArrayRepeatedColumnChunk


{- | Like 'readByteArrayRep' but for leaves whose list element
is Parquet-optional (so @max_def@ has one extra step for the
element nullability). The decoder uses
@parent_empty_def = max_def - 2@, which preserves
element-null entries inside present lists.
-}
readByteArrayRepOptElem
  :: PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String (V.Vector (V.Vector (Maybe ByteString)))
readByteArrayRepOptElem pf rg path =
  let !nRows = fromIntegral (P.rgNumRows rg) :: Int
  in case findChunk rg path of
       Left _ -> Right (missingFallback nRows)
       Right cc -> do
         cm <- case P.ccMetadata cc of
           Just m -> Right m
           Nothing -> Left "Delta.Checkpoint: column chunk has no metadata"
         (maxRep, maxDef) <-
           PL.maxLevelsForColumnPath
             (P.fmSchema (PR.pfFooter pf))
             (V.fromList path)
         chunkSlice <- columnChunkBytes pf cm
         PR.readGenericByteArrayRepeatedColumnChunkWith
           (P.cmCodec cm)
           maxRep
           maxDef
           (maxDef - 2)
           chunkSlice


{- | Generic per-leaf reader: looks up the chunk by path,
decompresses + decodes via the supplied per-type reader,
and pads to the row-group row count. If the leaf isn't in
the schema (an older Delta protocol that doesn't write
e.g. @add.deletionVector.*@), we return the @missing@
placeholder of the right length so per-row presence checks
stay sound.
-}
readLeaf
  :: MissingFallback r
  => (P.Compression -> Int -> Int -> ByteString -> Either String r)
  -> PR.ParquetFile
  -> P.RowGroup
  -> [Text]
  -> Either String r
readLeaf reader pf rg path =
  let !nRows = fromIntegral (P.rgNumRows rg) :: Int
  in case findChunk rg path of
       Left _ -> Right (missingFallback nRows)
       Right cc -> do
         cm <- case P.ccMetadata cc of
           Just m -> Right m
           Nothing -> Left "Delta.Checkpoint: column chunk has no metadata"
         (maxRep, maxDef) <-
           PL.maxLevelsForColumnPath
             (P.fmSchema (PR.pfFooter pf))
             (V.fromList path)
         chunkSlice <- columnChunkBytes pf cm
         reader (P.cmCodec cm) maxRep maxDef chunkSlice


{- | Fill-in for a leaf that the on-disk schema doesn't carry.
Optional columns get an all-'Nothing' vector at the row-group
length; repeated columns get all-empty inner vectors at the
row-group length. Either way, per-row presence checks
threaded through 'rowAction' continue to work.
-}
class MissingFallback r where
  missingFallback :: Int -> r


instance MissingFallback (V.Vector (Maybe a)) where
  missingFallback n = V.replicate n Nothing


instance MissingFallback (V.Vector (V.Vector (Maybe a))) where
  missingFallback n = V.replicate n V.empty


{- | Slice the bytes of a column chunk out of a 'ParquetFile'.
Mirrors what 'PR.columnChunkSlice' does but works directly
from a 'ColumnMetadata' so we don't have to thread the
(rgIdx, colIdx) pair through the per-leaf path.
-}
columnChunkBytes
  :: PR.ParquetFile
  -> P.ColumnMetadata
  -> Either String ByteString
columnChunkBytes pf cm =
  let !start = case P.cmDictionaryPageOffset cm of
        Just dpOff | dpOff > 0 -> fromIntegral dpOff :: Int
        _ -> fromIntegral (P.cmDataPageOffset cm) :: Int
      !len = fromIntegral (P.cmTotalCompressedSize cm) :: Int
      !bs = PR.pfBytes pf
  in if start < 0 || len < 0 || start + len > BS.length bs
       then Left "Delta.Checkpoint: column chunk out of range"
       else Right (BS.take len (BS.drop start bs))
