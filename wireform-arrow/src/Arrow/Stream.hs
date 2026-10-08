{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | High-level, pyarrow-shaped Arrow IPC API.

95% of callers should reach for this module. It hides the
record-batch + dictionary-batch plumbing of "Arrow.FlatBufferIPC"
behind a single-call shape that mirrors
@pyarrow.ipc.new_stream@ / @pyarrow.ipc.open_stream@:

@
-- Write
case 'encodeArrowStream' 'defaultWriteOptions' schema batches of
  Right bytes -> ...
  Left  err   -> ...   -- the batches do not fit the schema

-- Read
case 'decodeArrowStream' bytes of
  Right (schema, batches) -> ...
  Left  err               -> ...
@

@batches@ is a list of @V.'V.Vector' 'ColumnArray'@, one column
per schema field, repeated once per record batch.

'ColDictionary' columns are handled automatically, at any depth and
inside dictionary values: the writer collects the dictionaries from
the input columns and emits a 'DictBatch' per id ahead of the first
record batch that references it (a dictionary nested inside another
dictionary's values ahead of the outer one); the reader resolves the
placeholder values column in returned 'ColDictionary' nodes against
the dictionary batches it saw earlier in the stream.

The writers check every batch against the schema first
('Arrow.Write.Columns.validateColumns') and report a mismatch, or a
dictionary they cannot write, as 'Left'.

For the file format ('encodeArrowFile' / 'decodeArrowFile') the
semantics are identical: same input shape, same output shape,
same dictionary handling, just an additional @ARROW1@ wrapper +
'Footer' index appended.

For lower-level control (custom dict ids, manual variadic
buffer counts, raw 'RecordBatchDef' construction, delta
dictionaries) drop down to "Arrow.FlatBufferIPC".
-}
module Arrow.Stream (
  -- * Encoding / decoding

  -- ** Streams (eager)
  encodeArrowStream,
  decodeArrowStream,

  -- ** Files (eager)
  encodeArrowFile,
  decodeArrowFile,

  -- ** Streams (incremental / iterator)
  StreamReader,
  openStreamReader,
  streamReaderSchema,
  streamReaderNext,
  streamReaderToList,
  streamReaderIter,
  streamReaderProjected,
  streamReaderProjectedIter,

  -- * Options
  WriteOptions (..),
  DictHandling (..),
  defaultWriteOptions,
  BodyCompressionCodec (..),
  bodyCompressionAvailable,
) where

import Arrow.Column (
  ColumnArray (..),
  columnLength,
  concatColumnArray,
  concatColumnArrays,
  isNullableColumn,
  resolveDictionaryColumn,
  sliceColumnArray,
 )
import Arrow.FlatBufferIPC (
  DictBatch (..),
  bodyCompressionAvailable,
  buildRecordBatchBytesWith,
  decompressBody,
  materializeRecordBatchFB,
  readArrowFileFBWithDicts,
  writeArrowFileFBWithDicts,
  writeArrowStreamFBWithDicts,
 )
import Arrow.FlatBufferIPC qualified as FB
import Arrow.Types (
  ArrowType (..),
  BodyCompressionCodec (..),
  Buffer (..),
  DictionaryEncoding (..),
  Field (..),
  FieldNode (..),
  RecordBatchDef (..),
  Schema (..),
 )
import Arrow.Write.Columns (validateColumns)
import Columnar.Stream qualified as IS
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (foldlM)
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (mapAccumM)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP


-- ============================================================
-- Write options
-- ============================================================

{- | Arrow IPC writer configuration. Construct one with
'defaultWriteOptions' and override the fields you care about.
-}
data WriteOptions = WriteOptions
  { writeBodyCompression :: !(Maybe BodyCompressionCodec)
  {- ^ When 'Just', record and dictionary batch buffers are
  compressed per Arrow's 'BodyCompression' table. 'Nothing' (the
  default) leaves buffers uncompressed. A codec that was not
  compiled in (cabal flags @zstd@ / @lz4@; check with
  'bodyCompressionAvailable') is ignored and the batches are
  written uncompressed, so the output is always readable.
  -}
  , writeDictHandling :: !DictHandling
  {- ^ How the writer treats repeated dictionary ids across
  batches: emit a single dict up front, or emit a fresh
  @isDelta=false@ replacement before any batch whose
  dictionary differs. Default: 'DictEmitOnce'. PyArrow's
  default matches 'DictReplaceOnChange'; use that when
  encoding streams destined for pyarrow-with-varying-dicts.
  -}
  }
  deriving (Show, Eq)


-- | Dictionary-emission strategy for the high-level writer.
data DictHandling
  = {- | Emit one dictionary batch per id ahead of the first record
    batch. When batches carry different value columns for one id,
    the distinct value columns are concatenated into one dictionary
    and every batch's indices are shifted into it. Fails ('Left')
    when the combined dictionary has more values than the field's
    index type can address, or when the value columns cannot be
    concatenated. This is the default, and the only strategy the
    file format allows.
    -}
    DictEmitOnce
  | {- | Emit a fresh @isDelta=false@ replacement dict batch
    before any record batch whose dictionary values differ
    from the most-recently-emitted dict for that id (and before
    it, the dictionaries nested in its values that changed; an
    outer dictionary is re-sent whenever a dictionary nested in its
    values is). Matches the semantics pyarrow uses when a stream
    writer sees a changed dict.
    -}
    DictReplaceOnChange
  deriving (Show, Eq)


-- | Defaults: no body compression, emit-once dictionaries.
defaultWriteOptions :: WriteOptions
defaultWriteOptions =
  WriteOptions
    { writeBodyCompression = Nothing
    , writeDictHandling = DictEmitOnce
    }


-- ============================================================
-- Streams
-- ============================================================

{- | Encode a sequence of column-major batches as a self-contained
Arrow IPC /stream/. Equivalent to pyarrow's
@ipc.new_stream + write_batch + close@; see 'WriteOptions' for the
knobs and 'defaultWriteOptions' for pyarrow-like defaults.

Every batch is first checked against the schema
('Arrow.Write.Columns.validateColumns': column count, equal column
lengths, column shapes, struct and fixed-size list row counts);
a mismatch is a 'Left'.

Dictionary-encoded columns ('ColDictionary' / 'ColDictionaryMaybe'
anywhere in the column tree, including inside dictionary values) are
handled automatically, see 'DictHandling'. Dictionary batches use the
value field the schema declares for the id (type, children), and a
dictionary nested in another dictionary's values is written before
the outer one.
-}
encodeArrowStream
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> Either String ByteString
encodeArrowStream opts sch batches = do
  mapM_ (validateColumns (arrowFields sch)) batches
  case writeDictHandling opts of
    DictEmitOnce -> do
      (dicts, batchPairs) <- compileBatchesWith opts sch batches
      Right (writeArrowStreamFBWithDicts sch dicts batchPairs)
    DictReplaceOnChange ->
      encodeArrowStreamReplaceDicts opts sch batches


{- | Writer for the 'DictReplaceOnChange' strategy. Interleaves
dict batches with record batches: each record batch is
preceded by fresh @isDelta=false@ dict batches for any
dictionary id whose values have changed since the last emission,
plus every dictionary whose values contain one of those.
-}
encodeArrowStreamReplaceDicts
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> Either String ByteString
encodeArrowStreamReplaceDicts opts sch batches0 = go Map.empty batches0 [schemaMsg]
  where
    !mCodec = writeBodyCompression opts
    !plan = dictPlan sch
    !schemaMsg = FB.encapsulateMessage (FB.buildSchemaMessage sch) BS.empty
    !eos = BS.pack [0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0]
    go _ [] acc = Right (BS.concat (reverse (eos : acc)))
    go !lastDicts (cols0 : rest) acc = do
      (curDicts, rebased) <- unifyDictionaries plan [cols0]
      let !cols = case rebased of
            [c] -> c
            _ -> cols0
          !(rb, body) = buildRecordBatchBytesWith mCodec sch cols
          !changed = Map.keysSet (Map.filterWithKey (\did vs -> Map.lookup did lastDicts /= Just vs) curDicts)
          !emitDicts = Map.restrictKeys curDicts (withDependents plan changed)
          !newLast = Map.union curDicts lastDicts
          !dictBytes =
            BS.concat
              ( map
                  (\db -> FB.encapsulateMessage (FB.buildDictionaryBatchMessage db) (dbBody db))
                  (buildDictBatches mCodec sch plan emitDicts)
              )
          !rbBytes =
            FB.encapsulateMessage
              (FB.buildRecordBatchMessage rb (fromIntegral (BS.length body)))
              body
      go newLast rest (rbBytes : dictBytes : acc)


{- | Decode an Arrow IPC stream back into 'Schema' + record-batch
column vectors. Dictionary references are resolved
transparently: the returned 'ColumnArray' values contain the
actual dictionary values, not just indices.
-}
decodeArrowStream
  :: ByteString
  -> Either String (Schema, [V.Vector ColumnArray])
decodeArrowStream bs = do
  (sch, frames) <- FB.readArrowStreamFBInterleaved bs
  decodeInterleavedFrames sch frames


-- ============================================================
-- Files
-- ============================================================

{- | Encode batches as an Arrow IPC /file/ (@ARROW1@ header, the
stream payload, a FlatBuffers 'Footer' indexing every dictionary and
record batch, @ARROW1@ trailer). Batches are checked against the
schema as in 'encodeArrowStream'. Dictionaries are always unified
into one dictionary batch per id ('DictEmitOnce', whatever
'writeDictHandling' says), because the file format does not allow
replacement dictionaries.
-}
encodeArrowFile
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> Either String ByteString
encodeArrowFile opts sch batches = do
  mapM_ (validateColumns (arrowFields sch)) batches
  (dicts, batchPairs) <- compileBatchesWith opts sch batches
  Right (writeArrowFileFBWithDicts sch dicts batchPairs)


{- | Decode an Arrow IPC file. Dictionary-resolved 'ColumnArray'
values are returned just like 'decodeArrowStream'.
-}
decodeArrowFile
  :: ByteString
  -> Either String (Schema, [V.Vector ColumnArray])
decodeArrowFile bs = do
  (sch, dicts, frames) <- readArrowFileFBWithDicts bs
  decodeBatches sch dicts frames


-- ============================================================
-- Internal: compile + decode shared between stream / file paths.
-- ============================================================

{- | Unify the dictionaries of every batch (see 'unifyDictionaries'),
then build one 'DictBatch' per dictionary id (nested dictionaries
first, see 'dictPlan') and the index-only @(rb, body)@ pair of each
batch.
-}
compileBatchesWith
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> Either String ([DictBatch], [(RecordBatchDef, ByteString)])
compileBatchesWith opts sch batches = do
  let !mCodec = writeBodyCompression opts
      !plan = dictPlan sch
  (dictMap, rebased) <- unifyDictionaries plan batches
  let !dicts = buildDictBatches mCodec sch plan dictMap
      !pairs = map (buildRecordBatchBytesWith mCodec sch) rebased
  Right (dicts, pairs)


{- | What the writer needs to know about the schema's dictionaries:
the order to emit them in, the largest index each id's index type
addresses, and the ids nested inside each id's value type.
-}
data DictPlan = DictPlan
  { dpOrder :: ![Int64]
  -- ^ Every dictionary id, a dictionary nested inside another's values before the outer one.
  , dpMaxIndex :: !(Map.Map Int64 Int64)
  -- ^ Largest index the id's index type can hold (the smallest over fields sharing the id).
  , dpNested :: !(Map.Map Int64 (Set.Set Int64))
  -- ^ Ids declared anywhere inside the id's value type.
  }


-- | Walk the schema in post-order (children before parents) collecting the 'DictPlan'.
dictPlan :: Schema -> DictPlan
dictPlan sch =
  let ((order, maxIx, nested), _) = goFields ([], Map.empty, Map.empty) (arrowFields sch)
  in DictPlan {dpOrder = reverse order, dpMaxIndex = maxIx, dpNested = nested}
  where
    goFields st fs = V.foldl' (\(s, ids) f -> let (s', ids') = goField s f in (s', Set.union ids ids')) (st, Set.empty) fs
    goField st f =
      let ((order, maxIx, nested), inner) = goFields st (fieldChildren f)
      in case fieldDictionary f of
           Nothing -> ((order, maxIx, nested), inner)
           Just de ->
             let !did = deId de
             in ( ( if did `elem` order then order else did : order
                  , Map.insertWith min did (maxIndexFor (deIndexType de)) maxIx
                  , Map.insertWith Set.union did inner nested
                  )
                , Set.insert did inner
                )


-- | Largest dictionary index an index type holds (indices are stored as 'Int32').
maxIndexFor :: ArrowType -> Int64
maxIndexFor = \case
  AInt 8 True -> 127
  AInt 8 False -> 255
  AInt 16 True -> 32767
  AInt 16 False -> 65535
  _ -> fromIntegral (maxBound :: Int32)


-- | The ids together with every dictionary whose values contain one of them.
withDependents :: DictPlan -> Set.Set Int64 -> Set.Set Int64
withDependents plan ids =
  Set.union ids (Map.keysSet (Map.filter (not . Set.disjoint ids) (dpNested plan)))


{- | One dictionary batch per id, in 'dpOrder'. The value field is the
dictionary field itself minus its encoding (same type and children);
its nullability follows the values column.
-}
buildDictBatches :: Maybe BodyCompressionCodec -> Schema -> DictPlan -> Map.Map Int64 ColumnArray -> [DictBatch]
buildDictBatches mCodec sch plan dictMap = mapMaybe one (dpOrder plan)
  where
    one did = do
      values <- Map.lookup did dictMap
      f <- findDictField did (arrowFields sch)
      let !valuesField = f {fieldDictionary = Nothing, fieldNullable = isNullableColumn values}
          !innerSchema = sch {arrowFields = V.singleton valuesField, arrowMetadata = V.empty, arrowFeatures = V.empty}
          !(rb, body) = buildRecordBatchBytesWith mCodec innerSchema (V.singleton values)
      Just DictBatch {dbId = did, dbIsDelta = False, dbData = rb, dbBody = body}


-- | Distinct value columns seen for one id (newest first, with their offsets) and their total length.
data DictAcc = DictAcc ![(ColumnArray, Int32)] !Int64


{- | Collect the dictionaries of a run of batches into one values
column per id. Walking the batches in order, each dictionary column's
values are appended to its id's combined dictionary unless an equal
values column was already appended, and the column's indices are
shifted by the position of its values inside the combined dictionary,
so every batch can be written against a single dictionary batch.

Dictionary columns inside dictionary values are collected the same
way, before their outer value column is placed, so the outer
dictionary is written with indices into the combined inner one. The
rebased columns carry an empty values column of the right type (the
writer only needs the indices), which keeps the concatenation of
outer value columns from shifting already rebased inner indices.

Fails when the value columns of one id cannot be concatenated, or
when combining them produces more values than the id's index type can
address.
-}
unifyDictionaries :: DictPlan -> [V.Vector ColumnArray] -> Either String (Map.Map Int64 ColumnArray, [V.Vector ColumnArray])
unifyDictionaries plan batches = do
  (seen, rebased) <- mapAccumM (mapAccumM (mapAccumDictionaries shiftOne)) Map.empty batches
  dicts <- Map.traverseWithKey combine seen
  Right (dicts, rebased)
  where
    shiftOne st col = case col of
      ColDictionary did ix vals -> do
        (st1, vals') <- mapAccumDictionaries shiftOne st vals
        (st2, off) <- place st1 did vals'
        Right (st2, ColDictionary did (if off == 0 then ix else VP.map (+ off) ix) (sliceColumnArray 0 0 vals'))
      ColDictionaryMaybe did ix vals -> do
        (st1, vals') <- mapAccumDictionaries shiftOne st vals
        (st2, off) <- place st1 did vals'
        Right (st2, ColDictionaryMaybe did (if off == 0 then ix else V.map (fmap (+ off)) ix) (sliceColumnArray 0 0 vals'))
      _ -> Right (st, col)
    place st did vals =
      let !len = fromIntegral (columnLength vals) :: Int64
      in case Map.lookup did st of
           Nothing -> Right (Map.insert did (DictAcc [(vals, 0)] len) st, 0)
           Just (DictAcc entries total) -> case lookup vals entries of
             Just off -> Right (st, off)
             Nothing
               | total > fromIntegral (maxBound :: Int32) ->
                   Left ("Arrow.Stream: dictionary id " ++ show did ++ ": combined dictionary exceeds Int32 indices")
               | otherwise ->
                   Right (Map.insert did (DictAcc ((vals, fromIntegral total) : entries) (total + len)) st, fromIntegral total)
    combine did (DictAcc entries total) = case reverse entries of
      [(v, _)] -> Right v
      ordered
        | total - 1 > maxIx ->
            Left
              ( "Arrow.Stream: dictionary id "
                  ++ show did
                  ++ ": the batches' dictionaries combine to "
                  ++ show total
                  ++ " values but the field's index type addresses at most "
                  ++ show (maxIx + 1)
                  ++ " (use DictReplaceOnChange or a wider index type)"
              )
        | otherwise -> case concatColumnArrays (map fst ordered) of
            Right v -> Right v
            Left e -> Left ("Arrow.Stream: dictionary id " ++ show did ++ ": value columns cannot be combined: " ++ e)
      where
        !maxIx = Map.findWithDefault (fromIntegral (maxBound :: Int32)) did (dpMaxIndex plan)


{- | Thread a state through every dictionary column of a column tree
(pre-order, children left to right), rebuilding the tree. Dictionary
values are not entered; the callback decides what to do with them.
-}
mapAccumDictionaries
  :: (s -> ColumnArray -> Either String (s, ColumnArray))
  -> s
  -> ColumnArray
  -> Either String (s, ColumnArray)
mapAccumDictionaries f = go
  where
    one st c k = (\(st', c') -> (st', k c')) <$> go st c
    go st col = case col of
      ColDictionary {} -> f st col
      ColDictionaryMaybe {} -> f st col
      ColStruct n cs -> (\(st', kids) -> (st', ColStruct n kids)) <$> mapAccumM named st cs
      ColStructMaybe v cs -> (\(st', kids) -> (st', ColStructMaybe v kids)) <$> mapAccumM named st cs
      ColList o c -> one st c (ColList o)
      ColListMaybe v o c -> one st c (ColListMaybe v o)
      ColLargeList o c -> one st c (ColLargeList o)
      ColLargeListMaybe v o c -> one st c (ColLargeListMaybe v o)
      ColFixedSizeList w n c -> one st c (ColFixedSizeList w n)
      ColFixedSizeListMaybe w v c -> one st c (ColFixedSizeListMaybe w v)
      ColMap o k v -> do
        (st1, k') <- go st k
        (st2, v') <- go st1 v
        Right (st2, ColMap o k' v')
      ColMapMaybe vs o k v -> do
        (st1, k') <- go st k
        (st2, v') <- go st1 v
        Right (st2, ColMapMaybe vs o k' v')
      ColDenseUnion ts o cs -> (\(st', kids) -> (st', ColDenseUnion ts o kids)) <$> mapAccumM go st cs
      ColSparseUnion ts cs -> (\(st', kids) -> (st', ColSparseUnion ts kids)) <$> mapAccumM go st cs
      ColRunEndEncoded re vs -> one st vs (ColRunEndEncoded re)
      ColListView o s c -> one st c (ColListView o s)
      ColListViewMaybe v o s c -> one st c (ColListViewMaybe v o s)
      ColLargeListView o s c -> one st c (ColLargeListView o s)
      ColLargeListViewMaybe v o s c -> one st c (ColLargeListViewMaybe v o s)
      _ -> Right (st, col)
    named st (nm, c) = (\(st', c') -> (st', (nm, c'))) <$> go st c


{- | Decode the file's dictionary batches (in file order, honouring
delta batches) and its record batches, resolving every dictionary
column against the final dictionaries.
-}
decodeBatches
  :: Schema
  -> [DictBatch]
  -> [(RecordBatchDef, ByteString)]
  -> Either String (Schema, [V.Vector ColumnArray])
decodeBatches sch dicts frames = do
  dictMap <- foldlM (applyDictBatch sch) Map.empty dicts
  resolved <- traverse (decodeOneBatch dictMap) frames
  Right (sch, resolved)
  where
    decodeOneBatch m (rb, body) = do
      (rb', body') <- maybeDecompressBatch rb body
      cols <- materializeRecordBatchFB sch rb' body'
      V.mapM (resolveDictionaryColumn (`Map.lookup` m)) cols


{- | Fold one dictionary batch into the id-to-values map: a delta
batch appends to the existing dictionary ('concatColumnArray'; a
delta whose values cannot be appended is an error), any other batch
replaces it.

Dictionary columns inside the values (a dictionary whose value type
contains dictionary-encoded fields) are resolved against the
dictionaries in force when the batch arrives, as Arrow C++ does: a
later replacement or delta of the inner dictionary affects the outer
dictionaries sent after it, not the ones already received.
-}
applyDictBatch :: Schema -> Map.Map Int64 ColumnArray -> DictBatch -> Either String (Map.Map Int64 ColumnArray)
applyDictBatch sch dictMap db = do
  (did, raw) <- decodeDictBatch sch db
  vals <- resolveDictionaryColumn (`Map.lookup` dictMap) raw
  if dbIsDelta db
    then case Map.lookup did dictMap of
      Nothing -> Right (Map.insert did vals dictMap)
      Just old -> case concatColumnArray old vals of
        Right new -> Right (Map.insert did new dictMap)
        Left e -> Left ("Arrow.Stream: delta dictionary batch for id " ++ show did ++ ": " ++ e)
    else Right (Map.insert did vals dictMap)


{- | Decode a stream-order frame list (dict + record batches
interleaved) while honouring replacement / delta dictionaries:
each record batch resolves against the dictionaries in force at its
position in the stream.
-}
decodeInterleavedFrames
  :: Schema
  -> [FB.StreamFrame]
  -> Either String (Schema, [V.Vector ColumnArray])
decodeInterleavedFrames sch = go Map.empty []
  where
    go _ acc [] = Right (sch, reverse acc)
    go !dictMap acc (FB.SFDict db : rest) = do
      newMap <- applyDictBatch sch dictMap db
      go newMap acc rest
    go !dictMap acc (FB.SFBatch rb body : rest) = do
      (rb', body') <- maybeDecompressBatch rb body
      cols <- materializeRecordBatchFB sch rb' body'
      resolved <- V.mapM (resolveDictionaryColumn (`Map.lookup` dictMap)) cols
      go dictMap (resolved : acc) rest


{- | If the record batch advertises body compression, run the
per-buffer decompressor and rewrite the buffer offsets to
point at the uncompressed layout (suitable for
'materializeRecordBatchFB'). Applies to dictionary batches too.
-}
maybeDecompressBatch
  :: RecordBatchDef -> ByteString -> Either String (RecordBatchDef, ByteString)
maybeDecompressBatch rb body = case rbBodyCompression rb of
  Nothing -> Right (rb, body)
  Just codec -> do
    (newBufs, newBody) <- decompressBody codec (rbBuffers rb) body
    Right
      ( rb
          { rbBuffers = newBufs
          , rbBodyCompression = Nothing
          }
      , newBody
      )


{- | Materialise the values column inside a 'DictBatch'. The values
field is the schema's dictionary field for this id without its
dictionary encoding (same value type and children). Dictionary values
carry their own nullability, independent of the indices: the values
column is read as nullable exactly when the batch ships a non-empty
validity bitmap (or a non-zero null count) for it.
-}
decodeDictBatch
  :: Schema -> DictBatch -> Either String (Int64, ColumnArray)
decodeDictBatch sch db = do
  f <- case findDictField (dbId db) (arrowFields sch) of
    Just f -> Right f
    Nothing ->
      Left $
        "Arrow.Stream: dictionary batch with id "
          ++ show (dbId db)
          ++ " doesn't match any field in the schema"
  (dictRb, dictBody) <- maybeDecompressBatch (dbData db) (dbBody db)
  let hasValiditySlot = case fieldType f of
        ANull -> False
        AUnion _ _ -> False
        ARunEndEncoded -> False
        _ -> True
      shipsValidity =
        maybe False ((> 0) . bufLength) (rbBuffers dictRb V.!? 0)
          || maybe False ((> 0) . fnNullCount) (rbNodes dictRb V.!? 0)
      !valuesField =
        f
          { fieldDictionary = Nothing
          , fieldNullable = hasValiditySlot && shipsValidity
          }
      !innerSchema = sch {arrowFields = V.singleton valuesField, arrowMetadata = V.empty, arrowFeatures = V.empty}
  cols <- materializeRecordBatchFB innerSchema dictRb dictBody
  case V.toList cols of
    [vals] -> Right (dbId db, vals)
    _ -> Left "Arrow.Stream: dictionary batch must hold exactly one column"


{- | Locate the 'Field' whose 'fieldDictionary' carries the given
id, walking nested children depth-first.
-}
findDictField :: Int64 -> V.Vector Field -> Maybe Field
findDictField did = goVec
  where
    goVec fs = goList (V.toList fs)
    goList [] = Nothing
    goList (f : fs) = case fieldDictionary f of
      Just de
        | deId de == did ->
            Just f
      _ -> case goVec (fieldChildren f) of
        Just g -> Just g
        Nothing -> goList fs



-- ============================================================
-- Streaming reader
-- ============================================================

{- | An iterator-style handle for reading an Arrow IPC stream
batch-by-batch, mirroring pyarrow's @ipc.RecordBatchStreamReader@:

@
case 'openStreamReader' bytes of
  Left  e  -> handleError e
  Right rd -> do
    let !sch = 'streamReaderSchema' rd
    loop rd
  where
    loop rd = case 'streamReaderNext' rd of
      Right (Just (cols, rd')) -> consume cols >> loop rd'
      Right Nothing            -> finish ()
      Left  e                  -> handleError e
@

All dictionary batches present in the stream are consumed and
materialised at 'openStreamReader' time, so subsequent
'streamReaderNext' calls only allocate per-batch column data.
Use 'streamReaderToList' to drain the iterator into a list
(equivalent to 'decodeArrowStream' but keeps the streaming
shape for callers that want incremental processing later).
-}
data StreamReader = StreamReader
  { srSchema :: !Schema
  , srDictMap :: !(Map.Map Int64 ColumnArray)
  -- ^ Dictionaries in force at the current stream position.
  , srFrames :: ![FB.StreamFrame]
  }


{- | Initialise an iterator from raw stream bytes. Parses the schema
and splits the stream into frames; dictionary batches are applied
(replacements and deltas) in stream order as 'streamReaderNext'
walks past them, so each record batch sees the dictionaries in force
at its position.
-}
openStreamReader :: ByteString -> Either String StreamReader
openStreamReader bs = do
  (sch, frames) <- FB.readArrowStreamFBInterleaved bs
  Right
    StreamReader
      { srSchema = sch
      , srDictMap = Map.empty
      , srFrames = frames
      }


-- | The schema decoded at 'openStreamReader' time.
streamReaderSchema :: StreamReader -> Schema
streamReaderSchema = srSchema


{- | Pull the next record batch from the iterator. Returns:

  * @Right (Just (cols, rd'))@: a materialised batch with
    dictionary references resolved, plus a continuation
    reader for the remaining frames.
  * @Right Nothing@: the stream's EOS marker has been
    consumed; no more batches.
  * @Left e@: a parse / materialisation error.
-}
streamReaderNext
  :: StreamReader
  -> Either String (Maybe (V.Vector ColumnArray, StreamReader))
streamReaderNext rd = case srFrames rd of
  [] -> Right Nothing
  (FB.SFDict db : rest) -> do
    dictMap <- applyDictBatch (srSchema rd) (srDictMap rd) db
    streamReaderNext rd {srDictMap = dictMap, srFrames = rest}
  (FB.SFBatch rb body : rest) -> do
    (rb', body') <- maybeDecompressBatch rb body
    cols <- materializeRecordBatchFB (srSchema rd) rb' body'
    resolved <- V.mapM (resolveDictionaryColumn (`Map.lookup` srDictMap rd)) cols
    Right (Just (resolved, rd {srFrames = rest}))


{- | Drain a 'StreamReader' into a list of batches. Equivalent
(modulo the schema bundling) to 'decodeArrowStream' but keeps
the streaming shape for callers that prefer to iterate.
-}
streamReaderToList
  :: StreamReader
  -> Either String [V.Vector ColumnArray]
streamReaderToList rd0 = go rd0 []
  where
    go rd acc = case streamReaderNext rd of
      Left e -> Left e
      Right Nothing -> Right (reverse acc)
      Right (Just (cols, rd')) -> go rd' (cols : acc)


{- | Lift a 'StreamReader' into the cross-format 'IS.Iter' shape.
Each 'IS.iterStep' decodes one record batch, applies body
decompression if needed, materialises columns, and resolves
dictionary references — matching 'streamReaderNext' semantics
without forcing the caller to thread the continuation reader
by hand.
-}
streamReaderIter :: StreamReader -> IS.Iter (V.Vector ColumnArray)
streamReaderIter rd0 = IS.iterUnfold rd0 $ \r ->
  case streamReaderNext r of
    Left e -> Left e
    Right Nothing -> Right Nothing
    Right (Just (cols, r')) -> Right (Just (cols, r'))


-- ============================================================
-- Column projection
-- ============================================================

{- | Like 'streamReaderToList' but only materialises the named
columns. Useful when the schema has dozens of columns and the
caller only needs a handful.

The returned 'Schema' is the projected schema (preserving the
order the caller asked for) and each batch is a vector of
'ColumnArray' values in the same order. Names not present in
the source schema produce a 'Left'.
-}
streamReaderProjected
  :: [Text]
  -> StreamReader
  -> Either String (Schema, [V.Vector ColumnArray])
streamReaderProjected names rd0 = do
  let !sourceSchema = srSchema rd0
  idxs <- resolveProjectionIndices names sourceSchema
  let !projSchema = projectSchema idxs sourceSchema
  IS.iterToList (streamReaderProjectedIter names rd0)
    >>= \batches -> Right (projSchema, batches)


{- | Iterator-shaped variant of 'streamReaderProjected'. Each
'IS.iterStep' yields a vector of column arrays containing
/only/ the requested columns, in the requested order.

The full record batch must still be materialised at decode
time (Arrow IPC's body layout doesn't support per-column
decode without parsing the record batch metadata), but the
returned vector aliases only the columns the caller asked for
— making it a real win for downstream code that drops the
unused columns immediately.
-}
streamReaderProjectedIter
  :: [Text]
  -> StreamReader
  -> IS.Iter (V.Vector ColumnArray)
streamReaderProjectedIter names rd0 =
  case resolveProjectionIndices names (srSchema rd0) of
    Left e -> IS.iterUnfold () (\_ -> Left e)
    Right idxs -> IS.iterMap (projectColumns idxs) (streamReaderIter rd0)


resolveProjectionIndices :: [Text] -> Schema -> Either String (V.Vector Int)
resolveProjectionIndices names sch = do
  let !nameToIdx =
        Map.fromList
          [ (fieldName f, i)
          | (i, f) <- V.toList (V.indexed (arrowFields sch))
          ]
  V.fromList <$> traverse (lookupOne nameToIdx) names
  where
    lookupOne m nm = case Map.lookup nm m of
      Just i -> Right i
      Nothing ->
        Left $
          "Arrow.Stream: projected column not present in source schema: "
            ++ show nm


projectSchema :: V.Vector Int -> Schema -> Schema
projectSchema idxs sch =
  sch
    { arrowFields = V.map (V.unsafeIndex (arrowFields sch)) idxs
    }


projectColumns :: V.Vector Int -> V.Vector ColumnArray -> V.Vector ColumnArray
projectColumns idxs cols = V.map (V.unsafeIndex cols) idxs
