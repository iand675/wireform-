{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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

The eager encoders lay the whole output out in one allocation and copy
every body byte once. The lazy encoders ('encodeArrowStreamLazy',
'encodeArrowFileLazy') return a lazy 'BL.ByteString' whose chunks are
the message headers and the column buffers themselves (no body copy),
for 'BL.hPut' and socket writers. Decoded columns alias the input
bytes (see 'Arrow.Column.copyColumn' to detach them).

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

  -- ** Streams (lazy, zero-copy bodies)
  encodeArrowStreamLazy,

  -- ** Files (eager)
  encodeArrowFile,
  decodeArrowFile,

  -- ** Files (lazy, zero-copy bodies)
  encodeArrowFileLazy,

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
  ColumnArray,
  columnLength,
  concatColumnArray,
  concatColumnArrays,
  resolveDictionaryColumn,
  sliceColumnArray,
 )
import Arrow.Column.Internal (IntegralPrim (..), PrimType (..), SomePrimType (..), integralPrim, primTypeFor)
import Arrow.Column.Internal qualified as I
import Arrow.FlatBufferIPC (DictBatch (..), StreamFrame (..), readArrowFileFBWithDicts, readArrowStreamFBInterleaved)
import Arrow.FlatBufferIPC.Write (
  Frame,
  bodyCompressionAvailable,
  batchFrame,
  dictionaryFrame,
  encodeBatch,
  renderFile,
  renderFileLazy,
  renderStream,
  renderStreamLazy,
  schemaFrame,
 )
import Arrow.Read.Columns (decodeDictionaryBatch, decodeRecordBatch)
import Arrow.Types (
  ArrowType (..),
  BodyCompressionCodec (..),
  DictionaryEncoding (..),
  Field (..),
  RecordBatchDef,
  Schema (..),
 )
import Arrow.Write.Columns (validateColumns)
import Columnar.Stream qualified as IS
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldlM)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (mapAccumM)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS


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
    and every batch's keys are shifted into it. Fails ('Left')
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
lengths, column shapes, nulls only under nullable fields, nested
lengths); a mismatch is a 'Left'.

Dictionary-encoded columns ('ColDictionary' anywhere in the column
tree, including inside dictionary values) are handled automatically,
see 'DictHandling'. Dictionary batches use the value field the schema
declares for the id (type, children), and a dictionary nested in
another dictionary's values is written before the outer one. Keys are
written at the width of the field's index type.

The output is one allocation holding every body byte exactly once.
-}
encodeArrowStream
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> Either String ByteString
encodeArrowStream opts sch batches = renderStream <$> streamFrames opts sch batches


{- | 'encodeArrowStream' as a lazy 'BL.ByteString'. Its chunks are the
message headers, the column buffers themselves (aliased, not copied)
and shared padding, so the work is proportional to the number of
buffers, not the data size. Same checks and dictionary handling.
-}
encodeArrowStreamLazy
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> Either String BL.ByteString
encodeArrowStreamLazy opts sch batches = renderStreamLazy <$> streamFrames opts sch batches


-- | Every frame of the stream except the end-of-stream marker.
streamFrames :: WriteOptions -> Schema -> [V.Vector ColumnArray] -> Either String [Frame]
streamFrames opts sch batches = do
  mapM_ (validateColumns (arrowFields sch)) batches
  case writeDictHandling opts of
    DictEmitOnce -> do
      Compiled dicts rbs <- compileBatches opts sch batches
      Right (schemaFrame sch : dicts ++ rbs)
    DictReplaceOnChange -> (schemaFrame sch :) <$> replaceDictFrames opts sch batches


{- | Frames for the 'DictReplaceOnChange' strategy. Each record batch
is preceded by fresh @isDelta=false@ dict batches for any dictionary
id whose values have changed since the last emission, plus every
dictionary whose values contain one of those.
-}
replaceDictFrames :: WriteOptions -> Schema -> [V.Vector ColumnArray] -> Either String [Frame]
replaceDictFrames opts sch = go Map.empty []
  where
    !mCodec = writeBodyCompression opts
    !plan = dictPlan sch
    go _ acc [] = Right (concat (reverse acc))
    go !lastDicts acc (cols0 : rest) = do
      (curDicts, rebased) <- unifyDictionaries plan [cols0]
      let !cols = case rebased of
            [c] -> c
            _ -> cols0
          !changed = Map.keysSet (Map.filterWithKey (\did vs -> Map.lookup did lastDicts /= Just vs) curDicts)
          !emitDicts = Map.restrictKeys curDicts (withDependents plan changed)
      dictFrames <- buildDictFrames mCodec sch plan emitDicts
      eb <- encodeBatch mCodec (arrowFields sch) cols
      go (Map.union curDicts lastDicts) ((dictFrames ++ [batchFrame eb]) : acc) rest


{- | Decode an Arrow IPC stream back into 'Schema' + record-batch
column vectors. Dictionary references are resolved
transparently: the returned 'ColumnArray' values contain the
actual dictionary values, not just keys. Columns alias the input.
-}
decodeArrowStream
  :: ByteString
  -> Either String (Schema, [V.Vector ColumnArray])
decodeArrowStream bs = do
  (sch, frames) <- readArrowStreamFBInterleaved bs
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
  Compiled dicts rbs <- compileBatches opts sch batches
  Right (renderFile sch dicts rbs)


-- | 'encodeArrowFile' as a lazy 'BL.ByteString' aliasing the column buffers (see 'encodeArrowStreamLazy').
encodeArrowFileLazy
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> Either String BL.ByteString
encodeArrowFileLazy opts sch batches = do
  mapM_ (validateColumns (arrowFields sch)) batches
  Compiled dicts rbs <- compileBatches opts sch batches
  Right (renderFileLazy sch dicts rbs)


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
-- Internal: dictionary planning and unification
-- ============================================================

-- | Dictionary frames (in 'dpOrder') and record batch frames.
data Compiled = Compiled ![Frame] ![Frame]


{- | Unify the dictionaries of every batch (see 'unifyDictionaries'),
then lay out one dictionary batch per id (nested dictionaries first,
see 'dictPlan') and every record batch.
-}
compileBatches :: WriteOptions -> Schema -> [V.Vector ColumnArray] -> Either String Compiled
compileBatches opts sch batches = do
  let !mCodec = writeBodyCompression opts
      !plan = dictPlan sch
  (dictMap, rebased) <- unifyDictionaries plan batches
  dicts <- buildDictFrames mCodec sch plan dictMap
  rbs <- traverse (fmap batchFrame . encodeBatch mCodec (arrowFields sch)) rebased
  Right (Compiled dicts rbs)


{- | What the writer needs to know about the schema's dictionaries:
the order to emit them in, the narrowest index type declared for each
id, and the ids nested inside each id's value type.
-}
data DictPlan = DictPlan
  { dpOrder :: ![Int64]
  -- ^ Every dictionary id, a dictionary nested inside another's values before the outer one.
  , dpIndexType :: !(Map.Map Int64 ArrowType)
  -- ^ The narrowest index type over the fields sharing the id.
  , dpNested :: !(Map.Map Int64 (Set.Set Int64))
  -- ^ Ids declared anywhere inside the id's value type.
  }


-- | Walk the schema in post-order (children before parents) collecting the 'DictPlan'.
dictPlan :: Schema -> DictPlan
dictPlan sch =
  let ((order, ixTypes, nested), _) = goFields ([], Map.empty, Map.empty) (arrowFields sch)
  in DictPlan {dpOrder = reverse order, dpIndexType = ixTypes, dpNested = nested}
  where
    goFields st fs = V.foldl' (\(s, ids) f -> let (s', ids') = goField s f in (s', Set.union ids ids')) (st, Set.empty) fs
    goField st f =
      let ((order, ixTypes, nested), inner) = goFields st (fieldChildren f)
      in case fieldDictionary f of
           Nothing -> ((order, ixTypes, nested), inner)
           Just de ->
             let !did = deId de
             in ( ( if did `elem` order then order else did : order
                  , Map.insertWith narrower did (deIndexType de) ixTypes
                  , Map.insertWith Set.union did inner nested
                  )
                , Set.insert did inner
                )
    narrower new old = if maxIndexFor new < maxIndexFor old then new else old


-- | Largest dictionary key an index type holds (signed 32-bit when the type is not an integer).
maxIndexFor :: ArrowType -> Int64
maxIndexFor = \case
  AInt 8 True -> 127
  AInt 8 False -> 255
  AInt 16 True -> 32767
  AInt 16 False -> 65535
  AInt 32 False -> 4294967295
  AInt 64 _ -> maxBound
  _ -> 2147483647


-- | The key element type for an index type (signed 32-bit when the type is not an integer).
keyPrimFor :: ArrowType -> SomePrimType
keyPrimFor ty = case primTypeFor ty of
  Just (SomePrimType t) | Just IntegralPrim <- integralPrim t -> SomePrimType t
  _ -> SomePrimType PInt32


-- | The ids together with every dictionary whose values contain one of them.
withDependents :: DictPlan -> Set.Set Int64 -> Set.Set Int64
withDependents plan ids =
  Set.union ids (Map.keysSet (Map.filter (not . Set.disjoint ids) (dpNested plan)))


{- | One dictionary batch frame per id present in the map, in
'dpOrder'. The value field is the dictionary field itself minus its
encoding (same type and children).
-}
buildDictFrames :: Maybe BodyCompressionCodec -> Schema -> DictPlan -> Map.Map Int64 ColumnArray -> Either String [Frame]
buildDictFrames mCodec sch plan dictMap = traverse one (filter (`Map.member` dictMap) (dpOrder plan))
  where
    one did = case findDictField did (arrowFields sch) of
      Nothing -> Left ("Arrow.Stream: dictionary id " ++ show did ++ " has no field in the schema")
      Just f -> do
        let !valuesField = f {fieldDictionary = Nothing, fieldNullable = True}
        eb <- encodeBatch mCodec (V.singleton valuesField) (V.singleton (Map.findWithDefault (I.ColNull 0) did dictMap))
        Right (dictionaryFrame did False eb)


-- | Distinct value columns seen for one id (newest first, with their offsets) and their total length.
data DictAcc = DictAcc ![(ColumnArray, Int64)] !Int64


{- | Collect the dictionaries of a run of batches into one values
column per id. Walking the batches in order, each dictionary column's
values are appended to its id's combined dictionary unless an equal
values column was already appended, and the column's keys are
shifted by the position of its values inside the combined dictionary
(written at the id's narrowest index type), so every batch can be
written against a single dictionary batch.

Dictionary columns inside dictionary values are collected the same
way, before their outer value column is placed, so the outer
dictionary is written with keys into the combined inner one. The
rebased columns carry an empty values column of the right type (the
writer only needs the keys), which keeps the concatenation of
outer value columns from shifting already rebased inner keys.

Fails when the value columns of one id cannot be concatenated, or
when the combined dictionary has more values than the id's index
type can address.
-}
unifyDictionaries :: DictPlan -> [V.Vector ColumnArray] -> Either String (Map.Map Int64 ColumnArray, [V.Vector ColumnArray])
unifyDictionaries plan batches = do
  (seen, rebased) <- mapAccumM (mapAccumM (mapAccumDictionaries shiftOne)) Map.empty batches
  dicts <- Map.traverseWithKey combine seen
  Right (dicts, rebased)
  where
    indexType did = Map.findWithDefault (AInt 32 True) did (dpIndexType plan)
    shiftOne st col = case col of
      I.ColDictionary did keys vals -> do
        (st1, vals') <- mapAccumDictionaries shiftOne st vals
        (st2, off) <- place st1 did vals'
        keys' <- if off == 0 then Right keys else shiftKeys (keyPrimFor (indexType did)) off keys
        Right (st2, I.ColDictionary did keys' (sliceColumnArray 0 0 vals'))
      _ -> Right (st, col)
    place st did vals =
      let !len = fromIntegral (columnLength vals) :: Int64
      in case Map.lookup did st of
           Nothing -> Right (Map.insert did (DictAcc [(vals, 0)] len) st, 0)
           Just (DictAcc entries total) -> case lookup vals entries of
             Just off -> Right (st, off)
             Nothing -> Right (Map.insert did (DictAcc ((vals, total) : entries) (total + len)) st, total)
    combine did (DictAcc entries total)
      | total - 1 > maxIx =
          Left
            ( "Arrow.Stream: dictionary id "
                ++ show did
                ++ ": the batches' dictionaries combine to "
                ++ show total
                ++ " values but the field's index type addresses at most "
                ++ show (toInteger maxIx + 1)
                ++ " (use DictReplaceOnChange or a wider index type)"
            )
      | otherwise = case reverse entries of
          [(v, _)] -> Right v
          ordered -> case concatColumnArrays (map fst ordered) of
            Right v -> Right v
            Left e -> Left ("Arrow.Stream: dictionary id " ++ show did ++ ": value columns cannot be combined: " ++ e)
      where
        !maxIx = maxIndexFor (indexType did)


{- | Keys plus an offset, at the given integer type (one pass). Valid
keys stay below the combined dictionary length, which 'combine' checks
against the index type, so they never wrap; null slots may.
-}
shiftKeys :: SomePrimType -> Int64 -> ColumnArray -> Either String ColumnArray
shiftKeys (SomePrimType tt) off keys = case keys of
  I.ColPrim kt v ks
    | Just IntegralPrim <- integralPrim kt
    , Just IntegralPrim <- integralPrim tt ->
        Right (I.ColPrim tt v (VS.map (\k -> fromIntegral (fromIntegral k + off)) ks))
  _ -> Left "Arrow.Stream: dictionary keys must be an integer column"


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
      I.ColDictionary {} -> f st col
      I.ColStruct n v cs -> (\(st', kids) -> (st', I.ColStruct n v kids)) <$> mapAccumM named st cs
      I.ColList v o c -> one st c (I.ColList v o)
      I.ColLargeList v o c -> one st c (I.ColLargeList v o)
      I.ColFixedSizeList w n v c -> one st c (I.ColFixedSizeList w n v)
      I.ColMap v o k x -> do
        (st1, k') <- go st k
        (st2, x') <- go st1 x
        Right (st2, I.ColMap v o k' x')
      I.ColDenseUnion ts o cs -> (\(st', kids) -> (st', I.ColDenseUnion ts o kids)) <$> mapAccumM go st cs
      I.ColSparseUnion ts cs -> (\(st', kids) -> (st', I.ColSparseUnion ts kids)) <$> mapAccumM go st cs
      I.ColRunEndEncoded off n re vs -> one st vs (I.ColRunEndEncoded off n re)
      I.ColListView v o s c -> one st c (I.ColListView v o s)
      I.ColLargeListView v o s c -> one st c (I.ColLargeListView v o s)
      _ -> Right (st, col)
    named st (nm, c) = (\(st', c') -> (st', (nm, c'))) <$> go st c


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
-- Internal: decoding
-- ============================================================

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
  resolved <- traverse (\(rb, body) -> decodeResolved sch dictMap rb body) frames
  Right (sch, resolved)


-- | One record batch with its dictionary columns resolved against the dictionaries in force.
decodeResolved :: Schema -> Map.Map Int64 ColumnArray -> RecordBatchDef -> ByteString -> Either String (V.Vector ColumnArray)
decodeResolved sch dictMap rb body = do
  cols <- decodeRecordBatch sch rb body
  V.mapM (resolveDictionaryColumn (`Map.lookup` dictMap)) cols


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
  (did, raw) <- decodeDictionaryBatch sch db
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
  -> [StreamFrame]
  -> Either String (Schema, [V.Vector ColumnArray])
decodeInterleavedFrames sch = go Map.empty []
  where
    go _ acc [] = Right (sch, reverse acc)
    go !dictMap acc (SFDict db : rest) = do
      newMap <- applyDictBatch sch dictMap db
      go newMap acc rest
    go !dictMap acc (SFBatch rb body : rest) = do
      resolved <- decodeResolved sch dictMap rb body
      go dictMap (resolved : acc) rest


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

Dictionary batches are applied (replacements and deltas) as
'streamReaderNext' walks past them, so each record batch sees the
dictionaries in force at its position. Use 'streamReaderToList' to
drain the iterator into a list (equivalent to 'decodeArrowStream' but
keeps the streaming shape for callers that want incremental
processing later).
-}
data StreamReader = StreamReader
  { srSchema :: !Schema
  , srDictMap :: !(Map.Map Int64 ColumnArray)
  -- ^ Dictionaries in force at the current stream position.
  , srFrames :: ![StreamFrame]
  }


{- | Initialise an iterator from raw stream bytes. Parses the schema
and splits the stream into frames; nothing is decoded until
'streamReaderNext'.
-}
openStreamReader :: ByteString -> Either String StreamReader
openStreamReader bs = do
  (sch, frames) <- readArrowStreamFBInterleaved bs
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

  * @Right (Just (cols, rd'))@: a decoded batch with
    dictionary references resolved, plus a continuation
    reader for the remaining frames.
  * @Right Nothing@: the stream's EOS marker has been
    consumed; no more batches.
  * @Left e@: a parse / validation error.
-}
streamReaderNext
  :: StreamReader
  -> Either String (Maybe (V.Vector ColumnArray, StreamReader))
streamReaderNext rd = case srFrames rd of
  [] -> Right Nothing
  (SFDict db : rest) -> do
    dictMap <- applyDictBatch (srSchema rd) (srDictMap rd) db
    streamReaderNext rd {srDictMap = dictMap, srFrames = rest}
  (SFBatch rb body : rest) -> do
    resolved <- decodeResolved (srSchema rd) (srDictMap rd) rb body
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
Each 'IS.iterStep' decodes one record batch and resolves dictionary
references, matching 'streamReaderNext' semantics without forcing
the caller to thread the continuation reader by hand.
-}
streamReaderIter :: StreamReader -> IS.Iter (V.Vector ColumnArray)
streamReaderIter rd0 = IS.iterUnfold rd0 streamReaderNext


-- ============================================================
-- Column projection
-- ============================================================

{- | Like 'streamReaderToList' but only returns the named
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

The whole record batch is still decoded (and validated), but decoding
aliases the input, so the unrequested columns cost only their
validation.
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
  let !nameToIdx = Map.fromList (V.toList (V.imap (\i f -> (fieldName f, i)) (arrowFields sch)))
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
