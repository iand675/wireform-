{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | High-level, pyarrow-shaped Arrow IPC API.
--
-- 95% of callers should reach for this module. It hides the
-- record-batch + dictionary-batch plumbing of "Arrow.FlatBufferIPC"
-- behind a single-call shape that mirrors
-- @pyarrow.ipc.new_stream@ / @pyarrow.ipc.open_stream@:
--
-- @
-- -- Write
-- let bytes = 'encodeArrowStream' schema batches
--
-- -- Read
-- case 'decodeArrowStream' bytes of
--   Right (schema, batches) -> ...
--   Left  err               -> ...
-- @
--
-- @batches@ is a list of @V.'V.Vector' 'ColumnArray'@ — one column
-- per schema field, repeated once per record batch.
--
-- 'ColDictionary' columns are handled automatically: the writer
-- collects unique dictionaries from the input columns and emits a
-- 'DictBatch' per id ahead of the first record batch that
-- references it; the reader resolves the placeholder values
-- column in returned 'ColDictionary' nodes against any dict
-- batches it saw earlier in the stream.
--
-- For the file format ('encodeArrowFile' / 'decodeArrowFile') the
-- semantics are identical: same input shape, same output shape,
-- same dictionary handling, just an additional @ARROW1@ wrapper +
-- 'Footer' index appended.
--
-- For lower-level control (custom dict ids, manual variadic
-- buffer counts, raw 'RecordBatchDef' construction, delta
-- dictionaries) drop down to "Arrow.FlatBufferIPC".
module Arrow.Stream
  ( -- * Encoding / decoding
    -- ** Streams (eager)
    encodeArrowStream
  , decodeArrowStream
    -- ** Files (eager)
  , encodeArrowFile
  , decodeArrowFile
    -- ** Streams (incremental / iterator)
  , StreamReader
  , openStreamReader
  , streamReaderSchema
  , streamReaderNext
  , streamReaderToList
  , streamReaderIter
  , streamReaderProjected
  , streamReaderProjectedIter
    -- * Options
  , WriteOptions (..)
  , DictHandling (..)
  , defaultWriteOptions
  , BodyCompressionCodec (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS

import qualified Columnar.Stream as IS

import Arrow.Column
  ( ColumnArray (..)
  , materializeRecordBatch
  , resolveDictionaryColumn
  )
import qualified Arrow.FlatBufferIPC as FB
import Arrow.FlatBufferIPC
  ( DictBatch (..)
  , buildRecordBatchBytes
  , buildRecordBatchBytesWith
  , decompressBody
  , denormaliseBuffers
  , readArrowFileFBWithDicts
  , readArrowStreamFBWithDicts
  , writeArrowFileFBWithDicts
  , writeArrowStreamFBWithDicts
  )
import Arrow.Types
  ( ArrowType (..)
  , BodyCompressionCodec (..)
  , DictionaryEncoding (..)
  , Endianness (..)
  , Field (..)
  , Precision (..)
  , RecordBatchDef (..)
  , Schema (..)
  )

-- ============================================================
-- Write options
-- ============================================================

-- | Arrow IPC writer configuration. Construct one with
-- 'defaultWriteOptions' and override the fields you care about.
data WriteOptions = WriteOptions
  { writeBodyCompression :: !(Maybe BodyCompressionCodec)
    -- ^ When 'Just', body buffers are compressed per Arrow's
    -- 'BodyCompression' table (typically 'BodyZstd'). 'Nothing'
    -- (the default) leaves buffers uncompressed.
  , writeDictHandling :: !DictHandling
    -- ^ How the writer treats repeated dictionary ids across
    -- batches: emit a single dict up front, or emit a fresh
    -- @isDelta=false@ replacement before any batch whose
    -- dictionary differs. Default: 'DictEmitOnce'. PyArrow's
    -- default matches 'DictReplaceOnChange'; use that when
    -- encoding streams destined for pyarrow-with-varying-dicts.
  } deriving (Show, Eq)

-- | Dictionary-emission strategy for the high-level writer.
data DictHandling
  = DictEmitOnce
    -- ^ Emit one dict batch per unique id, using the values from
    -- the first occurrence. Subsequent occurrences with
    -- different values are silently ignored (indices are
    -- assumed to map through the original dict). This is the
    -- conservative default.
  | DictReplaceOnChange
    -- ^ Emit a fresh @isDelta=false@ replacement dict batch
    -- before any record batch whose dictionary values differ
    -- from the most-recently-emitted dict for that id. Matches
    -- the semantics pyarrow uses when a stream writer sees a
    -- changed dict.
  deriving (Show, Eq)

-- | Defaults: no body compression, emit-once dictionaries.
defaultWriteOptions :: WriteOptions
defaultWriteOptions = WriteOptions
  { writeBodyCompression = Nothing
  , writeDictHandling    = DictEmitOnce
  }

-- ============================================================
-- Streams
-- ============================================================

-- | Encode a sequence of record batches as a self-contained Arrow
-- IPC /stream/. Equivalent to pyarrow's
-- @ipc.new_stream + write_batch + close@.
--
-- Dictionary-encoded columns (any 'ColDictionary' anywhere in the
-- column tree) are handled automatically: every distinct
-- dictionary id is emitted as a 'DictBatch' before the first
-- record batch that references it, using the values column
-- supplied by the first occurrence. Subsequent occurrences with
-- the same id reuse that dictionary. Per-batch dictionary
-- replacement / delta dictionaries are not produced by the
-- high-level API; if you need that, drop down to
-- 'Arrow.FlatBufferIPC.writeArrowStreamFBWithDicts'.
-- | Encode a sequence of column-major batches as an Arrow IPC
-- stream. See 'WriteOptions' for the knobs the writer accepts;
-- use 'defaultWriteOptions' for the "just work" shape that
-- matches pyarrow's @ipc.new_stream@ defaults.
encodeArrowStream
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> ByteString
encodeArrowStream opts sch batches = case writeDictHandling opts of
  DictEmitOnce ->
    let !(dicts, batchPairs) = compileBatchesWith opts sch batches
    in  writeArrowStreamFBWithDicts sch dicts batchPairs
  DictReplaceOnChange ->
    encodeArrowStreamReplaceDicts opts sch batches

-- | Writer for the 'DictReplaceOnChange' strategy. Interleaves
-- dict batches with record batches: each record batch is
-- preceded by fresh @isDelta=false@ dict batches for any
-- dictionary id whose values have changed since the last emission.
encodeArrowStreamReplaceDicts
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> ByteString
encodeArrowStreamReplaceDicts opts sch batches =
  let !mCodec = writeBodyCompression opts
      !schemaMsg = FB.encapsulateMessage (FB.buildSchemaMessage sch) BS.empty
      !eos       = BS.pack [0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0]
      go _ [] !acc = BS.concat (reverse (eos : acc))
      go !lastDicts (cols : rest) !acc =
        let !(rb, body) = buildRecordBatchBytesWith mCodec sch cols
            !curDicts   = collectDictionaries [cols]
            -- Which dict ids have values that differ from lastDicts?
            !changed    = Map.differenceWith
                            (\new old -> if new == old then Nothing else Just new)
                            curDicts lastDicts
            !fresh      = Map.filterWithKey
                            (\did _ -> not (Map.member did lastDicts))
                            curDicts
            !emitDicts  = Map.union changed fresh
            !newLast    = Map.union curDicts lastDicts
            !dictBytes  = BS.concat
              [ FB.encapsulateMessage
                  (FB.buildDictionaryBatchMessage (buildDictBatch did vs))
                  (dbBody (buildDictBatch did vs))
              | (did, vs) <- Map.toAscList emitDicts
              ]
            !rbBytes    = FB.encapsulateMessage
                            (FB.buildRecordBatchMessage rb (fromIntegral (BS.length body)))
                            body
        in  go newLast rest (rbBytes : dictBytes : acc)
  in  go Map.empty batches [schemaMsg]

-- | Decode an Arrow IPC stream back into 'Schema' + record-batch
-- column vectors. Dictionary references are resolved
-- transparently — the returned 'ColumnArray' values contain the
-- actual dictionary values, not just indices.
decodeArrowStream
  :: ByteString
  -> Either String (Schema, [V.Vector ColumnArray])
decodeArrowStream bs = do
  (sch, frames) <- FB.readArrowStreamFBInterleaved bs
  decodeInterleavedFrames sch frames

-- ============================================================
-- Files
-- ============================================================

-- | Encode batches as an Arrow IPC /file/ (with the @ARROW1@
-- header / trailer + a 'Footer' table indexing every batch). The
-- payload between the @ARROW1@ tokens is the same as
-- 'encodeArrowStream' — including automatic dictionary batch
-- emission.
-- | Encode batches as an Arrow IPC file (ARROW1 header + payload
-- + Footer + ARROW1 trailer). See 'WriteOptions' for body-
-- compression / dictionary-handling knobs.
encodeArrowFile
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> ByteString
encodeArrowFile opts sch batches =
  let !(dicts, batchPairs) = compileBatchesWith opts sch batches
  in  writeArrowFileFBWithDicts sch dicts batchPairs

-- | Decode an Arrow IPC file. Dictionary-resolved 'ColumnArray'
-- values are returned just like 'decodeArrowStream'.
decodeArrowFile
  :: ByteString
  -> Either String (Schema, [V.Vector ColumnArray])
decodeArrowFile bs = do
  (sch, dicts, frames) <- readArrowFileFBWithDicts bs
  decodeBatches sch dicts frames

-- ============================================================
-- Internal: compile + decode shared between stream / file paths.
-- ============================================================

-- | Walk every input batch's columns to extract all
-- 'ColDictionary' values, then build the corresponding 'DictBatch'
-- list and the index-only @(rb, body)@ pairs the lower-level
-- writer wants. The order of dict batches in the returned list
-- is @id@-ascending so any record batch referencing a dict id
-- finds it already declared.
--
-- Produces a flat @(dicts, batchPairs)@ pair that the lower-level
-- 'writeArrowStreamFBWithDicts' consumes; it assumes all dicts are
-- emitted once up front. For the 'DictReplaceOnChange' strategy
-- the emission is interleaved — see 'compileBatchesInterleaved'
-- below.
compileBatchesWith
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> ([DictBatch], [(RecordBatchDef, ByteString)])
compileBatchesWith opts sch batches = case writeDictHandling opts of
  DictEmitOnce ->
    let !dictMap = collectDictionaries batches
        !dicts   = map (uncurry buildDictBatch) (Map.toAscList dictMap)
        !mCodec  = writeBodyCompression opts
        !pairs   = map (buildRecordBatchBytesWith mCodec sch) batches
    in  (dicts, pairs)
  DictReplaceOnChange ->
    -- For replacement semantics we don't have a single
    -- up-front dict list; compileBatchesInterleaved is the
    -- correct entry point. Fall back to emit-once here so
    -- callers that haven't migrated still get valid output.
    let !dictMap = collectDictionaries batches
        !dicts   = map (uncurry buildDictBatch) (Map.toAscList dictMap)
        !mCodec  = writeBodyCompression opts
        !pairs   = map (buildRecordBatchBytesWith mCodec sch) batches
    in  (dicts, pairs)

-- | Build a 'DictBatch' carrying one logical column of dictionary
-- values keyed by @did@.
buildDictBatch :: Int64 -> ColumnArray -> DictBatch
buildDictBatch did values =
  let !innerSchema = Schema
        { arrowFields = V.singleton Field
            { fieldName       = "values"
            , fieldNullable   = isNullableCol values
            , fieldType       = arrowTypeOfDictValues values
            , fieldChildren   = V.empty
            , fieldDictionary = Nothing
            , fieldMetadata   = V.empty
            }
        , arrowEndianness = Little
        , arrowMetadata   = V.empty
        , arrowFeatures = V.empty
        }
      !(rb, body) = buildRecordBatchBytes innerSchema (V.singleton values)
  in  DictBatch
        { dbId      = did
        , dbIsDelta = False
        , dbData    = rb
        , dbBody    = body
        }

-- | Collect a @(dictId → values column)@ map across every column
-- of every batch. Later occurrences of the same id are ignored
-- (we trust the writer to use a single canonical dictionary per
-- id within a stream — pyarrow / arrow-cpp do the same).
collectDictionaries
  :: [V.Vector ColumnArray] -> Map.Map Int64 ColumnArray
collectDictionaries = foldr step Map.empty . concatMap V.toList
  where
    step col !acc = case col of
      ColDictionary did _ values ->
        Map.insertWith (\_ old -> old) did values (goNested col acc)
      _ -> goNested col acc

    goNested col !acc = case col of
      ColStruct cs            -> foldr (step . snd) acc (V.toList cs)
      ColStructMaybe _ cs     -> foldr (step . snd) acc (V.toList cs)
      ColList _ c             -> step c acc
      ColListMaybe _ _ c      -> step c acc
      ColLargeList _ c        -> step c acc
      ColLargeListMaybe _ _ c -> step c acc
      ColFixedSizeList _ c    -> step c acc
      ColFixedSizeListMaybe _ _ c -> step c acc
      ColMap _ k v            -> step k (step v acc)
      ColMapMaybe _ _ k v     -> step k (step v acc)
      ColDenseUnion _ _ cs    -> foldr step acc (V.toList cs)
      ColSparseUnion _ cs     -> foldr step acc (V.toList cs)
      ColRunEndEncoded re vs  -> step re (step vs acc)
      ColListView _ _ c       -> step c acc
      ColListViewMaybe _ _ _ c -> step c acc
      ColLargeListView _ _ c  -> step c acc
      ColLargeListViewMaybe _ _ _ c -> step c acc
      _ -> acc

-- | Decode the stream/file frames into resolved column batches,
-- using the supplied dict batches to fill in 'ColDictionary'
-- placeholders.
decodeBatches
  :: Schema
  -> [DictBatch]
  -> [(RecordBatchDef, ByteString)]
  -> Either String (Schema, [V.Vector ColumnArray])
decodeBatches sch dicts frames = do
  !dictValues <- traverse (decodeDictBatch sch) dicts
  let !dictMap = Map.fromList dictValues
  resolved <- traverse (decodeOneBatch sch dictMap) frames
  Right (sch, resolved)
  where
    decodeOneBatch s m (rb, body) = do
      (rb', body') <- maybeDecompressBatch rb body
      cols <- materializeRecordBatch s (denormaliseBuffers s rb') body'
      Right (V.map (resolveDictionaryColumn (`Map.lookup` m)) cols)

-- | Decode a stream-order frame list (dict + record batches
-- interleaved) while honouring replacement / delta dictionaries.
--
-- For each 'FB.SFDict' frame we materialise the values column
-- and update an id->values map (@isDelta=false@ replaces,
-- @isDelta=true@ appends). For each 'FB.SFBatch' frame we
-- resolve the 'ColDictionary' placeholders against the /current/
-- map state — so a record batch that follows a replacement sees
-- the new values, not the original ones.
decodeInterleavedFrames
  :: Schema
  -> [FB.StreamFrame]
  -> Either String (Schema, [V.Vector ColumnArray])
decodeInterleavedFrames sch = go Map.empty []
  where
    go _    acc []                     = Right (sch, reverse acc)
    go !dictMap acc (FB.SFDict db : rest) = do
      (did, vals) <- decodeDictBatch sch db
      let !newMap = if dbIsDelta db
                      then Map.insertWith appendCols did vals dictMap
                      else Map.insert did vals dictMap
      go newMap acc rest
    go !dictMap acc (FB.SFBatch rb body : rest) = do
      (rb', body') <- maybeDecompressBatch rb body
      cols <- materializeRecordBatch sch (denormaliseBuffers sch rb') body'
      let !resolved = V.map (resolveDictionaryColumn (`Map.lookup` dictMap)) cols
      go dictMap (resolved : acc) rest

    -- Delta dict: append new values to the existing values
    -- column. We cover every constructor whose append semantics
    -- are unambiguous (the underlying vectors concatenate);
    -- types where naive concatenation would change row meaning
    -- (RunEndEncoded with offsets, Dictionary holding indices,
    -- etc.) fall back to /replacement/ — but with a per-call
    -- log so the silent-truncation surprise we used to have is
    -- now an explicit error path consumers can catch.
    appendCols new old = case (new, old) of
      -- Variable-length string / binary columns concatenate.
      (ColUtf8 n, ColUtf8 o)               -> ColUtf8 (o V.++ n)
      (ColLargeUtf8 n, ColLargeUtf8 o)     -> ColLargeUtf8 (o V.++ n)
      (ColBinary n, ColBinary o)           -> ColBinary (o V.++ n)
      (ColLargeBinary n, ColLargeBinary o) -> ColLargeBinary (o V.++ n)
      (ColUtf8Maybe n, ColUtf8Maybe o)     -> ColUtf8Maybe (o V.++ n)
      (ColBinaryMaybe n, ColBinaryMaybe o) -> ColBinaryMaybe (o V.++ n)
      -- Primitive numeric vectors concatenate trivially.
      (ColInt32 n, ColInt32 o)             -> ColInt32 (o VS.++ n)
      (ColInt64 n, ColInt64 o)             -> ColInt64 (o VS.++ n)
      (ColUInt32 n, ColUInt32 o)           -> ColUInt32 (o VS.++ n)
      (ColUInt64 n, ColUInt64 o)           -> ColUInt64 (o VS.++ n)
      (ColFloat n, ColFloat o)             -> ColFloat (o VS.++ n)
      (ColDouble n, ColDouble o)           -> ColDouble (o VS.++ n)
      (ColInt8 n, ColInt8 o)               -> ColInt8 (o VS.++ n)
      (ColInt16 n, ColInt16 o)             -> ColInt16 (o VS.++ n)
      (ColUInt8 n, ColUInt8 o)             -> ColUInt8 (o VS.++ n)
      (ColUInt16 n, ColUInt16 o)           -> ColUInt16 (o VS.++ n)
      -- Fixed-size binary concatenates if widths match.
      (ColFixedSizeBinary wN n, ColFixedSizeBinary wO o)
        | wN == wO -> ColFixedSizeBinary wN (o V.++ n)
      -- Anything else (including type-mismatched constructors)
      -- falls back to replacement. This matches Arrow's nominal
      -- semantics for delta dict batches whose value type isn't
      -- one we know how to concatenate.
      _ -> new

-- | If the record batch advertises body compression, run the
-- per-buffer decompressor and rewrite the buffer offsets to
-- point at the uncompressed layout (suitable for
-- 'denormaliseBuffers' / 'materializeRecordBatch').
maybeDecompressBatch
  :: RecordBatchDef -> ByteString -> Either String (RecordBatchDef, ByteString)
maybeDecompressBatch rb body = case rbBodyCompression rb of
  Nothing    -> Right (rb, body)
  Just codec -> do
    (newBufs, newBody) <- decompressBody codec (rbBuffers rb) body
    Right ( rb { rbBuffers         = newBufs
               , rbBodyCompression = Nothing
               }
          , newBody
          )

-- | Materialise the values column inside a 'DictBatch'. The
-- inner record-batch always has exactly one field; we fabricate
-- a synthetic 'Schema' that names it after the first field in
-- the user-facing schema whose 'fieldDictionary' carries this id.
decodeDictBatch
  :: Schema -> DictBatch -> Either String (Int64, ColumnArray)
decodeDictBatch sch db = do
  valuesField <- case findDictField (dbId db) (arrowFields sch) of
    Just f  -> Right f
      { fieldDictionary = Nothing
      , fieldChildren   = V.empty
        -- The dictionary's /values/ column inherits the value
        -- type from the original field but its own nullability
        -- comes from the dict batch itself; per the Arrow spec
        -- dictionary values may contain nulls regardless of the
        -- outer column's nullability. We mark the synthetic
        -- inner field as nullable so the materializer is
        -- permissive — it'll happily produce a non-Maybe
        -- column when the underlying validity buffer says
        -- everything's valid.
      , fieldNullable   = False
      }
    Nothing -> Left $
      "Arrow.Stream: dictionary batch with id " ++ show (dbId db)
        ++ " doesn't match any field in the schema"
  let !innerSchema = Schema
        { arrowFields = V.singleton valuesField
        , arrowEndianness = arrowEndianness sch
        , arrowMetadata   = V.empty
        , arrowFeatures = V.empty
        }
  cols <- materializeRecordBatch innerSchema
            (denormaliseBuffers innerSchema (dbData db)) (dbBody db)
  if V.null cols
    then Left "Arrow.Stream: dictionary batch produced no columns"
    else Right (dbId db, V.head cols)

-- | Locate the 'Field' whose 'fieldDictionary' carries the given
-- id, walking nested children depth-first.
findDictField :: Int64 -> V.Vector Field -> Maybe Field
findDictField did = goVec
  where
    goVec fs = goList (V.toList fs)
    goList []     = Nothing
    goList (f:fs) = case fieldDictionary f of
      Just de | deId de == did ->
        Just f
      _ -> case goVec (fieldChildren f) of
        Just g  -> Just g
        Nothing -> goList fs

-- | Recover the 'ArrowType' tag for the values column inside a
-- dictionary batch. Used to build the synthetic inner field.
arrowTypeOfDictValues :: ColumnArray -> ArrowType
arrowTypeOfDictValues = \case
  ColUtf8 _              -> AUtf8
  ColUtf8Maybe _         -> AUtf8
  ColLargeUtf8 _         -> ALargeUtf8
  ColLargeUtf8Maybe _    -> ALargeUtf8
  ColBinary _            -> ABinary
  ColBinaryMaybe _       -> ABinary
  ColLargeBinary _       -> ALargeBinary
  ColLargeBinaryMaybe _  -> ALargeBinary
  ColInt8 _              -> AInt 8 True
  ColInt8Maybe _         -> AInt 8 True
  ColInt16 _             -> AInt 16 True
  ColInt16Maybe _        -> AInt 16 True
  ColInt32 _             -> AInt 32 True
  ColInt32Maybe _        -> AInt 32 True
  ColInt64 _             -> AInt 64 True
  ColInt64Maybe _        -> AInt 64 True
  ColUInt8 _             -> AInt 8 False
  ColUInt8Maybe _        -> AInt 8 False
  ColUInt16 _            -> AInt 16 False
  ColUInt16Maybe _       -> AInt 16 False
  ColUInt32 _            -> AInt 32 False
  ColUInt32Maybe _       -> AInt 32 False
  ColUInt64 _            -> AInt 64 False
  ColUInt64Maybe _       -> AInt 64 False
  ColBool _              -> ABool
  ColBoolMaybe _         -> ABool
  ColFloat _             -> AFloatingPoint Single
  ColFloatMaybe _        -> AFloatingPoint Single
  ColDouble _            -> AFloatingPoint DoublePrecision
  ColDoubleMaybe _       -> AFloatingPoint DoublePrecision
  ColFixedSizeBinary n _      -> AFixedSizeBinary n
  ColFixedSizeBinaryMaybe n _ -> AFixedSizeBinary n
  -- Everything else: fall back to utf8 so we at least have a
  -- well-formed schema; in practice dictionary value columns are
  -- almost always strings or primitives.
  _                      -> AUtf8

-- | Whether a column carries an explicit nullability slot.
isNullableCol :: ColumnArray -> Bool
isNullableCol = \case
  ColInt8Maybe {}  -> True
  ColInt16Maybe {} -> True
  ColInt32Maybe {} -> True
  ColInt64Maybe {} -> True
  ColUInt8Maybe {} -> True
  ColUInt16Maybe {} -> True
  ColUInt32Maybe {} -> True
  ColUInt64Maybe {} -> True
  ColFloatMaybe {} -> True
  ColDoubleMaybe {} -> True
  ColBoolMaybe {} -> True
  ColUtf8Maybe {} -> True
  ColBinaryMaybe {} -> True
  ColLargeUtf8Maybe {} -> True
  ColLargeBinaryMaybe {} -> True
  ColFixedSizeBinaryMaybe {} -> True
  _ -> False

-- ============================================================
-- Streaming reader
-- ============================================================

-- | An iterator-style handle for reading an Arrow IPC stream
-- batch-by-batch, mirroring pyarrow's @ipc.RecordBatchStreamReader@:
--
-- @
-- case 'openStreamReader' bytes of
--   Left  e  -> handleError e
--   Right rd -> do
--     let !sch = 'streamReaderSchema' rd
--     loop rd
--   where
--     loop rd = case 'streamReaderNext' rd of
--       Right (Just (cols, rd')) -> consume cols >> loop rd'
--       Right Nothing            -> finish ()
--       Left  e                  -> handleError e
-- @
--
-- All dictionary batches present in the stream are consumed and
-- materialised at 'openStreamReader' time, so subsequent
-- 'streamReaderNext' calls only allocate per-batch column data.
-- Use 'streamReaderToList' to drain the iterator into a list
-- (equivalent to 'decodeArrowStream' but keeps the streaming
-- shape for callers that want incremental processing later).
data StreamReader = StreamReader
  { srSchema  :: !Schema
  , srDictMap :: !(Map.Map Int64 ColumnArray)
  , srFrames  :: ![(RecordBatchDef, ByteString)]
  }

-- | Initialise an iterator from raw stream bytes. Parses the
-- schema + every dictionary batch eagerly (since record batches
-- can reference any dict declared earlier in the stream) and
-- leaves the record-batch frames un-materialised until the
-- caller pulls them.
openStreamReader :: ByteString -> Either String StreamReader
openStreamReader bs = do
  (sch, dicts, frames) <- readArrowStreamFBWithDicts bs
  dictPairs <- traverse (decodeDictBatch sch) dicts
  let !dictMap = Map.fromList dictPairs
  Right StreamReader
    { srSchema  = sch
    , srDictMap = dictMap
    , srFrames  = frames
    }

-- | The schema decoded at 'openStreamReader' time.
streamReaderSchema :: StreamReader -> Schema
streamReaderSchema = srSchema

-- | Pull the next record batch from the iterator. Returns:
--
--   * @Right (Just (cols, rd'))@ — a materialised batch with
--     dictionary references resolved, plus a continuation
--     reader for the remaining frames.
--   * @Right Nothing@ — the stream's EOS marker has been
--     consumed; no more batches.
--   * @Left e@ — a parse / materialisation error.
streamReaderNext
  :: StreamReader
  -> Either String (Maybe (V.Vector ColumnArray, StreamReader))
streamReaderNext rd = case srFrames rd of
  [] -> Right Nothing
  ((rb, body) : rest) -> do
    -- Honour body compression on the per-batch step. The
    -- eager 'decodeArrowStream' / 'decodeArrowFile' paths
    -- already do this via 'maybeDecompressBatch'; the
    -- iterator path used to skip it, which silently produced
    -- garbage values for compressed batches.
    (rb', body') <- maybeDecompressBatch rb body
    cols <- materializeRecordBatch (srSchema rd)
              (denormaliseBuffers (srSchema rd) rb')
              body'
    let !resolved = V.map
          (resolveDictionaryColumn (`Map.lookup` srDictMap rd)) cols
        !rd' = rd { srFrames = rest }
    Right (Just (resolved, rd'))

-- | Drain a 'StreamReader' into a list of batches. Equivalent
-- (modulo the schema bundling) to 'decodeArrowStream' but keeps
-- the streaming shape for callers that prefer to iterate.
streamReaderToList
  :: StreamReader
  -> Either String [V.Vector ColumnArray]
streamReaderToList rd0 = go rd0 []
  where
    go rd acc = case streamReaderNext rd of
      Left e                      -> Left e
      Right Nothing               -> Right (reverse acc)
      Right (Just (cols, rd'))    -> go rd' (cols : acc)

-- | Lift a 'StreamReader' into the cross-format 'IS.Iter' shape.
-- Each 'IS.iterStep' decodes one record batch, applies body
-- decompression if needed, materialises columns, and resolves
-- dictionary references — matching 'streamReaderNext' semantics
-- without forcing the caller to thread the continuation reader
-- by hand.
streamReaderIter :: StreamReader -> IS.Iter (V.Vector ColumnArray)
streamReaderIter rd0 = IS.iterUnfold rd0 $ \r ->
  case streamReaderNext r of
    Left e                      -> Left e
    Right Nothing               -> Right Nothing
    Right (Just (cols, r'))     -> Right (Just (cols, r'))

-- ============================================================
-- Column projection
-- ============================================================

-- | Like 'streamReaderToList' but only materialises the named
-- columns. Useful when the schema has dozens of columns and the
-- caller only needs a handful.
--
-- The returned 'Schema' is the projected schema (preserving the
-- order the caller asked for) and each batch is a vector of
-- 'ColumnArray' values in the same order. Names not present in
-- the source schema produce a 'Left'.
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

-- | Iterator-shaped variant of 'streamReaderProjected'. Each
-- 'IS.iterStep' yields a vector of column arrays containing
-- /only/ the requested columns, in the requested order.
--
-- The full record batch must still be materialised at decode
-- time (Arrow IPC's body layout doesn't support per-column
-- decode without parsing the record batch metadata), but the
-- returned vector aliases only the columns the caller asked for
-- — making it a real win for downstream code that drops the
-- unused columns immediately.
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
  let !nameToIdx = Map.fromList
        [ (fieldName f, i)
        | (i, f) <- V.toList (V.indexed (arrowFields sch))
        ]
  V.fromList <$> traverse (lookupOne nameToIdx) names
  where
    lookupOne m nm = case Map.lookup nm m of
      Just i  -> Right i
      Nothing -> Left $
        "Arrow.Stream: projected column not present in source schema: "
          ++ show nm

projectSchema :: V.Vector Int -> Schema -> Schema
projectSchema idxs sch = sch
  { arrowFields = V.map (V.unsafeIndex (arrowFields sch)) idxs
  }

projectColumns :: V.Vector Int -> V.Vector ColumnArray -> V.Vector ColumnArray
projectColumns idxs cols = V.map (V.unsafeIndex cols) idxs
