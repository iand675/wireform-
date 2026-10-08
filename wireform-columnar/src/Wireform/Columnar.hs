{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Unified entry point for wireform's three columnar formats.

The per-format modules ("Arrow.Stream", "Parquet.HighLevel",
"ORC") each expose their own native surface. This
module layers a single Arrow-shaped API on top of all three:
callers pass an Arrow 'AT.Schema' + a sequence of
@'V.Vector' 'AC.ColumnArray'@ batches, pick a 'Format', and
get bytes out or bytes in.

@
import qualified Wireform.Columnar as Col

let bytes = 'encode' Col.Arrow   Col.'defaultWriteOptions' sch batches
    -- or: 'encode' Col.Parquet Col.'defaultWriteOptions' sch batches
    -- or: 'encode' Col.ORC     Col.'defaultWriteOptions' sch batches

case 'decode' Col.Arrow Col.'defaultReadOptions' bytes of
  Right (sch', batches') -> ...
  Left  err              -> ...
@

Every format accepts the same inputs (Arrow schema + column
batches) and every format returns the same outputs (schema +
column batches). Format-specific knobs live on the options
records — fields irrelevant to the chosen format are ignored.
The per-format modules remain the canonical home for each
format's deep feature set (Parquet's bloom filters + page
indexes, ORC's stripe encryption, Arrow's streaming reader);
this facade handles the 80% case where you want to pick a
wire format and move on.
-}
module Wireform.Columnar (
  -- * Format selection
  Format (..),

  -- * Encoding
  encode,
  WriteOptions (..),
  defaultWriteOptions,

  -- * Decoding
  decode,
  decodeIter,
  decodeProjectedIter,
  decodeFilteredIter,
  decodeProjectedFilteredIter,
  decodeSchema,
  ReadOptions (..),
  defaultReadOptions,

  -- * Datasets (multi-file)
  decodeDatasetIter,
  decodeDatasetProjectedIter,
  decodeDatasetRowSlicedIter,
  decodeHeterogeneousDatasetIter,
  decodePartitionedDataset,
  parsePartitionPath,
  PartitionValue (..),

  -- * Records (via 'Arrow.Record.Table')

  {- | One-call helpers that lift a 'ArR.Table'-described record
  type all the way to / from a columnar wire format. Equivalent
  to @'encode' fmt opts schema [cols]@ / the inverse, but lets
  callers work in Haskell value space end-to-end.
  -}
  encodeRecords,
  decodeRecords,
  decodeRecordsIter,

  -- * Streaming primitives
  module Columnar.Stream,

  -- * Per-format passthroughs

  {- | When callers need format-specific knobs beyond what the
  unified options record exposes, drop down to the per-format
  modules. 'encode' / 'decode' are deliberately lossy in
  exchange for a uniform surface.
  -}
  module Arrow.Stream,
  module Parquet.HighLevel,
  module ORC,
) where

import Arrow.Column qualified as AC
import Arrow.Record qualified as ArR
import Arrow.Stream hiding (
  WriteOptions,
  decodeArrowFile,
  decodeArrowStream,
  defaultWriteOptions,
  encodeArrowFile,
  encodeArrowStream,
 )
import Arrow.Stream qualified as Arrow
import Arrow.Types qualified as AT
import Columnar.Stream
import Columnar.Stream qualified as IS
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64)
import ORC hiding (
  WriteOptions,
  decodeORC,
  defaultWriteOptions,
  encodeORC,
 )
import ORC qualified
import ORC.Arrow qualified as OArrow
import ORC.Read qualified as ORead
import ORC.Stripe qualified as OStripe
import Parquet.Arrow qualified as PArrow
import Parquet.HighLevel hiding (
  ReadOptions,
  WriteOptions,
  decodeParquet,
  defaultReadOptions,
  defaultWriteOptions,
  encodeParquet,
  encodeParquetNested,
 )
import Parquet.HighLevel qualified as Parquet
import Parquet.Predicate qualified as Pred


-- ============================================================
-- Format selection
-- ============================================================

-- | Which columnar format 'encode' / 'decode' should use.
data Format
  = {- | Apache Arrow IPC /stream/ format (pyarrow's @ipc.new_stream@
    shape). Produces a contiguous stream frame; read with
    'Arrow.Stream.decodeArrowStream'.
    -}
    Arrow
  | {- | Apache Arrow IPC /file/ format (@ARROW1@ sentinel +
    Footer block indexes). Seek-friendly; read with
    'Arrow.Stream.decodeArrowFile'.
    -}
    ArrowFile
  | {- | Apache Parquet. Uses 'Parquet.Arrow.arrowToParquet' to
    lower the input batches to flat Parquet columns. Nested
    types (struct / list / map) fall through to an error —
    for those use 'Parquet.HighLevel.encodeParquetNested'
    directly.
    -}
    Parquet
  | {- | Apache ORC. Uses 'ORC.Arrow.arrowToORC' to lower the
    input batches; each batch becomes one stripe.
    -}
    ORC
  deriving (Show, Eq, Ord, Enum, Bounded)


-- ============================================================
-- Write options
-- ============================================================

{- | Unified writer configuration. Every field is format-specific;
the writer silently ignores fields that don't apply to the
chosen 'Format'.

@
let opts = 'defaultWriteOptions'
            { 'arrowWrite'   = 'Arrow.Stream.defaultWriteOptions'
                                  { writeBodyCompression = Just BodyZstd }
            , 'parquetWrite' = 'Parquet.HighLevel.defaultWriteOptions'
                                  { writeCompression = ZSTD }
            }
'encode' Arrow opts sch batches
@
-}
data WriteOptions = WriteOptions
  { arrowWrite :: !Arrow.WriteOptions
  {- ^ Used when 'Format' is 'Arrow' or 'ArrowFile'. Body
  compression, dictionary-handling strategy.
  -}
  , parquetWrite :: !Parquet.WriteOptions
  {- ^ Used when 'Format' is 'Parquet'. Compression codec,
  page version, page index, per-column encryption, footer
  encryption, bloom filters.
  -}
  , orcWrite :: !ORC.WriteOptions
  -- ^ Used when 'Format' is 'ORC'. Stripe encryption plan.
  }


-- | Format-appropriate sensible defaults.
defaultWriteOptions :: WriteOptions
defaultWriteOptions =
  WriteOptions
    { arrowWrite = Arrow.defaultWriteOptions
    , parquetWrite = Parquet.defaultWriteOptions
    , orcWrite = ORC.defaultWriteOptions
    }


-- ============================================================
-- Read options
-- ============================================================

{- | Unified reader configuration. Currently only Parquet has
caller-visible read-time knobs (footer decryption); Arrow and
ORC readers self-configure from the file header.
-}
newtype ReadOptions = ReadOptions
  { parquetRead :: Parquet.ReadOptions
  }


{- | Empty/default reader options (expects plaintext Parquet
footers).
-}
defaultReadOptions :: ReadOptions
defaultReadOptions =
  ReadOptions
    { parquetRead = Parquet.defaultReadOptions
    }


-- ============================================================
-- Encode
-- ============================================================

{- | Encode an Arrow schema + a list of column batches into a
bytestring in the chosen columnar format. The Parquet / ORC
paths delegate to 'Parquet.Arrow.arrowToParquet' /
'ORC.Arrow.arrowToORC' for the Arrow-to-format lowering, so the
bridges' shape restrictions apply (see the per-format module
docs for what's currently supported).

Returns 'Left' if the format-specific bridge can't represent
the input (e.g. nested types in the Parquet flat path,
unsupported Arrow types in ORC), or, for the Arrow formats, when a
batch does not fit the schema or a dictionary cannot be written
(see 'Arrow.encodeArrowStream').
-}
encode
  :: Format
  -> WriteOptions
  -> AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String ByteString
encode fmt opts sch batches = case fmt of
  Arrow -> Arrow.encodeArrowStream (arrowWrite opts) sch batches
  ArrowFile -> Arrow.encodeArrowFile (arrowWrite opts) sch batches
  Parquet
    -- Any nullable column in the schema? Route through the
    -- mixed writer so nulls round-trip via definition-level
    -- streams. Otherwise use the fast all-required path which
    -- supports compression + page indexes + encryption.
    | anyNullable sch -> do
        (pSchema, pRgs) <- PArrow.arrowToParquetMixed sch batches
        Right (Parquet.encodeParquetMixed (parquetWrite opts) pSchema pRgs)
    | otherwise -> do
        (pSchema, pRgs) <- PArrow.arrowToParquet sch batches
        Right (Parquet.encodeParquet (parquetWrite opts) pSchema pRgs)
  ORC -> do
    (types, stripesWithRows) <- OArrow.arrowToORC sch batches
    ORC.encodeORC (orcWrite opts) types stripesWithRows


-- ============================================================
-- Decode
-- ============================================================

{- | Decode bytes in the given format back into an Arrow schema +
column batches. The Parquet / ORC paths reconstruct the Arrow
schema from the file's logical/converted-type annotations via
'Parquet.Arrow.parquetFileArrowSchema' / the ORC type table;
all row groups / stripes are materialised eagerly.

For streaming / incremental reads of Arrow files, use
'Arrow.Stream.openStreamReader' directly; for lazy Parquet row
group iteration use 'Parquet.Arrow.streamRowGroups'.
-}
decode
  :: Format
  -> ReadOptions
  -> ByteString
  -> Either String (AT.Schema, [V.Vector AC.ColumnArray])
decode fmt opts bs = case fmt of
  Arrow -> Arrow.decodeArrowStream bs
  ArrowFile -> Arrow.decodeArrowFile bs
  Parquet -> do
    pf <- Parquet.decodeParquet (parquetRead opts) bs
    let !sch = PArrow.parquetFileArrowSchema pf
    batches <-
      mapM
        ( \i -> case PArrow.parquetRowGroupToArrow sch pf i of
            Right cols -> Right cols
            Left err -> Left (show err)
        )
        [0 .. PArrow.numRowGroups pf - 1]
    Right (sch, batches)
  ORC -> do
    footer <- ORC.decodeORC bs
    orcFile <- ORead.loadORCFile bs
    sch <- orcFooterToArrowSchemaWithNullability orcFile footer
    let !numStripes = V.length (orcStripes footer)
    batches <-
      mapM
        (OArrow.orcStripeToArrow sch bs footer)
        [0 .. numStripes - 1]
    Right (sch, batches)


{- | Does the schema have at least one nullable leaf? Used to
pick between the required-only and mixed Parquet writer
paths.
-}
anyNullable :: AT.Schema -> Bool
anyNullable = any AT.fieldNullable . V.toList . AT.arrowFields


{- | Reconstruct an Arrow schema from an ORC footer using the
file's stripe footers to derive nullability per leaf. A
column is marked nullable iff its first stripe emitted a
@PRESENT@ stream for that column id — which is the exact
signal the bridge writer leaves behind for nullable Arrow
inputs.

ORC's file-level footer doesn't itself carry a per-column
nullability flag (every ORC column is implicitly nullable),
so the PRESENT-stream heuristic is the faithful inverse of
the bridge's encoder.
-}
orcFooterToArrowSchemaWithNullability
  :: ORead.ORCFile -> ORCFooter -> Either String AT.Schema
orcFooterToArrowSchemaWithNullability orcFile footer = do
  let !types = orcTypes footer
      !root = V.head types
      !subs = otSubtypes root
      !names = otFieldNames root
  -- Probe the first stripe for PRESENT streams; ORC writers
  -- emit PRESENT consistently across stripes, so stripe 0 is a
  -- reliable oracle.
  presentCols <- case V.length (orcStripes footer) of
    0 -> Right mempty
    _ -> do
      sf <- ORead.loadStripeFooter orcFile 0
      let pres =
            [ OStripe.stColumn s
            | s <- V.toList (OStripe.sfStreams sf)
            , OStripe.stKind s == orcPresentKind
            ]
      Right pres
  let !children =
        V.zipWith
          ( \name subIdx ->
              let !leafType = types V.! fromIntegral subIdx
                  !nullable = fromIntegral subIdx `elem` presentCols
              in AT.Field name nullable (orcKindToArrowType leafType) V.empty Nothing V.empty
          )
          names
          subs
  Right
    AT.Schema
      { AT.arrowFields = children
      , AT.arrowEndianness = AT.Little
      , AT.arrowMetadata = V.empty
      , AT.arrowFeatures = V.empty
      }
  where
    -- Stream.proto defines PRESENT as kind 0; the OT.Stream ADT
    -- keeps the raw numeric tag. Match against that directly to
    -- avoid a separate dispatch.
    orcPresentKind :: Word64
    orcPresentKind = 0


{- | Map an ORC 'ORCType' back to its Arrow flavour. Mirrors the
inverse of 'ORC.Arrow.arrowFieldToORCType'; falls back to
'AT.ABinary' for kinds the bridge doesn't handle yet
(callers that care drop down to ORC directly).
-}
orcKindToArrowType :: ORCType -> AT.ArrowType
orcKindToArrowType ot = case otKind ot of
  TKBoolean -> AT.ABool
  TKByte -> AT.AInt 8 True
  TKShort -> AT.AInt 16 True
  TKInt -> AT.AInt 32 True
  TKLong -> AT.AInt 64 True
  TKFloat -> AT.AFloatingPoint AT.Single
  TKDouble -> AT.AFloatingPoint AT.DoublePrecision
  TKString -> AT.AUtf8
  TKBinary -> AT.ABinary
  TKDate -> AT.ADate AT.DateDay
  TKTimestamp -> AT.ATimestamp AT.Microsecond Nothing
  TKTimestampInstant -> AT.ATimestamp AT.Microsecond (Just "UTC")
  TKDecimal -> AT.ADecimal 38 18
  _ -> AT.ABinary


-- ============================================================
-- Record-level helpers
-- ============================================================

{- | Encode a vector of Haskell records as a one-batch file in
the chosen columnar format. Schema comes from the 'ArR.Table'
instance.

@
bytes \<- either fail pure
      $ 'encodeRecords' 'Arrow' 'defaultWriteOptions' tradeTable trades
@
-}
encodeRecords
  :: Format
  -> WriteOptions
  -> ArR.Table r
  -> V.Vector r
  -> Either String ByteString
encodeRecords fmt opts tbl rs =
  let (sch, cols) = ArR.encodeTable tbl rs
  in encode fmt opts sch [cols]


{- | Inverse of 'encodeRecords'. Decodes the wire bytes, then
dispatches each row group / batch through the 'ArR.Table'
instance to reconstruct a flat record vector. Multi-batch
files are concatenated.
-}
decodeRecords
  :: Format
  -> ReadOptions
  -> ArR.Table r
  -> ByteString
  -> Either String (V.Vector r)
decodeRecords fmt opts tbl bs = do
  let !needed = ArR.tableRequiredColumns tbl
  (sch, batches) <-
    if null needed
      then decode fmt opts bs
      else do
        (narrowSch, it) <- decodeProjectedIter fmt opts needed bs
        xs <- IS.iterToList it
        Right (narrowSch, xs)
  perBatch <- traverse (ArR.decodeTable tbl sch) batches
  Right (V.concat perBatch)


-- ============================================================
-- Streaming decode
-- ============================================================

{- | Inspect the file's schema /without/ materialising any
batches. Useful for projection planning before the streaming
decode kicks off.
-}
decodeSchema
  :: Format
  -> ReadOptions
  -> ByteString
  -> Either String AT.Schema
decodeSchema fmt opts bs = case fmt of
  Arrow -> fst <$> Arrow.decodeArrowStream bs
  ArrowFile -> fst <$> Arrow.decodeArrowFile bs
  Parquet -> do
    pf <- Parquet.decodeParquet (parquetRead opts) bs
    Right (PArrow.parquetFileArrowSchema pf)
  ORC -> do
    footer <- ORC.decodeORC bs
    orcFile <- ORead.loadORCFile bs
    orcFooterToArrowSchemaWithNullability orcFile footer


{- | Decode bytes incrementally, yielding one record batch / row
group / stripe per 'IS.iterStep'.

Unlike 'decode', the iterator does not materialise every
batch up front; consumers can fold over it, take a prefix,
short-circuit on a predicate, etc. Errors halt the iterator
at the failing step.

The schema is returned alongside the iterator (it always
comes from a single up-front parse — for Arrow IPC streams
the first message; for Parquet / ORC the file footer).
-}
decodeIter
  :: Format
  -> ReadOptions
  -> ByteString
  -> Either String (AT.Schema, IS.Iter (V.Vector AC.ColumnArray))
decodeIter fmt opts bs = case fmt of
  Arrow -> do
    rd <- Arrow.openStreamReader bs
    Right (Arrow.streamReaderSchema rd, Arrow.streamReaderIter rd)
  ArrowFile -> do
    -- Arrow file uses the same record-batch shape; reuse the
    -- streaming reader on the post-magic body. The current
    -- 'Arrow.decodeArrowFile' eagerly reads everything, so the
    -- iterator below shares the same per-batch decode but
    -- avoids materialising the list spine.
    (sch, batches) <- Arrow.decodeArrowFile bs
    Right (sch, IS.iterFromList batches)
  Parquet -> do
    pf <- Parquet.decodeParquet (parquetRead opts) bs
    let !sch = PArrow.parquetFileArrowSchema pf
    Right (sch, PArrow.streamRowGroupsIter sch pf)
  ORC -> do
    footer <- ORC.decodeORC bs
    orcFile <- ORead.loadORCFile bs
    sch <- orcFooterToArrowSchemaWithNullability orcFile footer
    Right (sch, OArrow.streamStripesIter sch bs footer)


{- | Like 'decodeIter' but only decodes the named columns of
each batch / row group / stripe. The resulting schema is the
projected schema (preserving the order of @names@).

Names absent from the file's schema cause the iterator to
fail at the first step.
-}
decodeProjectedIter
  :: Format
  -> ReadOptions
  -> [Text]
  -> ByteString
  -> Either String (AT.Schema, IS.Iter (V.Vector AC.ColumnArray))
decodeProjectedIter fmt opts names bs = do
  fullSch <- decodeSchema fmt opts bs
  narrow <- projectFieldsByName names fullSch
  case fmt of
    Arrow -> do
      rd <- Arrow.openStreamReader bs
      Right (narrow, Arrow.streamReaderProjectedIter names rd)
    ArrowFile -> do
      (_, batches) <- Arrow.decodeArrowFile bs
      let projectIdxs = projectionIndices names fullSch
          pickCols cols = V.map (V.unsafeIndex cols) projectIdxs
      Right (narrow, IS.iterMap pickCols (IS.iterFromList batches))
    Parquet -> do
      pf <- Parquet.decodeParquet (parquetRead opts) bs
      Right (narrow, PArrow.streamRowGroupsProjectedIter fullSch names pf)
    ORC -> do
      footer <- ORC.decodeORC bs
      Right (narrow, OArrow.streamStripesProjectedIter fullSch names bs footer)


{- | Iterator-shaped variant of 'decodeIter' with predicate
pushdown. For Parquet, row groups whose statistics prove
the predicate matches no rows are dropped before any column
decoding happens. For ORC the same applies at the stripe
level. Arrow IPC has no per-batch statistics so the
predicate is held for downstream filtering and the dropped
count is always 0.

Returns the schema, the iterator, and a planning summary
@(totalCandidates, droppedByPredicate)@.
-}
decodeFilteredIter
  :: Format
  -> ReadOptions
  -> Pred.Predicate
  -> ByteString
  -> Either String (AT.Schema, Int, Int, IS.Iter (V.Vector AC.ColumnArray))
decodeFilteredIter fmt opts predicate bs = case fmt of
  Parquet -> do
    pf <- Parquet.decodeParquet (parquetRead opts) bs
    let !sch = PArrow.parquetFileArrowSchema pf
        (nRg, nSkip, it) =
          PArrow.streamRowGroupsFilteredIter sch predicate pf
    Right (sch, nRg, nSkip, it)
  ORC -> do
    footer <- ORC.decodeORC bs
    orcFile <- ORead.loadORCFile bs
    sch <- orcFooterToArrowSchemaWithNullability orcFile footer
    let (nStripes, nSkip, it) =
          OArrow.streamStripesFilteredIter sch predicate bs footer
    Right (sch, nStripes, nSkip, it)
  _ -> do
    -- Arrow IPC stream / file have no per-batch stats; we
    -- decode normally and let the caller filter rows.
    (sch, it) <- decodeIter fmt opts bs
    Right (sch, 0, 0, it)


{- | Combination of 'decodeFilteredIter' and
'decodeProjectedIter'.
-}
decodeProjectedFilteredIter
  :: Format
  -> ReadOptions
  -> [Text]
  -> Pred.Predicate
  -> ByteString
  -> Either String (AT.Schema, Int, Int, IS.Iter (V.Vector AC.ColumnArray))
decodeProjectedFilteredIter fmt opts names predicate bs = case fmt of
  Parquet -> do
    pf <- Parquet.decodeParquet (parquetRead opts) bs
    let !sch = PArrow.parquetFileArrowSchema pf
    narrow <- projectFieldsByName names sch
    (nRg, nSkip, it) <-
      PArrow.streamRowGroupsProjectedFilteredIter sch names predicate pf
    Right (narrow, nRg, nSkip, it)
  ORC -> do
    footer <- ORC.decodeORC bs
    orcFile <- ORead.loadORCFile bs
    sch <- orcFooterToArrowSchemaWithNullability orcFile footer
    (nStripes, nSkip, it) <-
      OArrow.streamStripesProjectedFilteredIter sch names predicate bs footer
    narrow <- projectFieldsByName names sch
    Right (narrow, nStripes, nSkip, it)
  _ -> do
    (narrow, it) <- decodeProjectedIter fmt opts names bs
    Right (narrow, 0, 0, it)


{- | Iterator-shaped 'decodeRecords'. Each step yields a
@V.Vector r@ for one batch / row group / stripe. Useful for
pipelines that want to emit results downstream without
buffering the full file.
-}
decodeRecordsIter
  :: Format
  -> ReadOptions
  -> ArR.Table r
  -> ByteString
  -> Either String (IS.Iter (V.Vector r))
decodeRecordsIter fmt opts tbl bs = do
  let !needed = ArR.tableRequiredColumns tbl
  (sch, batchIter) <-
    if null needed
      then decodeIter fmt opts bs
      else decodeProjectedIter fmt opts needed bs
  Right $ IS.iterMapM (ArR.decodeTable tbl sch) batchIter


-- ============================================================
-- Helpers shared by the projection paths.
-- ============================================================

{- | Build the index vector that maps @names@ to positions in the
supplied schema. Names not present produce an error.
-}
projectionIndices :: [Text] -> AT.Schema -> V.Vector Int
projectionIndices names sch =
  let !nameToIdx =
        [ (AT.fieldName f, i)
        | (i, f) <- V.toList (V.indexed (AT.arrowFields sch))
        ]
      lookupOne nm = case lookup nm nameToIdx of
        Just i -> i
        -- The caller has already validated the names via
        -- projectFieldsByName; an unsafe lookup here is a bug
        -- (so error rather than threading Either through).
        Nothing ->
          error $
            "Wireform.Columnar.projectionIndices: "
              ++ show nm
              ++ " missing — did you call projectFieldsByName first?"
  in V.fromList (map lookupOne names)


projectFieldsByName :: [Text] -> AT.Schema -> Either String AT.Schema
projectFieldsByName names sch =
  let !fields = AT.arrowFields sch
      pairs = [(AT.fieldName f, f) | f <- V.toList fields]
      pickOne nm = case lookup nm pairs of
        Just f -> Right f
        Nothing ->
          Left $
            "Wireform.Columnar: projected column "
              ++ show nm
              ++ " not present in source schema"
  in do
       fs <- traverse pickOne names
       Right sch {AT.arrowFields = V.fromList fs}


-- ============================================================
-- Multi-file dataset readers
-- ============================================================

{- | Iterate over the batches of every file in a (homogeneous)
dataset. The first file decides the schema; subsequent files
must use the same one or the iterator yields a 'Left' at the
step where the mismatch is discovered.

Files are decoded /sequentially/ in the order they appear in
the input list. For concurrent decoding lift the result to
'IS.IterIO' (via 'IS.iterIOFromIter') and pass it through
'IS.iterIOPrefetch' or 'IS.iterParallelMap'.

Useful for partitioned tables: pass the file paths from one
partition (or a glob), let the iterator stream through every
batch under one schema.
-}
decodeDatasetIter
  :: Format
  -> ReadOptions
  -> [(FilePath, ByteString)]
  -- ^ named bytes, one per file
  -> Either String (AT.Schema, IS.Iter (V.Vector AC.ColumnArray))
decodeDatasetIter _ _ [] =
  Right (AT.Schema V.empty AT.Little V.empty V.empty, IS.iterEmpty)
decodeDatasetIter fmt opts ((firstName, firstBs) : rest) = do
  (sch, firstIt) <- decodeIter fmt opts firstBs
  let _ = firstName
      -- Defer per-file decode via iterConcat over an outer
      -- iterator of inner iterators: each file is decoded only
      -- when the previous one is exhausted.
      outer :: IS.Iter (IS.Iter (V.Vector AC.ColumnArray))
      outer = IS.iterFromIndexed (length rest) $ \i ->
        let (name, bs) = rest !! i
        in case decodeIter fmt opts bs of
             Left e -> Left (name ++ ": " ++ e)
             Right (sch', it) ->
               if sch' /= sch
                 then
                   Left $
                     name ++ ": schema mismatch with first file in dataset"
                 else Right it
  Right (sch, IS.iterAppend firstIt (IS.iterConcat outer))


{- | Like 'decodeDatasetIter' but only materialises the named
columns out of every file.
-}
decodeDatasetProjectedIter
  :: Format
  -> ReadOptions
  -> [Text]
  -> [(FilePath, ByteString)]
  -> Either String (AT.Schema, IS.Iter (V.Vector AC.ColumnArray))
decodeDatasetProjectedIter _ _ _ [] =
  Right (AT.Schema V.empty AT.Little V.empty V.empty, IS.iterEmpty)
decodeDatasetProjectedIter fmt opts names ((firstName, firstBs) : rest) = do
  (firstSch, firstIt) <- decodeProjectedIter fmt opts names firstBs
  let _ = firstName
  let outer :: IS.Iter (IS.Iter (V.Vector AC.ColumnArray))
      outer = IS.iterFromIndexed (length rest) $ \i ->
        let (name, bs) = rest !! i
        in case decodeProjectedIter fmt opts names bs of
             Left e -> Left (name ++ ": " ++ e)
             Right (sch', it) ->
               if sch' /= firstSch
                 then
                   Left $
                     name ++ ": schema mismatch with first file in projected dataset"
                 else Right it
  Right (firstSch, IS.iterAppend firstIt (IS.iterConcat outer))


{- | Iterator-shaped slice: drop the first @offset@ rows (across
/all/ files in the dataset, summed) then take @len@ rows. The
per-batch slicing uses 'AC.sliceColumnArray' so the wide-batch
boundary cases don't pull whole batches into memory just to
discard them.
-}
decodeDatasetRowSlicedIter
  :: Format
  -> ReadOptions
  -> Int
  -- ^ rows to skip
  -> Int
  -- ^ rows to keep
  -> [(FilePath, ByteString)]
  -> Either String (AT.Schema, IS.Iter (V.Vector AC.ColumnArray))
decodeDatasetRowSlicedIter fmt opts skip keep files = do
  (sch, it) <- decodeDatasetIter fmt opts files
  let !sliced = IS.iterRowSlice batchRowCount sliceBatch skip keep it
  Right (sch, sliced)
  where
    batchRowCount cols
      | V.null cols = 0
      | otherwise = AC.columnLength (V.head cols)
    sliceBatch s l = V.map (AC.sliceColumnArray s l)


-- ============================================================
-- Cross-format / partitioned datasets
-- ============================================================

{- | Like 'decodeDatasetIter' but each file may be in a
different 'Format'. The first file establishes the schema;
subsequent files must produce structurally equivalent
schemas (under 'Arrow.Types.schemaEquivalent') or the
iterator yields a 'Left' at the boundary step.
-}
decodeHeterogeneousDatasetIter
  :: ReadOptions
  -> [(Format, FilePath, ByteString)]
  -> Either String (AT.Schema, IS.Iter (V.Vector AC.ColumnArray))
decodeHeterogeneousDatasetIter _ [] =
  Right (AT.Schema V.empty AT.Little V.empty V.empty, IS.iterEmpty)
decodeHeterogeneousDatasetIter opts ((firstFmt, firstName, firstBs) : rest) = do
  (sch, firstIt) <- decodeIter firstFmt opts firstBs
  let !firstFp = AT.schemaFingerprint sch
      outer :: IS.Iter (IS.Iter (V.Vector AC.ColumnArray))
      outer = IS.iterFromIndexed (length rest) $ \i ->
        let (fmt, name, bs) = rest !! i
        in case decodeIter fmt opts bs of
             Left e -> Left (name ++ ": " ++ e)
             Right (sch', it) ->
               -- Use both schemaEquivalent (the structural
               -- check) and schemaFingerprint (the byte-level
               -- check that includes Arrow-type details
               -- schemaEquivalent's per-field show happens to
               -- normalise identically). If they disagree the
               -- mismatch error includes both fingerprints to
               -- help the caller diagnose which field differs.
               let !otherFp = AT.schemaFingerprint sch'
               in if AT.schemaEquivalent sch' sch && otherFp == firstFp
                    then Right it
                    else
                      Left $
                        name
                          ++ ": schema mismatch with first file ("
                          ++ firstName
                          ++ "): "
                          ++ "first fingerprint="
                          ++ show firstFp
                          ++ ", this fingerprint="
                          ++ show otherFp
  Right (sch, IS.iterAppend firstIt (IS.iterConcat outer))


-- | Parsed Hive-style partition value.
data PartitionValue
  = PVText !Text
  | PVInteger !Int64
  deriving (Show, Eq)


{- | Parse a Hive-style partitioned path into its
@(partition_key, partition_value)@ pairs.

@
parsePartitionPath \"region=us-east/year=2024/data.parquet\"
  == [("region", PVText \"us-east\"), ("year", PVInteger 2024)]
@

Components without an @=@ are skipped (treated as ordinary
directory names). Numeric values that parse as 'Int64' use
'PVInteger'; everything else stays 'PVText'.
-}
parsePartitionPath :: FilePath -> [(Text, PartitionValue)]
parsePartitionPath path =
  let parts = filter (not . null) (splitOnSlash path)
  in [ (T.pack k, parseValue v)
     | part <- parts
     , (k, v) <- splitKV part
     ]
  where
    splitOnSlash :: String -> [String]
    splitOnSlash = foldr step [""]
      where
        step '/' acc = "" : acc
        step c (x : xs) = (c : x) : xs
        step _ [] = [] -- unreachable
    splitKV :: String -> [(String, String)]
    splitKV s = case break (== '=') s of
      (_, []) -> []
      (k, '=' : v) -> [(k, v)]
      _ -> []

    parseValue :: String -> PartitionValue
    parseValue s = case reads s :: [(Int64, String)] of
      [(n, "")] -> PVInteger n
      _ -> PVText (T.pack s)


{- | Read a partitioned dataset, dropping every file whose
partition values are excluded by the supplied keep-predicate.
The keep-predicate sees the parsed partition map for each
file; returning 'False' elides the file before it's even
decoded.

@
decodePartitionedDataset Parquet defaultReadOptions
  (\\parts -> lookup \"region\" parts == Just (PVText \"us-east\"))
  files
@
-}
decodePartitionedDataset
  :: Format
  -> ReadOptions
  -> ([(Text, PartitionValue)] -> Bool)
  -> [(FilePath, ByteString)]
  -> Either String (AT.Schema, IS.Iter (V.Vector AC.ColumnArray))
decodePartitionedDataset fmt opts keep files =
  let !surviving = filter (keep . parsePartitionPath . fst) files
  in decodeDatasetIter fmt opts surviving
