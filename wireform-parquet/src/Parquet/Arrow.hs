{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Arrow ↔ Parquet column-data bridge.

Lets callers keep a single in-memory representation
('Arrow.Column.ColumnArray') and choose Parquet at the wire-
format level, mirroring how
@pyarrow.parquet.write_table(table, path)@ works in Python: a
'pa.Table' goes in, Parquet bytes come out.

@
-- Arrow → Parquet
let !(schema, rowGroups) = 'arrowToParquet' arrowSchema arrowBatches
    bytes                = 'Parquet.HighLevel.encodeParquet'
                             'Parquet.HighLevel.defaultWriteOptions'
                             schema rowGroups

-- Parquet → Arrow (one row group at a time)
pf <- 'Parquet.HighLevel.decodeParquet' bytes
batch <- 'parquetRowGroupToArrow' arrowSchema pf 0
@

The bridge currently covers the flat-primitive shape Parquet's
writer ('Parquet.Write.ColumnData') natively supports: 'Int8',
'Int16', 'Int32', 'Int64', 'UInt8'..'UInt64', 'Float', 'Double',
'Bool', 'Utf8', 'Binary', plus their nullable variants. Nested
columns (struct / list / map / union / dictionary / view / REE)
aren't in Parquet's flat data plane, so they fall through to a
Left at translation time. Support for nested shredding via
"Parquet.Nested" is a separate item.
-}
module Parquet.Arrow (
  -- * Arrow → Parquet (flat, required + nullable)
  arrowToParquet,
  arrowToParquetMixed,
  columnArrayToColumnData,
  columnArrayToParquetColumn,

  -- * Arrow → Parquet (nested, via Parquet.Nested)
  arrowFieldToNestedSchema,
  columnArrayToNestedRows,

  -- * Parquet → Arrow
  parquetRowGroupToArrow,
  parquetRowGroupToArrowProjected,
  readParquetColumn,
  parquetFileArrowSchema,
  ProjectionError (..),

  -- * Streaming reader (one row group at a time)
  streamRowGroups,
  streamRowGroupsIter,
  streamRowGroupsProjectedIter,
  streamRowGroupsFilteredIter,
  streamRowGroupsProjectedFilteredIter,
  numRowGroups,

  -- * Page-index-driven page skipping
  readParquetColumnWithPagePruning,
) where

import Arrow.Column qualified as AC
import Arrow.Types qualified as AT
import Columnar.Stream qualified as IS
import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Primitive.ByteArray (copyByteArrayToAddr)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, sizeOf)
import Parquet.Nested qualified as PN
import Parquet.PageIndex qualified as PI
import Parquet.Predicate qualified as Pred
import Parquet.Read qualified as PR
import Parquet.Types qualified as P
import Parquet.Write qualified as PW
import System.IO.Unsafe (unsafeDupablePerformIO)


-- ============================================================
-- Arrow → Parquet
-- ============================================================

{- | Lower an Arrow schema + a sequence of column-major batches
to the inputs 'Parquet.HighLevel.encodeParquet' expects.

Each Arrow batch becomes one Parquet row group. Returns 'Left'
if any column type isn't representable in Parquet's flat data
plane (struct / list / dictionary / view / REE — see the
module docs for the supported subset).
-}
arrowToParquet
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String (V.Vector P.SchemaElement, [V.Vector PW.ColumnData])
arrowToParquet sch batches = do
  let !leafFields = arrowFieldsToLeaves (AT.arrowFields sch)
      !rootElem =
        P.SchemaElement
          { P.seName = "schema"
          , P.seRepetition = Nothing
          , P.seType = Nothing
          , P.seNumChildren = Just (fromIntegral (V.length leafFields))
          , P.seConvertedType = Nothing
          , P.seLogicalType = Nothing
          , P.seFieldId = Nothing
          }
  schemaElems <- V.mapM arrowFieldToSchemaElement leafFields
  let !pSchema = V.cons rootElem schemaElems
  rgData <- mapM (V.mapM columnArrayToColumnData) batches
  Right (pSchema, rgData)


{- | Like 'arrowToParquet' but returns 'PW.ParquetColumn' values
so nullable Arrow columns lower to 'PW.PCOptional' instead of
being dropped through 'optionalColumnPresentValues'. Pair with
'Parquet.Write.buildParquetFileMixed' to write a Parquet file
that actually carries the nulls; 'Parquet.HighLevel.encodeParquet'
auto-routes through this path when any input column is
nullable.
-}
arrowToParquetMixed
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String (V.Vector P.SchemaElement, [V.Vector PW.ParquetColumn])
arrowToParquetMixed sch batches = do
  let !leafFields = arrowFieldsToLeaves (AT.arrowFields sch)
      !rootElem =
        P.SchemaElement
          { P.seName = "schema"
          , P.seRepetition = Nothing
          , P.seType = Nothing
          , P.seNumChildren = Just (fromIntegral (V.length leafFields))
          , P.seConvertedType = Nothing
          , P.seLogicalType = Nothing
          , P.seFieldId = Nothing
          }
  schemaElems <- V.mapM arrowFieldToSchemaElement leafFields
  let !pSchema = V.cons rootElem schemaElems
  rgData <- mapM (V.zipWithM columnArrayToParquetColumn leafFields) batches
  Right (pSchema, rgData)


{- | Dispatch a single Arrow column onto the 'PW.ParquetColumn'
sum, driven by the schema field: a nullable field maps to
'PW.PCOptional' (definition levels carry the nulls), a required
field reuses 'columnArrayToColumnData' and wraps the result in
'PW.PCRequired'. A column holding nulls under a required field is
a 'Left': the Parquet schema element would claim every value is
present.
-}
columnArrayToParquetColumn
  :: AT.Field -> AC.ColumnArray -> Either String PW.ParquetColumn
columnArrayToParquetColumn fld col
  | not (AT.fieldNullable fld) =
      if AC.nullCount col > 0
        then
          Left $
            "Parquet.Arrow: column "
              <> show (AT.fieldName fld)
              <> " has "
              <> show (AC.nullCount col)
              <> " nulls but its field is not nullable"
        else PW.PCRequired <$> columnArrayToColumnData col
  | otherwise = PW.PCOptional <$> columnArrayToOptionalColumn col


{- | Lower one Arrow column to Parquet's nullable column shape.
Narrow and unsigned integers widen to INT32 / INT64 as in
'columnArrayToColumnData'; temporal columns use their integer
payload. Parquet's optional writer takes boxed @Maybe@ vectors, so
this is one boxing pass per column.
-}
columnArrayToOptionalColumn
  :: AC.ColumnArray -> Either String PW.OptionalColumn
columnArrayToOptionalColumn = \case
  AC.ColInt8 mv xs -> Right $ PW.OptInt32 (maybeVector fromIntegral mv xs)
  AC.ColInt16 mv xs -> Right $ PW.OptInt32 (maybeVector fromIntegral mv xs)
  AC.ColInt32 mv xs -> Right $ PW.OptInt32 (maybeVector id mv xs)
  AC.ColInt64 mv xs -> Right $ PW.OptInt64 (maybeVector id mv xs)
  AC.ColUInt8 mv xs -> Right $ PW.OptInt32 (maybeVector (fromIntegral :: Word8 -> Int32) mv xs)
  AC.ColUInt16 mv xs -> Right $ PW.OptInt32 (maybeVector (fromIntegral :: Word16 -> Int32) mv xs)
  AC.ColUInt32 mv xs -> Right $ PW.OptInt32 (maybeVector (fromIntegral :: Word32 -> Int32) mv xs)
  AC.ColUInt64 mv xs -> Right $ PW.OptInt64 (maybeVector (fromIntegral :: Word64 -> Int64) mv xs)
  AC.ColFloat mv xs -> Right $ PW.OptFloat (maybeVector id mv xs)
  AC.ColDouble mv xs -> Right $ PW.OptDouble (maybeVector id mv xs)
  AC.ColDate32 mv xs -> Right $ PW.OptInt32 (maybeVector id mv xs)
  AC.ColDate64 mv xs -> Right $ PW.OptInt64 (maybeVector id mv xs)
  AC.ColTime32 mv xs -> Right $ PW.OptInt32 (maybeVector id mv xs)
  AC.ColTime64 mv xs -> Right $ PW.OptInt64 (maybeVector id mv xs)
  AC.ColTimestamp mv xs -> Right $ PW.OptInt64 (maybeVector id mv xs)
  AC.ColDuration mv xs -> Right $ PW.OptInt64 (maybeVector id mv xs)
  col
    | Just ba <- AC.asBool col ->
        Right $ PW.OptBool (V.generate (AC.columnLength col) (AC.boolArrayAt ba))
    | Just ba <- AC.asBinary col ->
        Right $ PW.OptByteArray (V.generate (AC.bytesArrayLength ba) (AC.unsafeBytesAt ba))
    | Just ba <- AC.asLargeBinary col ->
        Right $ PW.OptByteArray (V.generate (AC.bytesArrayLength ba) (AC.unsafeBytesAt ba))
    | otherwise -> Left (noFlatEquivalent col)


{- | Project the (potentially-nested) Arrow field tree to a flat
list of leaves. Today we only support flat schemas (the bridge
doesn't round-trip nested types); nested fields fall through to
'columnArrayToColumnData' which then reports a clean 'Left'.
-}
arrowFieldsToLeaves :: V.Vector AT.Field -> V.Vector AT.Field
arrowFieldsToLeaves = V.filter (V.null . AT.fieldChildren)


{- | Translate an Arrow leaf 'Field' into a Parquet
'SchemaElement'.
-}
arrowFieldToSchemaElement
  :: AT.Field -> Either String P.SchemaElement
arrowFieldToSchemaElement f = do
  pType <- case AT.fieldType f of
    AT.AInt 8 _ -> Right P.PTInt32
    AT.AInt 16 _ -> Right P.PTInt32
    AT.AInt 32 _ -> Right P.PTInt32
    AT.AInt 64 _ -> Right P.PTInt64
    AT.ABool -> Right P.PTBoolean
    AT.AFloatingPoint AT.Single -> Right P.PTFloat
    AT.AFloatingPoint AT.DoublePrecision -> Right P.PTDouble
    AT.AUtf8 -> Right P.PTByteArray
    AT.ABinary -> Right P.PTByteArray
    AT.ALargeUtf8 -> Right P.PTByteArray
    AT.ALargeBinary -> Right P.PTByteArray
    -- Temporal types: map to INT32 (Date, Time-millis) or INT64
    -- (Date64, Time-micros/nanos, Timestamp, Duration). Logical
    -- / converted types are set below.
    AT.ADate AT.DateDay -> Right P.PTInt32
    AT.ADate AT.DateMillisecond -> Right P.PTInt64
    AT.ATime _ 32 -> Right P.PTInt32
    AT.ATime _ 64 -> Right P.PTInt64
    AT.ATimestamp _ _ -> Right P.PTInt64
    AT.ADuration _ -> Right P.PTInt64
    other ->
      Left $
        "Parquet.Arrow: Arrow type "
          <> show other
          <> " has no Parquet flat-primitive equivalent"
  let !rep = if AT.fieldNullable f then P.Optional else P.Required
      !logical = case AT.fieldType f of
        AT.AUtf8 -> Just P.LTString
        AT.ALargeUtf8 -> Just P.LTString
        AT.ADate _ -> Just P.LTDate
        AT.ATime AT.Millisecond _ -> Just (P.LTTime False P.LtMillis)
        AT.ATime AT.Microsecond _ -> Just (P.LTTime False P.LtMicros)
        AT.ATime AT.Nanosecond _ -> Just (P.LTTime False P.LtNanos)
        AT.ATime AT.Second _ -> Just (P.LTTime False P.LtMillis)
        -- Parquet doesn't model second precision; widen.
        AT.ATimestamp AT.Millisecond mtz ->
          Just (P.LTTimestamp (isJustUtc mtz) P.LtMillis)
        AT.ATimestamp AT.Microsecond mtz ->
          Just (P.LTTimestamp (isJustUtc mtz) P.LtMicros)
        AT.ATimestamp AT.Nanosecond mtz ->
          Just (P.LTTimestamp (isJustUtc mtz) P.LtNanos)
        AT.ATimestamp AT.Second mtz ->
          Just (P.LTTimestamp (isJustUtc mtz) P.LtMillis)
        _ -> Nothing
      isJustUtc Nothing = False
      isJustUtc (Just _) = True
  Right
    P.SchemaElement
      { P.seName = AT.fieldName f
      , P.seRepetition = Just rep
      , P.seType = Just pType
      , P.seNumChildren = Nothing
      , P.seConvertedType = case AT.fieldType f of
          AT.AUtf8 -> Just P.CTUtf8
          AT.ALargeUtf8 -> Just P.CTUtf8
          AT.ADate _ -> Just P.CTDate
          AT.ATime _ 32 -> Just P.CTTimeMillis
          AT.ATime _ 64 -> Just P.CTTimeMicros
          AT.ATimestamp AT.Millisecond _ -> Just P.CTTimestampMillis
          AT.ATimestamp AT.Microsecond _ -> Just P.CTTimestampMicros
          _ -> Nothing
      , P.seLogicalType = logical
      , P.seFieldId = Nothing
      }


{- | Lower one Arrow column to Parquet's required 'ColumnData'.
Nulls are dropped: the result holds the present values only (the
shape Parquet's required writer takes). Use
'columnArrayToParquetColumn' to keep the nulls through definition
levels.
-}
columnArrayToColumnData
  :: AC.ColumnArray -> Either String PW.ColumnData
columnArrayToColumnData = \case
  AC.ColInt8 mv xs -> Right $ PW.ColInt32 (presentPrim fromIntegral mv xs)
  AC.ColInt16 mv xs -> Right $ PW.ColInt32 (presentPrim fromIntegral mv xs)
  AC.ColInt32 mv xs -> Right $ PW.ColInt32 (presentPrim id mv xs)
  AC.ColInt64 mv xs -> Right $ PW.ColInt64 (presentPrim id mv xs)
  AC.ColUInt8 mv xs -> Right $ PW.ColInt32 (presentPrim (fromIntegral :: Word8 -> Int32) mv xs)
  AC.ColUInt16 mv xs -> Right $ PW.ColInt32 (presentPrim (fromIntegral :: Word16 -> Int32) mv xs)
  AC.ColUInt32 mv xs -> Right $ PW.ColInt32 (presentPrim (fromIntegral :: Word32 -> Int32) mv xs)
  AC.ColUInt64 mv xs -> Right $ PW.ColInt64 (presentPrim (fromIntegral :: Word64 -> Int64) mv xs)
  AC.ColFloat mv xs -> Right $ PW.ColFloat (presentPrim id mv xs)
  AC.ColDouble mv xs -> Right $ PW.ColDouble (presentPrim id mv xs)
  -- Temporal types: lower to the natural Parquet physical type
  -- the schema element declared (Int32 for Date32 / Time32, Int64
  -- for Date64 / Time64 / Timestamp / Duration).
  AC.ColDate32 mv xs -> Right $ PW.ColInt32 (presentPrim id mv xs)
  AC.ColDate64 mv xs -> Right $ PW.ColInt64 (presentPrim id mv xs)
  AC.ColTime32 mv xs -> Right $ PW.ColInt32 (presentPrim id mv xs)
  AC.ColTime64 mv xs -> Right $ PW.ColInt64 (presentPrim id mv xs)
  AC.ColTimestamp mv xs -> Right $ PW.ColInt64 (presentPrim id mv xs)
  AC.ColDuration mv xs -> Right $ PW.ColInt64 (presentPrim id mv xs)
  col
    | Just ba <- AC.asBool col ->
        Right $ PW.ColBool (presentBoxed (AC.columnLength col) (AC.nullCount col) (AC.boolArrayAt ba))
    | Just ba <- AC.asBinary col ->
        Right $ PW.ColByteArray (presentBoxed (AC.bytesArrayLength ba) (AC.nullCount col) (AC.unsafeBytesAt ba))
    | Just ba <- AC.asLargeBinary col ->
        Right $ PW.ColByteArray (presentBoxed (AC.bytesArrayLength ba) (AC.nullCount col) (AC.unsafeBytesAt ba))
    | otherwise -> Left (noFlatEquivalent col)


noFlatEquivalent :: AC.ColumnArray -> String
noFlatEquivalent col =
  "Parquet.Arrow: Arrow column shape "
    <> AC.columnTag col
    <> " has no flat Parquet equivalent (nested types "
    <> "go through Parquet.Nested, dictionary columns "
    <> "should pre-resolve to their values column)"


{- | Present values of a fixed-width column, cast to the Parquet
physical type, in one pass (one compaction pass when the column
has nulls).
-}
presentPrim
  :: (Storable a, VP.Prim b)
  => (a -> b) -> Maybe AC.Validity -> VS.Vector a -> VP.Vector b
presentPrim f mv xs = case mv of
  Nothing -> VP.generate n (\i -> f (VS.unsafeIndex xs i))
  Just v -> runST $ do
    out <- VPM.unsafeNew (n - AC.validityNullCount v)
    let go !i !j
          | i >= n = pure ()
          | otherwise = case AC.unsafePrimAt pa i of
              Just x -> VPM.unsafeWrite out j (f x) *> go (i + 1) (j + 1)
              Nothing -> go (i + 1) j
    go 0 0
    VP.unsafeFreeze out
  where
    !n = VS.length xs
    !pa = AC.PrimArray mv xs
{-# INLINE presentPrim #-}


{- | Present values of a boxed-row view (bool, bytes): @n@ rows,
@nulls@ of them null, read through @at@.
-}
presentBoxed :: Int -> Int -> (Int -> Maybe a) -> V.Vector a
presentBoxed n nulls at = runST $ do
  out <- VM.unsafeNew (n - nulls)
  let go !i !j
        | i >= n = pure ()
        | otherwise = case at i of
            Just x -> VM.unsafeWrite out j x *> go (i + 1) (j + 1)
            Nothing -> go (i + 1) j
  go 0 0
  V.unsafeFreeze out
{-# INLINE presentBoxed #-}


-- | Boxed @Maybe@ rows of a fixed-width column (Parquet's optional writer input).
maybeVector
  :: Storable a => (a -> b) -> Maybe AC.Validity -> VS.Vector a -> V.Vector (Maybe b)
maybeVector f mv xs =
  let !pa = AC.PrimArray mv xs
  in V.generate (VS.length xs) (\i -> f <$> AC.unsafePrimAt pa i)
{-# INLINE maybeVector #-}


-- ============================================================
-- Parquet → Arrow
-- ============================================================

{- | Read columns of a Parquet row group and lift them to Arrow
column shapes, driven by the caller-supplied /target/ Arrow
schema.

Lookup is by column name (Parquet's footer carries unique leaf
names), so the target schema may be narrower than the file
(projection), in a different order than the file (reordering),
or request wider types than the file (coercion: see
'coerceColumn' below for the supported widening rules).

Error cases:

  * 'MissingColumn' — the target requests a column name that
    isn't present in the file. Callers that want null-fill
    semantics should catch this constructor and synthesize a
    null column.
  * 'IncompatibleType' — the file carries the column but the
    target's type isn't reachable by the coercion table.
-}
parquetRowGroupToArrow
  :: AT.Schema
  -- ^ Target Arrow schema
  -> PR.ParquetFile
  -> Int
  -- ^ Row group index
  -> Either ProjectionError (V.Vector AC.ColumnArray)
parquetRowGroupToArrow target pf rgIdx = do
  let !fileLeaves =
        V.filter
          (maybe False (const True) . P.seType)
          (P.fmSchema (PR.pfFooter pf))
      !nameToIdx =
        Map.fromList
          [(P.seName se, i) | (i, se) <- V.toList (V.indexed fileLeaves)]
  V.mapM
    (readOneProjected pf rgIdx fileLeaves nameToIdx)
    (AT.arrowFields target)


-- ============================================================
-- Projection / schema evolution
-- ============================================================

-- | Why a projection request couldn't be satisfied.
data ProjectionError
  = {- | The target schema references a column name that isn't
    present in the Parquet footer. A real schema-evolution
    implementation would fill this column with nulls; callers
    that want that behaviour can catch this constructor and
    synthesize a null column.
    -}
    MissingColumn !Text
  | {- | The target schema requests a type the physical Parquet
    column can't be coerced to. The bridge's coercion table is
    intentionally narrow: numeric widening (Int32 → Int64,
    Float → Double), identity reads for equal types.
    -}
    IncompatibleType !Text !AT.ArrowType
  deriving (Show, Eq)


{- | Recover the Arrow-shaped schema implied by a Parquet file's
leaf columns. Uses the Parquet converted-type / logical-type
annotations to pick the right Arrow flavour (UTF-8 vs raw
binary, Date vs Int32, Timestamp vs Int64).
-}
parquetFileArrowSchema :: PR.ParquetFile -> AT.Schema
parquetFileArrowSchema pf =
  let !leaves =
        V.filter
          (maybe False (const True) . P.seType)
          (P.fmSchema (PR.pfFooter pf))
  in AT.Schema
       { AT.arrowFields = V.map schemaElementToArrowField leaves
       , AT.arrowEndianness = AT.Little
       , AT.arrowMetadata = V.empty
       , AT.arrowFeatures = V.empty
       }


{- | Map a Parquet 'P.SchemaElement' back to an Arrow leaf
'AT.Field' using its converted-type / logical-type hints.
Mirrors the inverse of 'arrowFieldToSchemaElement'.
-}
schemaElementToArrowField :: P.SchemaElement -> AT.Field
schemaElementToArrowField se =
  let !name = P.seName se
      !nullable = case P.seRepetition se of
        Just P.Optional -> True
        _ -> False
      !ty = arrowTypeFromSchemaElement se
  in AT.Field name nullable ty V.empty Nothing V.empty


arrowTypeFromSchemaElement :: P.SchemaElement -> AT.ArrowType
arrowTypeFromSchemaElement se = case P.seType se of
  Just P.PTInt96 ->
    -- INT96 is the legacy 12-byte timestamp (Hive / impala /
    -- older parquet writers). Expose as a 12-byte
    -- fixed-size-binary; downstream callers that know the
    -- (julian_day, nanos) interpretation can decode further.
    AT.AFixedSizeBinary 12
  Just P.PTFixedLenByteArray ->
    -- The concrete byte width lives in schema's type_length
    -- field, which we don't currently surface on
    -- 'P.SchemaElement'. Reasonable default is 16 (matches
    -- UUID / decimal128 columns that are the common case);
    -- callers needing the exact width drop into
    -- 'PR.readPlainFixedLenByteArrayColumnChunk' directly.
    case P.seLogicalType se of
      Just P.LTFloat16 -> AT.AFloatingPoint AT.Half
      Just P.LTUUID -> AT.AFixedSizeBinary 16
      _ -> AT.AFixedSizeBinary 16
  Just P.PTBoolean -> AT.ABool
  Just P.PTInt32 -> case (P.seLogicalType se, P.seConvertedType se) of
    (Just P.LTDate, _) -> AT.ADate AT.DateDay
    (Just (P.LTTime _ unit), _)
      | Just u <- arrowTimeUnit unit -> AT.ATime u 32
    (Just (P.LTInteger w isSigned), _)
      | w <= 32 -> AT.AInt (fromIntegral w) isSigned
    (_, Just P.CTDate) -> AT.ADate AT.DateDay
    (_, Just P.CTTimeMillis) -> AT.ATime AT.Millisecond 32
    _ -> AT.AInt 32 True
  Just P.PTInt64 -> case (P.seLogicalType se, P.seConvertedType se) of
    (Just (P.LTTime _ unit), _)
      | Just u <- arrowTimeUnit unit -> AT.ATime u 64
    (Just (P.LTTimestamp adj unit), _)
      | Just u <- arrowTimeUnit unit ->
          AT.ATimestamp
            u
            (if adj then Just (T.pack "UTC") else Nothing)
    (Just (P.LTInteger 64 isSigned), _) ->
      AT.AInt 64 isSigned
    (_, Just P.CTTimeMicros) -> AT.ATime AT.Microsecond 64
    (_, Just P.CTTimestampMillis) -> AT.ATimestamp AT.Millisecond Nothing
    (_, Just P.CTTimestampMicros) -> AT.ATimestamp AT.Microsecond Nothing
    _ -> AT.AInt 64 True
  Just P.PTFloat -> AT.AFloatingPoint AT.Single
  Just P.PTDouble -> AT.AFloatingPoint AT.DoublePrecision
  Just P.PTByteArray -> case (P.seLogicalType se, P.seConvertedType se) of
    (Just P.LTString, _) -> AT.AUtf8
    -- Geometry / Geography / Variant + Json / Bson are all
    -- bytes on the wire; expose as ABinary so callers see the
    -- raw WKB / JSON / variant bytes. The LogicalType
    -- annotation survives round-trip for downstream tools.
    (Just P.LTGeometry, _) -> AT.ABinary
    (Just P.LTGeography, _) -> AT.ABinary
    (Just (P.LTVariant _), _) -> AT.ABinary
    (Just P.LTJson, _) -> AT.AUtf8
    (Just P.LTBson, _) -> AT.ABinary
    (_, Just P.CTUtf8) -> AT.AUtf8
    (_, Just P.CTJson) -> AT.AUtf8
    (_, Just P.CTBson) -> AT.ABinary
    _ -> AT.ABinary
  _ -> AT.ABinary -- FIXED_LEN_BYTE_ARRAY / Int96 fallback


arrowTimeUnit :: P.LtTimeUnit -> Maybe AT.TimeUnit
arrowTimeUnit P.LtMillis = Just AT.Millisecond
arrowTimeUnit P.LtMicros = Just AT.Microsecond
arrowTimeUnit P.LtNanos = Just AT.Nanosecond


{- | Core per-column reader used by 'parquetRowGroupToArrow'.
Looks up the column by name, reads it at the file's native
Arrow type, then coerces to the target type via 'coerceColumn'
if needed.
-}
readOneProjected
  :: PR.ParquetFile
  -> Int
  -> V.Vector P.SchemaElement
  -> Map.Map Text Int
  -> AT.Field
  -> Either ProjectionError AC.ColumnArray
readOneProjected pf rgIdx fileLeaves nameToIdx fld =
  case Map.lookup (AT.fieldName fld) nameToIdx of
    Nothing -> Left (MissingColumn (AT.fieldName fld))
    Just fileIdx ->
      case readParquetColumn pf rgIdx fileIdx fld of
        Right col -> Right col
        Left _ ->
          -- Direct read failed — load at the file's native type
          -- and coerce to the target.
          let !fileFld =
                schemaElementToArrowField
                  (V.unsafeIndex fileLeaves fileIdx)
          in case readParquetColumn pf rgIdx fileIdx fileFld of
               Left _ -> Left (IncompatibleType (AT.fieldName fld) (AT.fieldType fld))
               Right c' ->
                 coerceColumn (AT.fieldType fld) c'
                   `orLeft` IncompatibleType
                     (AT.fieldName fld)
                     (AT.fieldType fld)
  where
    orLeft (Right x) _ = Right x
    orLeft (Left _) e = Left e


{- | Best-effort column coercion for the projection path. The
coercion table is deliberately narrow: numeric widening
(Int32 → Int64, Float → Double) and identity reads. Returns
'Left' for unsupported coercions; callers route that to
'IncompatibleType'.
-}
coerceColumn :: AT.ArrowType -> AC.ColumnArray -> Either String AC.ColumnArray
coerceColumn target col = case (target, col) of
  (AT.AInt 64 True, AC.ColInt32 mv xs) ->
    AC.mkPrim AC.PInt64 mv (VS.map (fromIntegral :: Int32 -> Int64) xs)
  (AT.AInt 64 False, AC.ColInt32 mv xs) ->
    AC.mkPrim AC.PUInt64 mv (VS.map (fromIntegral :: Int32 -> Word64) xs)
  (AT.AFloatingPoint AT.DoublePrecision, AC.ColFloat mv xs) ->
    AC.mkPrim AC.PDouble mv (VS.map (realToFrac :: Float -> Double) xs)
  _ -> Left ("coerceColumn: " ++ show target ++ " <- " ++ AC.columnTag col)


{- | Read one column chunk and project it into a 'ColumnArray'.
Dispatches on the Arrow target type + nullability; falls back
to a clean 'Left' for shapes the bridge doesn't yet cover.
-}
readParquetColumn
  :: PR.ParquetFile
  -> Int
  -- ^ row-group index
  -> Int
  -- ^ column index within the row group
  -> AT.Field
  -> Either String AC.ColumnArray
readParquetColumn pf rgIdx colIdx fld = do
  chunk <- PR.columnChunkSlice pf rgIdx colIdx
  liftColumn (ChunkSource (chunkCodec pf rgIdx colIdx) chunk) fld


{- | Where a column's values come from: a whole column chunk, or
the pages of one selected by a page-index keep mask. Both go
through the generic per-page dispatchers, which handle every
encoding the spec defines for the physical type (PLAIN,
dictionary, DELTA_*, BYTE_STREAM_SPLIT) and both DATA_PAGE and
DATA_PAGE_V2.
-}
data Source
  = ChunkSource P.Compression ByteString
  | PagesSource P.Compression ByteString (V.Vector P.PageLocation) (V.Vector Bool)


{- | Decode a flat column from a 'Source' at the Arrow target
type. Required fields read the dense value stream; nullable
fields read the definition-level form. The optional readers
take (max_repetition_level, max_definition_level); for a flat
optional primitive these are (0, 1).

Fixed-width values are copied once from the decoder's primitive
vector into a pinned Arrow buffer; nullable values go through
'AC.fromMaybes' (one pass, validity built alongside). Byte arrays
are copied once into the offsets/data buffers.
-}
liftColumn :: Source -> AT.Field -> Either String AC.ColumnArray
liftColumn src fld = case AT.fieldType fld of
  AT.AInt 32 True -> int32 AC.PInt32
  AT.AInt 64 True -> int64 AC.PInt64
  AT.AFloatingPoint AT.Single
    | nullable -> AC.fromMaybes AC.PFloat <$> optFloat src
    | otherwise -> AC.primColumn AC.PFloat . primToStorable <$> reqFloat src
  AT.AFloatingPoint AT.DoublePrecision
    | nullable -> AC.fromMaybes AC.PDouble <$> optDouble src
    | otherwise -> AC.primColumn AC.PDouble . primToStorable <$> reqDouble src
  AT.ABool
    | nullable -> AC.fromMaybeBools <$> optBool src
    | otherwise -> AC.fromBools <$> reqBool src
  AT.AUtf8
    | nullable -> utf8FromBinary . AC.fromMaybeByteStrings =<< optBytes src
    | otherwise -> utf8FromBinary . AC.fromByteStrings =<< reqBytes src
  AT.ABinary
    | nullable -> AC.fromMaybeByteStrings <$> optBytes src
    | otherwise -> AC.fromByteStrings <$> reqBytes src
  -- Temporal: read the underlying int stream and tag it with the
  -- Arrow column flavour.
  AT.ADate AT.DateDay -> int32 AC.PDate32
  AT.ADate AT.DateMillisecond -> int64 AC.PDate64
  AT.ATime _ 32 -> int32 AC.PTime32
  AT.ATime _ 64 -> int64 AC.PTime64
  AT.ATimestamp _ _ -> int64 AC.PTimestamp
  AT.ADuration _ -> int64 AC.PDuration
  -- INT96 (legacy 12-byte timestamp) and FIXED_LEN_BYTE_ARRAY
  -- (UUIDs / float16 / decimal128 in fixed form). Both are
  -- exposed via 'ColFixedSizeBinary'; the bridge handles the
  -- required, whole-chunk PLAIN case.
  AT.AFixedSizeBinary w
    | not nullable
    , ChunkSource codec chunk <- src ->
        fixedSizeBinary w
          =<< if w == 12
            then PR.readPlainInt96ColumnChunk codec chunk
            else PR.readPlainFixedLenByteArrayColumnChunk w codec chunk
  other ->
    Left $
      "Parquet.Arrow: column type "
        <> show other
        <> " (nullable="
        <> show nullable
        <> ") not yet supported by the read bridge; use the "
        <> "specialised readers in Parquet.Read"
  where
    !nullable = AT.fieldNullable fld
    int32 :: AC.PrimType Int32 -> Either String AC.ColumnArray
    int32 t
      | nullable = AC.fromMaybes t <$> optInt32 src
      | otherwise = AC.primColumn t . primToStorable <$> reqInt32 src
    int64 :: AC.PrimType Int64 -> Either String AC.ColumnArray
    int64 t
      | nullable = AC.fromMaybes t <$> optInt64 src
      | otherwise = AC.primColumn t . primToStorable <$> reqInt64 src


reqInt32 :: Source -> Either String (VP.Vector Int32)
reqInt32 (ChunkSource c bs) = PR.readGenericInt32ColumnChunk c bs
reqInt32 (PagesSource c f l k) = PR.readGenericInt32SelectedPages c f l k


reqInt64 :: Source -> Either String (VP.Vector Int64)
reqInt64 (ChunkSource c bs) = PR.readGenericInt64ColumnChunk c bs
reqInt64 (PagesSource c f l k) = PR.readGenericInt64SelectedPages c f l k


reqFloat :: Source -> Either String (VP.Vector Float)
reqFloat (ChunkSource c bs) = PR.readGenericFloatColumnChunk c bs
reqFloat (PagesSource c f l k) = PR.readGenericFloatSelectedPages c f l k


reqDouble :: Source -> Either String (VP.Vector Double)
reqDouble (ChunkSource c bs) = PR.readGenericDoubleColumnChunk c bs
reqDouble (PagesSource c f l k) = PR.readGenericDoubleSelectedPages c f l k


reqBool :: Source -> Either String (V.Vector Bool)
reqBool (ChunkSource c bs) = PR.readGenericBoolColumnChunk c bs
reqBool (PagesSource c f l k) = PR.readGenericBoolSelectedPages c f l k


reqBytes :: Source -> Either String (V.Vector ByteString)
reqBytes (ChunkSource c bs) = PR.readGenericByteArrayColumnChunk c bs
reqBytes (PagesSource c f l k) = PR.readGenericByteArraySelectedPages c f l k


optInt32 :: Source -> Either String (V.Vector (Maybe Int32))
optInt32 (ChunkSource c bs) = PR.readGenericInt32OptionalColumnChunk c 0 1 bs
optInt32 (PagesSource c f l k) = PR.readGenericInt32OptionalSelectedPages c 0 1 f l k


optInt64 :: Source -> Either String (V.Vector (Maybe Int64))
optInt64 (ChunkSource c bs) = PR.readGenericInt64OptionalColumnChunk c 0 1 bs
optInt64 (PagesSource c f l k) = PR.readGenericInt64OptionalSelectedPages c 0 1 f l k


optFloat :: Source -> Either String (V.Vector (Maybe Float))
optFloat (ChunkSource c bs) = PR.readGenericFloatOptionalColumnChunk c 0 1 bs
optFloat (PagesSource c f l k) = PR.readGenericFloatOptionalSelectedPages c 0 1 f l k


optDouble :: Source -> Either String (V.Vector (Maybe Double))
optDouble (ChunkSource c bs) = PR.readGenericDoubleOptionalColumnChunk c 0 1 bs
optDouble (PagesSource c f l k) = PR.readGenericDoubleOptionalSelectedPages c 0 1 f l k


optBool :: Source -> Either String (V.Vector (Maybe Bool))
optBool (ChunkSource c bs) = PR.readGenericBoolOptionalColumnChunk c 0 1 bs
optBool (PagesSource c f l k) = PR.readGenericBoolOptionalSelectedPages c 0 1 f l k


optBytes :: Source -> Either String (V.Vector (Maybe ByteString))
optBytes (ChunkSource c bs) = PR.readGenericByteArrayOptionalColumnChunk c 0 1 bs
optBytes (PagesSource c f l k) = PR.readGenericByteArrayOptionalSelectedPages c 0 1 f l k


{- | Copy a decoder's primitive vector into a pinned Arrow value
buffer with one @memcpy@.
-}
primToStorable :: forall a. Storable a => VP.Vector a -> VS.Vector a
primToStorable (VP.Vector off n ba) = unsafeDupablePerformIO $ do
  mv <- VSM.unsafeNew n
  let !sz = sizeOf (undefined :: a)
  VSM.unsafeWith mv $ \p -> copyByteArrayToAddr (castPtr p) ba (off * sz) (n * sz)
  VS.unsafeFreeze mv


{- | Retag a binary column built from Parquet BYTE_ARRAY values as
utf8. The C kernel validates the data in one pass; a column with
invalid UTF-8 (which an Arrow utf8 column cannot hold) is decoded
lossily instead, replacing bad sequences with U+FFFD, so one bad
value does not fail the whole batch.
-}
utf8FromBinary :: AC.ColumnArray -> Either String AC.ColumnArray
utf8FromBinary col = case col of
  AC.ColBinary mv offs dat
    | Right utf8 <- AC.mkUtf8 mv offs dat -> Right utf8
  _
    | Just ba <- AC.asBinary col ->
        Right $
          AC.fromMaybeTexts
            (V.generate (AC.bytesArrayLength ba) (fmap decodeUtf8Lossy . AC.unsafeBytesAt ba))
    | otherwise -> Left ("Parquet.Arrow: expected a binary column, got " <> AC.columnTag col)


-- | One fixed-size-binary column from equal-width values (one copy).
fixedSizeBinary :: Int -> V.Vector ByteString -> Either String AC.ColumnArray
fixedSizeBinary w vals
  | BS.length dat /= w * rows =
      Left ("Parquet.Arrow: fixed-size binary values are not all " <> show w <> " bytes")
  | otherwise = AC.mkFixedSizeBinary w rows Nothing dat
  where
    !rows = V.length vals
    !dat = BS.concat (V.toList vals)


{- | Look up the column's 'Compression' codec from the footer.
| Compute @max_repetition_level@ for a column chunk by
walking its 'cmPathInSchema' back through the file's schema
and counting 'Repeated' elements. Returns 'Nothing' if the
chunk's metadata is absent or the path can't be resolved
(in which case the caller should treat it as conservatively
repetition-bearing).

Spec: parquet-format/Nested-Layout.md, "Computing levels".
max_repetition_level = number of @REPEATED@ elements on the
path from the root to (and including) the leaf.
-}
columnMaxRepetitionLevel :: PR.ParquetFile -> Int -> Int -> Maybe Int
columnMaxRepetitionLevel pf rgIdx colIdx = do
  rg <- (P.fmRowGroups (PR.pfFooter pf)) V.!? rgIdx
  cc <- P.rgColumns rg V.!? colIdx
  cm <- P.ccMetadata cc
  let !path = P.cmPathInSchema cm
      !schemaV = P.fmSchema (PR.pfFooter pf)
      !pathSegs = V.toList path
  walkPath schemaV pathSegs
  where
    -- Schema is a flattened pre-order walk: root + children
    -- (each carrying num_children). For each segment of the
    -- column path we walk the current subtree's children
    -- until we find a name match, then descend.
    --
    -- We don't need the exact sub-tree positions — only to
    -- count REPEATED reps along the way — so a simple
    -- name-based scan over the flattened list is enough.
    walkPath schemaV segs = Just (sum (countRepetition schemaV <$> segs))

    countRepetition schemaV name =
      let matches = V.filter (\se -> P.seName se == name) schemaV
      in case V.toList matches of
           (se : _) | P.seRepetition se == Just P.Repeated -> 1
           _ -> 0


chunkCodec :: PR.ParquetFile -> Int -> Int -> P.Compression
chunkCodec pf rgIdx colIdx =
  let fm = PR.pfFooter pf
      rgs = P.fmRowGroups fm
      rg = V.unsafeIndex rgs rgIdx
      cols = P.rgColumns rg
      cc = V.unsafeIndex cols colIdx
  in case P.ccMetadata cc of
       Just md -> P.cmCodec md
       Nothing -> P.Uncompressed


{- | UTF-8 decode that swallows invalid sequences (replacing them
with U+FFFD). Real Arrow strings should always be valid UTF-8;
we don't want a single bad byte to fail an entire batch read.
-}
decodeUtf8Lossy :: ByteString -> T.Text
decodeUtf8Lossy bs = case TE.decodeUtf8' bs of
  Right t -> t
  Left _ -> TE.decodeUtf8With TE.lenientDecode bs


-- ============================================================
-- Arrow → Parquet.Nested (nested types)
-- ============================================================

{- | Map an Arrow 'AT.Field' onto a 'PN.NestedSchema' tree
suitable for 'Parquet.HighLevel.encodeParquetNested'. Handles
flat primitives (wraps them in 'PN.NSRequired' / 'PN.NSOptional'
based on 'fieldNullable'), struct, and list types.
-}
arrowFieldToNestedSchema
  :: AT.Field -> Either String PN.NestedSchema
arrowFieldToNestedSchema f = do
  core <- case AT.fieldType f of
    AT.AInt 8 _ -> Right (PN.NSPrimitive PN.LtInt32)
    AT.AInt 16 _ -> Right (PN.NSPrimitive PN.LtInt32)
    AT.AInt 32 _ -> Right (PN.NSPrimitive PN.LtInt32)
    AT.AInt 64 _ -> Right (PN.NSPrimitive PN.LtInt64)
    AT.ABool -> Right (PN.NSPrimitive PN.LtBool)
    AT.AFloatingPoint AT.Single -> Right (PN.NSPrimitive PN.LtFloat)
    AT.AFloatingPoint AT.DoublePrecision -> Right (PN.NSPrimitive PN.LtDouble)
    AT.AUtf8 -> Right (PN.NSPrimitive PN.LtString)
    AT.ABinary -> Right (PN.NSPrimitive PN.LtBinary)
    AT.ALargeUtf8 -> Right (PN.NSPrimitive PN.LtString)
    AT.ALargeBinary -> Right (PN.NSPrimitive PN.LtBinary)
    AT.ADate _ -> Right (PN.NSPrimitive PN.LtInt32)
    AT.ATime _ 32 -> Right (PN.NSPrimitive PN.LtInt32)
    AT.ATime _ 64 -> Right (PN.NSPrimitive PN.LtInt64)
    AT.ATimestamp _ _ -> Right (PN.NSPrimitive PN.LtInt64)
    AT.ADuration _ -> Right (PN.NSPrimitive PN.LtInt64)
    AT.AStruct -> do
      childSchemas <-
        V.mapM
          ( \c -> do
              inner <- arrowFieldToNestedSchema c
              Right (AT.fieldName c, inner)
          )
          (AT.fieldChildren f)
      Right (PN.NSStruct childSchemas)
    AT.AList ->
      case V.toList (AT.fieldChildren f) of
        [child] -> do
          inner <- arrowFieldToNestedSchema child
          Right (PN.NSList inner)
        _ -> Left "Parquet.Arrow: AList must have exactly one child field"
    AT.ALargeList ->
      case V.toList (AT.fieldChildren f) of
        [child] -> do
          inner <- arrowFieldToNestedSchema child
          Right (PN.NSList inner)
        _ -> Left "Parquet.Arrow: ALargeList must have exactly one child field"
    AT.AMap _ ->
      case V.toList (AT.fieldChildren f) of
        [structField]
          | V.length (AT.fieldChildren structField) == 2 ->
              let !kf = V.unsafeIndex (AT.fieldChildren structField) 0
                  !vf = V.unsafeIndex (AT.fieldChildren structField) 1
              in do
                   kSch <- arrowFieldToNestedSchema kf
                   vSch <- arrowFieldToNestedSchema vf
                   Right (PN.NSMap kSch vSch)
        _ -> Left "Parquet.Arrow: AMap's child must be a struct with exactly (key, value)"
    other ->
      Left $
        "Parquet.Arrow.arrowFieldToNestedSchema: "
          ++ show other
          ++ " not yet supported"
  Right $
    if AT.fieldNullable f
      then PN.NSOptional core
      else PN.NSRequired core


{- | Lower an Arrow 'AC.ColumnArray' to a row-major vector of
'PN.NestedRow' entries. Handles struct, list, and flat
primitives; the row count matches the column's logical length.
Null rows (of any supported shape) become 'PN.NRNull'.
-}
columnArrayToNestedRows
  :: AC.ColumnArray -> Either String (V.Vector PN.NestedRow)
columnArrayToNestedRows col = case col of
  AC.ColInt32 mv xs -> Right (primRows PN.LvInt32 mv xs)
  AC.ColInt64 mv xs -> Right (primRows PN.LvInt64 mv xs)
  AC.ColFloat mv xs -> Right (primRows PN.LvFloat mv xs)
  AC.ColDouble mv xs -> Right (primRows PN.LvDouble mv xs)
  -- Struct: each row is an NRStruct of field values indexed in
  -- declared order (children may hold more rows than the struct).
  AC.ColStruct n mv childCols -> do
    childRows <- V.mapM (columnArrayToNestedRows . AC.sliceColumnArray 0 n . snd) childCols
    if V.any ((< n) . V.length) childRows
      then Left "Parquet.Arrow: struct children have fewer rows than the struct"
      else Right $ V.generate n $ \i ->
        if AC.isValidAt mv i
          then PN.NRStruct (V.map (`V.unsafeIndex` i) childRows)
          else PN.NRNull
  -- List: the row's child range selects its slice of the child
  -- rows (offsets need not start at zero).
  AC.ColList _ _ child -> listRows child
  AC.ColLargeList _ _ child -> listRows child
  _
    | Just ba <- AC.asBool col ->
        Right (V.generate (AC.columnLength col) (leafRow PN.LvBool . AC.boolArrayAt ba))
    | Just ua <- AC.asUtf8 col -> Right (textRows ua)
    | Just ua <- AC.asLargeUtf8 col -> Right (textRows ua)
    | Just ba <- AC.asBinary col -> Right (bytesRows ba)
    | Just ba <- AC.asLargeBinary col -> Right (bytesRows ba)
    | otherwise ->
        Left $
          "Parquet.Arrow.columnArrayToNestedRows: "
            ++ AC.columnTag col
            ++ " not yet supported (map, union, "
            ++ "dictionary, view, REE, interval)"
  where
    listRows child = do
      childRows <- columnArrayToNestedRows child
      Right $ V.generate (AC.columnLength col) $ \i ->
        case AC.listRange col i of
          Just (AC.ChildRange start len) -> PN.NRList (V.slice start len childRows)
          Nothing -> PN.NRNull
    textRows ua =
      let AC.Utf8Array ba = ua
      in V.generate (AC.bytesArrayLength ba) (leafRow PN.LvString . AC.unsafeTextAt ua)
    bytesRows ba = V.generate (AC.bytesArrayLength ba) (leafRow PN.LvBinary . AC.unsafeBytesAt ba)


leafRow :: (a -> PN.LeafValue) -> Maybe a -> PN.NestedRow
leafRow f = maybe PN.NRNull (PN.NRLeaf . f)
{-# INLINE leafRow #-}


primRows
  :: Storable a => (a -> PN.LeafValue) -> Maybe AC.Validity -> VS.Vector a -> V.Vector PN.NestedRow
primRows f mv xs =
  let !pa = AC.PrimArray mv xs
  in V.generate (VS.length xs) (leafRow f . AC.unsafePrimAt pa)
{-# INLINE primRows #-}


-- ============================================================
-- Streaming reader
-- ============================================================

{- | Number of row groups in the file. Useful as a loop bound for
'parquetRowGroupToArrow' / 'streamRowGroups'.
-}
numRowGroups :: PR.ParquetFile -> Int
numRowGroups pf =
  V.length (P.fmRowGroups (PR.pfFooter pf))


{- | Lazily project every row group of a Parquet file into Arrow
columns, mirroring pyarrow's @ParquetFile.iter_batches()@. The
resulting list defers the per-row-group decode until the
consumer pulls the corresponding @Either@; failed row groups
surface their parse error in the @Left@ slot without aborting
the rest of the stream.

@
pf <- 'Parquet.HighLevel.decodeParquet' bytes
forM_ ('streamRowGroups' arrowSchema pf) $ \\rg -> case rg of
  Right cols -> consume cols
  Left  err  -> log err
@
-}
streamRowGroups
  :: AT.Schema
  -> PR.ParquetFile
  -> [Either String (V.Vector AC.ColumnArray)]
streamRowGroups sch pf =
  [ case parquetRowGroupToArrow sch pf i of
      Right cols -> Right cols
      Left err -> Left (show err)
  | i <- [0 .. numRowGroups pf - 1]
  ]


{- | Iterator-shaped variant of 'streamRowGroups'. Each
'IS.iterStep' decodes one row group on demand. Use
'Columnar.Stream.iterTake' / 'Columnar.Stream.iterFold' /
friends to drive it without materialising every row group up
front.

Behaves like 'streamRowGroups' for the per-row-group decode
(same target Arrow schema controls projection / coercion);
the difference is that decoding errors halt the iterator at
the failing step instead of being threaded through a list.
-}
streamRowGroupsIter
  :: AT.Schema
  -> PR.ParquetFile
  -> IS.Iter (V.Vector AC.ColumnArray)
streamRowGroupsIter sch pf =
  IS.iterFromIndexed (numRowGroups pf) $ \i ->
    case parquetRowGroupToArrow sch pf i of
      Right cols -> Right cols
      Left err -> Left (show err)


{- | Like 'streamRowGroupsIter' but projects each row group to a
subset of named columns (and an optional reordering). The
caller supplies the target Arrow schema /before/ projection;
this helper extracts the named subset and runs the
per-row-group decode against the narrower schema, so only the
requested columns are read off disk.

Names not present in the source schema cause every iterator
step to fail with the same error (matching the
'Arrow.Stream.streamReaderProjectedIter' shape).
-}
streamRowGroupsProjectedIter
  :: AT.Schema
  -> [Text]
  -> PR.ParquetFile
  -> IS.Iter (V.Vector AC.ColumnArray)
streamRowGroupsProjectedIter sch names pf =
  case projectFields names sch of
    Left e -> IS.iterUnfold () (\_ -> Left e)
    Right narrowSch -> streamRowGroupsIter narrowSch pf


{- | Decode a single row group with column projection. Equivalent
to @'parquetRowGroupToArrow' (projectSchema names target) pf
rgIdx@ but checks the projection up front so the error path is
single-shot rather than per-column.
-}
parquetRowGroupToArrowProjected
  :: AT.Schema
  -> [Text]
  -> PR.ParquetFile
  -> Int
  -> Either ProjectionError (V.Vector AC.ColumnArray)
parquetRowGroupToArrowProjected target names pf rgIdx = do
  narrow <- case projectFields names target of
    Right s -> Right s
    Left _ -> Left (MissingColumn (T.pack "<projection>"))
  parquetRowGroupToArrow narrow pf rgIdx


{- | Iterator over row groups that drops any row group whose
statistics prove the predicate matches no rows.

Skipping is /sound/: only row groups whose
'Parquet.Predicate.evalRowGroup' returns 'Pred.PSkip' are
elided. Row groups whose statistics are missing or
inconclusive are decoded normally and yielded as iterator
elements.

Returns the iterator paired with the planning summary
@(totalRowGroups, skippedRowGroups)@ so callers can log how
effective the predicate was without holding onto the file.
-}
streamRowGroupsFilteredIter
  :: AT.Schema
  -> Pred.Predicate
  -> PR.ParquetFile
  -> (Int, Int, IS.Iter (V.Vector AC.ColumnArray))
streamRowGroupsFilteredIter sch predicate pf =
  let !leafNames = leafColumnNames pf
      !rgs = P.fmRowGroups (PR.pfFooter pf)
      !nRg = V.length rgs
      keep i =
        Pred.evalRowGroup leafNames predicate (V.unsafeIndex rgs i)
          == Pred.PMaybeKeep
      !kept = V.filter keep (V.enumFromN 0 nRg)
      !nKept = V.length kept
      !nSkip = nRg - nKept
      step k =
        let !i = V.unsafeIndex kept k
        in case parquetRowGroupToArrow sch pf i of
             Right cols -> Right cols
             Left err -> Left (show err)
  in (nRg, nSkip, IS.iterFromIndexed nKept step)


{- | Combination of 'streamRowGroupsProjectedIter' and
'streamRowGroupsFilteredIter': only decodes the named
columns of row groups whose statistics survive the
predicate.
-}
streamRowGroupsProjectedFilteredIter
  :: AT.Schema
  -> [Text]
  -> Pred.Predicate
  -> PR.ParquetFile
  -> Either String (Int, Int, IS.Iter (V.Vector AC.ColumnArray))
streamRowGroupsProjectedFilteredIter sch names predicate pf = do
  narrowSch <- projectFields names sch
  let (nRg, nSkip, it) =
        streamRowGroupsFilteredIter narrowSch predicate pf
  Right (nRg, nSkip, it)


{- | Leaf column names of a 'PR.ParquetFile' in the same order
the row groups' @rgColumns@ vectors use. Built from the
footer's flat schema (skipping the synthetic root struct).
-}
leafColumnNames :: PR.ParquetFile -> V.Vector Text
leafColumnNames pf =
  V.map
    P.seName
    ( V.filter
        (maybe False (const True) . P.seType)
        (P.fmSchema (PR.pfFooter pf))
    )


{- | Read one column with page-level predicate pushdown.

Looks up the column chunk's 'OffsetIndex' + 'ColumnIndex',
evaluates the predicate against the 'ColumnIndex' to produce
a per-page keep mask, then decodes only the surviving pages
using the file-offset-based 'PR.readGeneric*SelectedPages'
family.

Handles both required and (top-level-)nullable columns
automatically, dispatching to the optional-pages decoder
when 'AT.fieldNullable' is set.

Returns 'Right (Nothing, ...)' when the column doesn't carry
a 'ColumnIndex' / 'OffsetIndex' pair (page-level pruning isn't
possible without the page-index region — fall back to
'readParquetColumn'). Returns 'Right (Just (kept, total), col)'
when pruning ran, with @kept@ pages decoded out of @total@.

/Repeated columns are rejected/: a column whose path crosses
a 'Repeated' schema element (lists, maps, or any field with
@max_repetition_level > 0@) carries per-page repetition
streams that change the row count of every kept page. The
simple keep-mask shape this function exposes can't model
that, so it returns 'Left' with a typed error rather than
silently producing the wrong row count. Callers reading
nested columns should fall through to 'readParquetColumn'
(no pushdown) or to 'Parquet.Nested' for the Dremel-shredded
read path.
-}
readParquetColumnWithPagePruning
  :: PR.ParquetFile
  -> Int
  -- ^ row-group index
  -> Int
  -- ^ column index within the row group
  -> AT.Field
  -- ^ Arrow target field
  -> Pred.PColPredicate
  -- ^ predicate to push down to the page index
  -> Either String (Maybe (Int, Int), AC.ColumnArray)
readParquetColumnWithPagePruning pf rgIdx colIdx fld predicate = do
  -- Refuse repeated columns up front (see haddock).
  case columnMaxRepetitionLevel pf rgIdx colIdx of
    Just rep
      | rep > 0 ->
          Left $
            "Parquet.Arrow.readParquetColumnWithPagePruning: column "
              ++ show colIdx
              ++ " has max_repetition_level="
              ++ show rep
              ++ " (lists/maps/repeated fields). Page-level pushdown "
              ++ "doesn't model per-page row-count changes from "
              ++ "repetition streams; use 'readParquetColumn' or "
              ++ "'Parquet.Nested' instead."
    _ -> Right ()
  mIdx <- loadIndices pf rgIdx colIdx
  case mIdx of
    Nothing -> do
      col <- mapLeftShow (readParquetColumn pf rgIdx colIdx fld)
      Right (Nothing, col)
    Just (oi, ci, ptype) -> do
      let !decisions = Pred.evalPagesByColumnIndex ptype ci predicate
          !keep = V.map (== Pred.PMaybeKeep) decisions
          !total = V.length keep
          !nKept = V.length (V.filter id keep)
          !src = PagesSource (chunkCodec pf rgIdx colIdx) (PR.pfBytes pf) (P.oiPageLocations oi) keep
      col <- liftColumn src fld
      Right (Just (nKept, total), col)
  where
    mapLeftShow :: Either e a -> Either String a
    mapLeftShow (Right x) = Right x
    mapLeftShow (Left _) = Left "Parquet.Arrow: page-pruning fallback failed"


loadIndices
  :: PR.ParquetFile
  -> Int
  -> Int
  -> Either String (Maybe (P.OffsetIndex, P.ColumnIndex, P.ParquetType))
loadIndices pf rgIdx colIdx = do
  mOff <- PI.readOffsetIndex pf rgIdx colIdx
  mCol <- PI.readColumnIndex pf rgIdx colIdx
  case (mOff, mCol) of
    (Just oi, Just ci) -> do
      let !rgs = P.fmRowGroups (PR.pfFooter pf)
          !rg = V.unsafeIndex rgs rgIdx
          !cc = V.unsafeIndex (P.rgColumns rg) colIdx
      case P.ccMetadata cc of
        Just md -> Right (Just (oi, ci, P.cmType md))
        Nothing -> Right Nothing
    _ -> Right Nothing


-- | Build a sub-schema by name. Preserves the order of @names@.
projectFields :: [Text] -> AT.Schema -> Either String AT.Schema
projectFields names sch =
  let !fields = AT.arrowFields sch
      !byName =
        Map.fromList
          [(AT.fieldName f, f) | f <- V.toList fields]
      pickOne nm = case Map.lookup nm byName of
        Just f -> Right f
        Nothing ->
          Left $
            "Parquet.Arrow: projected column "
              ++ show nm
              ++ " not present in target schema"
  in do
       fs <- traverse pickOne names
       Right sch {AT.arrowFields = V.fromList fs}
