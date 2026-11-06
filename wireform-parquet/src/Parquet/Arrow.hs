{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

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
'Bool', 'Utf8', 'Binary', plus their nullable variants. Use the
"Parquet.Nested" writer helpers for structs, lists and maps. The
reader reconstructs those containers from physical schema paths and
Dremel levels, including legacy list layouts and nullable ancestors.
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
  ReadLimits (..),
  defaultReadLimits,
  parquetRowGroupToArrowWithLimits,
  parquetRowGroupToArrowProjected,
  readParquetColumn,
  parquetFileArrowSchema,
  parquetFileArrowSchemaChecked,
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
import Arrow.Column.Buffer qualified as Buffer
import Arrow.Types qualified as AT
import Arrow.Write.Columns (validateColumns)
import Columnar.Stream qualified as IS
import Control.Monad.ST (runST)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BSI
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Primitive.ByteArray (copyByteArrayToAddr)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (byteSwap64, Word16, Word32, Word64, Word8)
import Foreign.Ptr (castPtr)
import Foreign.Storable (pokeByteOff, Storable, sizeOf)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (Int (I#), Int#, (+#))
import Parquet.Arrow.Geospatial qualified as Geo
import Parquet.Arrow.File qualified as FileArrow
import Parquet.File qualified as File
import Parquet.Arrow.Nested qualified as AN
import Parquet.Levels qualified as Levels
import Parquet.Nested qualified as PN
import Parquet.Nested.Generate qualified as Generate
import Parquet.Page qualified as Page
import Parquet.PageIndex qualified as PI
import Parquet.Predicate qualified as Pred
import Parquet.Read qualified as PR
import Parquet.Types qualified as P
import Parquet.Timestamp.Internal qualified as Timestamp
import Parquet.Write qualified as PW
import Parquet.Variant.Schema qualified as VariantSchema
import Parquet.Variant.Array qualified as VariantArray
import System.IO.Unsafe (unsafeDupablePerformIO)


-- ============================================================
-- Arrow → Parquet
-- ============================================================

{- | Lower an Arrow schema + a sequence of column-major batches
to the inputs 'Parquet.HighLevel.encodeParquet' expects.

Each Arrow batch becomes one Parquet row group. Returns 'Left'
if any column type isn't representable in Parquet's flat data
plane (struct / list / dictionary / view / REE — see the
module docs for the supported subset), the batch disagrees with the
schema, or a column contains nulls. This dense path writes required
fields even when an input field permits nulls; use
'arrowToParquetMixed' to preserve nullable schema declarations.
-}
arrowToParquet
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String (V.Vector P.SchemaElement, [V.Vector PW.ColumnData])
arrowToParquet sch batches = do
  mapM_ (validateColumns (AT.arrowFields sch)) batches
  mapM_ (V.mapM_ rejectNulls) batches
  let !leafFields = AT.arrowFields sch
      !rootElem =
        P.SchemaElement
          { P.seTypeLength = Nothing
          , P.seName = "schema"
          , P.seRepetition = Nothing
          , P.seType = Nothing
          , P.seNumChildren = Just (fromIntegral (V.length leafFields))
          , P.seConvertedType = Nothing
          , P.seLogicalType = Nothing
          , P.seFieldId = Nothing
          }
  schemaElems <- V.mapM (\field -> arrowFieldToSchemaElement field {AT.fieldNullable = False}) leafFields
  let !pSchema = V.cons rootElem schemaElems
  rgData <-
    mapM
      ( V.zipWithM
          (\field col -> normalizeTemporal field col >>= columnArrayToColumnData)
          leafFields
      )
      batches
  Right (pSchema, rgData)
  where
    rejectNulls col
      | AC.nullCount col == 0 = Right ()
      | otherwise = Left "Parquet.Arrow.arrowToParquet: use arrowToParquetMixed to preserve null values"


{- | Like 'arrowToParquet' but returns 'PW.ParquetColumn' values
so nullable Arrow columns lower to 'PW.PCOptional' instead of
being dropped through 'optionalColumnPresentValues'. Pair with
'Parquet.HighLevel.encodeParquetMixed' to write a Parquet file
that carries the nulls and definition levels.
-}
arrowToParquetMixed
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String (V.Vector P.SchemaElement, [V.Vector PW.ParquetColumn])
arrowToParquetMixed sch batches = do
  mapM_ (validateColumns (AT.arrowFields sch)) batches
  let !leafFields = AT.arrowFields sch
      !rootElem =
        P.SchemaElement
          { P.seTypeLength = Nothing
          , P.seName = "schema"
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
columnArrayToParquetColumn fld input = do
  validateColumns (V.singleton fld) (V.singleton input)
  col <- normalizeTemporal fld input
  columnArrayToParquetColumnRaw fld col


columnArrayToParquetColumnRaw :: AT.Field -> AC.ColumnArray -> Either String PW.ParquetColumn
columnArrayToParquetColumnRaw fld col
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


-- Parquet DATE is INT32 days; TIME/TIMESTAMP have no second unit.
-- Normalize the payload together with its annotation, rejecting lossy or
-- overflowing conversions. Null slots have no value and are ignored.
normalizeTemporal :: AT.Field -> AC.ColumnArray -> Either String AC.ColumnArray
normalizeTemporal field column = case (AT.fieldType field, column) of
  (AT.ADate AT.DateMillisecond, AC.ColDate64 validity values) ->
    checkedTemporal AC.PDate32 validDay (fromIntegral . (`quot` dayMillis)) validity values
  (AT.ATime AT.Second 32, AC.ColTime32 validity values) ->
    checkedTemporal AC.PTime32 (withinDay 86400) (* 1000) validity values
  (AT.ATime AT.Millisecond 32, AC.ColTime32 validity values) ->
    checkTime 86400000 validity values
  (AT.ATime AT.Microsecond 64, AC.ColTime64 validity values) ->
    checkTime 86400000000 validity values
  (AT.ATime AT.Nanosecond 64, AC.ColTime64 validity values) ->
    checkTime 86400000000000 validity values
  (AT.ATime _ _, _) -> Left "Parquet.Arrow: invalid time unit/bit-width combination"
  (AT.ATimestamp AT.Second _, AC.ColTimestamp validity values) ->
    checkedTemporal
      AC.PTimestamp
      (\value -> value >= (minBound :: Int64) `quot` 1000 && value <= (maxBound :: Int64) `quot` 1000)
      (* 1000)
      validity
      values
  (AT.ADuration _, _) -> Left "Parquet.Arrow: duration has no standard Parquet logical type"
  _ -> Right column
  where
    dayMillis = 86400000 :: Int64
    validDay value =
      let (days, remainder) = value `quotRem` dayMillis
      in remainder == 0
           && days >= fromIntegral (minBound :: Int32)
           && days <= fromIntegral (maxBound :: Int32)
    withinDay limit value = value >= 0 && value < limit
    checkTime limit validity values
      | VS.ifoldl' (\ok i value -> ok && (not (AC.isValidAt validity i) || withinDay limit value)) True values = Right column
      | otherwise = Left "Parquet.Arrow: time is outside one day"


checkedTemporal
  :: (Storable a, Storable b)
  => AC.PrimType b
  -> (a -> Bool)
  -> (a -> b)
  -> Maybe AC.Validity
  -> VS.Vector a
  -> Either String AC.ColumnArray
checkedTemporal tag valid convert validity values
  | VS.ifoldl' (\ok i value -> ok && (not (AC.isValidAt validity i) || valid value)) True values =
      AC.mkPrim tag validity (VS.map convert values)
  | otherwise = Left "Parquet.Arrow: temporal conversion loses precision or exceeds its range"


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
    AT.AFloatingPoint AT.Half -> Right P.PTFixedLenByteArray
    AT.AFloatingPoint AT.Single -> Right P.PTFloat
    AT.AFloatingPoint AT.DoublePrecision -> Right P.PTDouble
    AT.AUtf8 -> Right P.PTByteArray
    AT.ABinary -> Right P.PTByteArray
    AT.ALargeUtf8 -> Right P.PTByteArray
    AT.ALargeBinary -> Right P.PTByteArray
    -- Dates normalize to INT32 days and seconds normalize to milliseconds.
    AT.ADate AT.DateDay -> Right P.PTInt32
    AT.ADate AT.DateMillisecond -> Right P.PTInt32
    AT.ATime AT.Second 32 -> Right P.PTInt32
    AT.ATime AT.Millisecond 32 -> Right P.PTInt32
    AT.ATime AT.Microsecond 64 -> Right P.PTInt64
    AT.ATime AT.Nanosecond 64 -> Right P.PTInt64
    AT.ATimestamp _ _ -> Right P.PTInt64
    other ->
      Left $
        "Parquet.Arrow: Arrow type "
          <> show other
          <> " has no Parquet flat-primitive equivalent"
  geospatial <- Geo.fromArrowField f
  let !rep = if AT.fieldNullable f then P.Optional else P.Required
      !logical = case geospatial of
        Just annotation -> Just annotation
        Nothing -> baseLogical
      baseLogical = case AT.fieldType f of
        AT.AFloatingPoint AT.Half -> Just P.LTFloat16
        AT.AInt w signed -> Just (P.LTInteger (fromIntegral w) signed)
        AT.AUtf8 -> Just (if isJsonField f then P.LTJson else P.LTString)
        AT.ALargeUtf8 -> Just (if isJsonField f then P.LTJson else P.LTString)
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
      { P.seTypeLength = if AT.fieldType f == AT.AFloatingPoint AT.Half then Just 2 else Nothing
      , P.seName = AT.fieldName f
      , P.seRepetition = Just rep
      , P.seType = Just pType
      , P.seNumChildren = Nothing
      , P.seConvertedType = case AT.fieldType f of
          AT.AInt 8 True -> Just P.CTInt8
          AT.AInt 16 True -> Just P.CTInt16
          AT.AInt 32 True -> Just P.CTInt32
          AT.AInt 64 True -> Just P.CTInt64
          AT.AInt 8 False -> Just P.CTUInt8
          AT.AInt 16 False -> Just P.CTUInt16
          AT.AInt 32 False -> Just P.CTUInt32
          AT.AInt 64 False -> Just P.CTUInt64
          AT.AUtf8 -> Just (if isJsonField f then P.CTJson else P.CTUtf8)
          AT.ALargeUtf8 -> Just (if isJsonField f then P.CTJson else P.CTUtf8)
          AT.ADate _ -> Just P.CTDate
          AT.ATime AT.Second 32 -> Just P.CTTimeMillis
          AT.ATime AT.Millisecond 32 -> Just P.CTTimeMillis
          AT.ATime AT.Microsecond 64 -> Just P.CTTimeMicros
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


isJsonField :: AT.Field -> Bool
isJsonField field = V.elem ("ARROW:extension:name", "arrow.json") (AT.fieldMetadata field)


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

Lookup is by top-level column name; nested leaf names remain scoped
to their parents. The target schema may be narrower than the file
(projection), in a different order than the file (reordering),
or request wider types than the file (coercion: see
'coerceColumn' below for the supported widening rules). Struct children
are projected recursively by name; list/map wrappers use positional child
matching and may use caller-selected names. Lists may request 64-bit offsets.
A required target is rejected if a visible source value is null.

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
parquetRowGroupToArrow = parquetRowGroupToArrowWithLimits defaultReadLimits


-- | Budget for values synthesized without any physical FILE leaf. Ordinary
-- column-page decoding retains its existing encoded-size and page bounds.
data ReadLimits = ReadLimits
  { readMaxUnbackedValues :: {-# UNPACK #-} !Int64
  }
  deriving (Show, Eq)


defaultReadLimits :: ReadLimits
defaultReadLimits = ReadLimits {readMaxUnbackedValues = 1048576}


-- | Like 'parquetRowGroupToArrow', with a caller-selected aggregate budget for
-- synthesized zero-field FILE values (one value per output slot per FILE).
parquetRowGroupToArrowWithLimits :: ReadLimits -> AT.Schema -> PR.ParquetFile -> Int -> Either ProjectionError (V.Vector AC.ColumnArray)
parquetRowGroupToArrowWithLimits limits target pf rgIdx = do
  if readMaxUnbackedValues limits < 0
    then Left (ColumnDecodeFailed "<limits>" "negative unbacked-value budget")
    else Right ()
  plans <-
    either (Left . ColumnDecodeFailed "<schema>") Right $
      AN.plans schemaElementToArrowField (P.fmSchema (PR.pfFooter pf))
  columns <-
    if V.any isNested plans
      then readNestedGroup limits target pf rgIdx plans
      else readFlatGroup target pf rgIdx
  either
    (Left . ColumnDecodeFailed "<target-schema>")
    Right
    (validateColumns (AT.arrowFields target) columns)
  Right columns
  where
    isNested plan = case AN.planKind plan of AN.Primitive -> False; _ -> True


readFlatGroup :: AT.Schema -> PR.ParquetFile -> Int -> Either ProjectionError (V.Vector AC.ColumnArray)
readFlatGroup target pf rgIdx = do
  let !fileLeaves =
        V.filter
          (maybe False (const True) . P.seType)
          (P.fmSchema (PR.pfFooter pf))
      !nameToIdx =
        Map.fromList
          [(P.seName se, i) | (i, se) <- V.toList (V.indexed fileLeaves)]
  if Map.size nameToIdx /= V.length fileLeaves
    then Left (ColumnDecodeFailed "<schema>" "duplicate top-level column names")
    else V.mapM (readOneProjected pf rgIdx fileLeaves nameToIdx) (AT.arrowFields target)


readNestedGroup :: ReadLimits -> AT.Schema -> PR.ParquetFile -> Int -> V.Vector AN.Plan -> Either ProjectionError (V.Vector AC.ColumnArray)
readNestedGroup limits target pf rgIdx plans = do
  group <-
    maybe
      (Left (ColumnDecodeFailed "<row-group>" "row group index out of range"))
      Right
      (P.fmRowGroups (PR.pfFooter pf) V.!? rgIdx)
  if P.rgNumRows group < 0 || toInteger (P.rgNumRows group) > toInteger (maxBound :: Int) - 7
    then Left (ColumnDecodeFailed "<row-group>" "row count cannot be represented safely")
    else Right ()
  let named = Map.fromList (V.toList (V.map (\p -> (AT.fieldName (AN.planField p), p)) plans))
      rows = fromIntegral (P.rgNumRows group)
      nativeFields = V.foldl' collectTimestampFields IntMap.empty plans
  if Map.size named /= V.length plans
    then Left (ColumnDecodeFailed "<schema>" "duplicate top-level column names")
    else do
      -- Select requested storage widths before any physical leaf is forced.
      -- A preliminary type-only projection leaves nullability checks to the
      -- normal projection below, which has the decoded definition levels.
      storagePlans <-
        V.mapM
          ( \field -> do
              plan <- maybe (Left (MissingColumn (AT.fieldName field))) Right (Map.lookup (AT.fieldName field) named)
              projectNestedPlan True V.empty 0 (AT.fieldName field) field plan
          )
          (AT.arrowFields target)
      let overrides = V.foldl' collectNarrow IntMap.empty storagePlans
          physical = V.generate (V.length (P.rgColumns group)) (readLeveledLeaf overrides pf rgIdx)
      selected <-
        V.mapM
          ( \field -> do
              plan <- maybe (Left (MissingColumn (AT.fieldName field))) Right (Map.lookup (AT.fieldName field) named)
              projectNestedPlan (rows == 0) physical 0 (AT.fieldName field) field plan
          )
          (AT.arrowFields target)
      _ <-
        V.foldM
          ( \total plan -> do
              cost <- either (Left . ColumnDecodeFailed "<unbacked>") Right (AN.unbackedValues rows physical plan)
              if cost > readMaxUnbackedValues limits - total
                then Left (ColumnDecodeFailed "<unbacked>" "synthesized FILE values exceed the unbacked-value budget")
                else Right (total + cost)
          )
          0
          selected
      V.zipWithM
        ( \field projected -> do
            column <- either (Left . ColumnDecodeFailed (AT.fieldName field)) Right (AN.assembleWithRows rows (widenNestedPrimitive nativeFields) physical projected)
            if AC.columnLength column /= rows
              then Left (ColumnDecodeFailed (AT.fieldName field) "decoded row count differs from row-group metadata")
              else Right column
        )
        (AT.arrowFields target)
        selected


collectTimestampFields :: IntMap.IntMap AT.Field -> AN.Plan -> IntMap.IntMap AT.Field
collectTimestampFields fields plan = case AN.planKind plan of
  AN.Primitive
    | Timestamp.isTimestampField (AN.planField plan) -> IntMap.insert (AN.planAnchor plan) (AN.planField plan) fields
    | otherwise -> fields
  AN.Struct children -> V.foldl' collectTimestampFields fields children
  AN.List child -> collectTimestampFields fields child
  AN.Map key value -> collectTimestampFields (collectTimestampFields fields key) value
  AN.EmptyFile -> fields


-- Only context-dependent narrow decimals need an override. Ordinals keep
-- equally named leaves under different Variant objects independent.
collectNarrow :: IntMap.IntMap AT.ArrowType -> AN.Plan -> IntMap.IntMap AT.ArrowType
collectNarrow current plan = case AN.planKind plan of
  AN.Primitive -> case AT.fieldType (AN.planField plan) of
    typ@AT.ADecimal32 {} -> IntMap.insert (AN.planAnchor plan) typ current
    typ@AT.ADecimal64 {} -> IntMap.insert (AN.planAnchor plan) typ current
    AT.ALargeUtf8 -> IntMap.insert (AN.planAnchor plan) AT.ALargeUtf8 current
    AT.ALargeBinary -> IntMap.insert (AN.planAnchor plan) AT.ALargeBinary current
    _ -> current
  AN.Struct fields -> V.foldl' collectNarrow current fields
  AN.List child -> collectNarrow current child
  AN.Map key value -> collectNarrow (collectNarrow current key) value
  AN.EmptyFile -> current


-- Retaining a Variant annotation also retains its exact semantic storage.
-- Dropping fallback fields or widening int32 to int64 changes Variant values.
-- Raw physical projections remain available by removing the extension.
sameVariantStorage :: AT.Field -> AT.Field -> Bool
sameVariantStorage source target =
  sameType && AT.fieldDictionary source == AT.fieldDictionary target && sameChildren && sameExtension
  where
    sourceType = AT.fieldType source
    targetType = AT.fieldType target
    list typ = typ `elem` [AT.AList, AT.ALargeList]
    sameType = sourceType == targetType || (list sourceType && list targetType)
    sourceChildren = AT.fieldChildren source
    targetChildren = AT.fieldChildren target
    sameChildren
      | list sourceType = V.length sourceChildren == 1 && V.length targetChildren == 1 && sameVariantStorage (V.head sourceChildren) (V.head targetChildren)
      | otherwise =
          let named = Map.fromList (V.toList (V.map (\field -> (AT.fieldName field, field)) sourceChildren))
          in V.length sourceChildren == V.length targetChildren && V.all (\field -> maybe False (\native -> sameVariantStorage native field) (Map.lookup (AT.fieldName field) named)) targetChildren
    extensions = V.filter ((== "ARROW:extension:name") . fst) . AT.fieldMetadata
    sameExtension = extensions source == extensions target


-- Select the logical tree before assembly. List/map wrapper names are
-- positional; struct members are selected by name. Anchors follow retained
-- children so an omitted physical leaf is neither read nor decoded.
projectNestedPlan
  :: Bool
  -> V.Vector (Either String AN.LeafColumn)
  -> Int
  -> Text
  -> AT.Field
  -> AN.Plan
  -> Either ProjectionError AN.Plan
projectNestedPlan emptyRowGroup leaves ancestorPresence path requested native = do
  if VariantSchema.isVariantField requested
    then do
      either
        (Left . ColumnDecodeFailed path)
        Right
        (VariantSchema.validateVariantFieldWithLimits (VariantSchema.VariantSchemaLimits 128 maxBound) requested)
      if VariantSchema.isVariantField source && sameVariantStorage source requested then Right () else incompatible
    else Right ()
  timestampCompatible <-
    if Timestamp.isTimestampField source || Timestamp.isTimestampField requested
      then either (Left . ColumnDecodeFailed path) Right (Timestamp.fieldsCompatible source requested)
      else Right (compatiblePrimitive (AT.fieldType source) (AT.fieldType requested))
  case AT.fieldDictionary requested of
    Just _ -> incompatible
    Nothing -> Right ()
  projected <- case AN.planKind native of
    AN.Primitive
      | V.null (AT.fieldChildren requested)
      , timestampCompatible ->
          Right native {AN.planField = requested}
    AN.EmptyFile
      | AT.fieldType requested == AT.AStruct && V.null (AT.fieldChildren requested) ->
          Right native {AN.planField = requested}
    AN.Struct children
      | AT.fieldType requested == AT.AStruct -> do
          let named = Map.fromList (V.toList (V.map (\p -> (AT.fieldName (AN.planField p), p)) children))
          selected <-
            V.mapM
              ( \field -> do
                  let childPath = path <> "." <> AT.fieldName field
                  child <- maybe (Left (MissingColumn childPath)) Right (Map.lookup (AT.fieldName field) named)
                  projectNestedPlan emptyRowGroup leaves visible childPath field child
              )
              (AT.fieldChildren requested)
          Right
            native
              { AN.planField = requested
              , AN.planAnchor = maybe (AN.planAnchor native) AN.planAnchor (V.find ((>= 0) . AN.planAnchor) selected)
              , AN.planKind = AN.Struct selected
              }
    AN.List child
      | AT.fieldType requested `elem` [AT.AList, AT.ALargeList]
      , [childField] <- V.toList (AT.fieldChildren requested) -> do
          selected <- projectNestedPlan emptyRowGroup leaves (AN.planSlotDef child) (path <> "." <> AT.fieldName childField) childField child
          Right native {AN.planField = requested, AN.planAnchor = AN.planAnchor selected, AN.planKind = AN.List selected}
    AN.Map key value
      | AT.fieldType requested == AT.AMap False
      , [entryField] <- V.toList (AT.fieldChildren requested)
      , AT.fieldType entryField == AT.AStruct
      , not (AT.fieldNullable entryField)
      , Nothing <- AT.fieldDictionary entryField
      , [keyField, valueField] <- V.toList (AT.fieldChildren entryField)
      , not (AT.fieldNullable keyField) -> do
          selectedKey <- projectNestedPlan emptyRowGroup leaves (AN.planSlotDef key) (path <> "." <> AT.fieldName keyField) keyField key
          selectedValue <- projectNestedPlan emptyRowGroup leaves (AN.planSlotDef value) (path <> "." <> AT.fieldName valueField) valueField value
          let anchor = if AN.planAnchor selectedKey >= 0 then AN.planAnchor selectedKey else AN.planAnchor selectedValue
          Right native {AN.planField = requested, AN.planAnchor = anchor, AN.planKind = AN.Map selectedKey selectedValue}
    _ -> incompatible
  -- Null ancestors have no child value. Narrowing a nullable child is valid
  -- only when every event at which its ancestors are present contains it.
  -- Check native levels before changing validity/filling absent child slots.
  if not emptyRowGroup && AT.fieldNullable source && not (AT.fieldNullable requested)
    then do
      if AN.planAnchor projected < 0 then incompatible else Right ()
      leaf <- maybe (Left (ColumnDecodeFailed path "missing physical leaf")) (either (Left . ColumnDecodeFailed path) Right) (leaves V.!? AN.planAnchor projected)
      if VP.any (\def -> fromIntegral def >= ancestorPresence && fromIntegral def < AN.planPresence native) (AN.leafDefs leaf)
        then incompatible
        else Right projected
    else Right projected
  where
    source = AN.planField native
    visible = max ancestorPresence (AN.planPresence native)
    incompatible = Left (IncompatibleType path (AT.fieldType requested))
    compatiblePrimitive actual target =
      actual == target
        || canWidenBinary actual target
        || (actual == AT.AInt 32 True && target == AT.AInt 64 True)
        || (actual == AT.AFloatingPoint AT.Single && target == AT.AFloatingPoint AT.DoublePrecision)


widenNestedPrimitive :: IntMap.IntMap AT.Field -> AN.Plan -> AC.ColumnArray -> Either String AC.ColumnArray
widenNestedPrimitive sources plan column
  | Just source <- IntMap.lookup (AN.planAnchor plan) sources
  , Timestamp.isTimestampField source || Timestamp.isTimestampField (AN.planField plan) =
      Timestamp.projectTimestampColumn source (AN.planField plan) column
widenNestedPrimitive _ plan column = case (AT.fieldType (AN.planField plan), column) of
  (AT.AInt 64 True, AC.ColInt32 {}) -> coerceColumn (AT.AInt 64 True) column
  (AT.AFloatingPoint AT.DoublePrecision, AC.ColFloat {}) -> coerceColumn (AT.AFloatingPoint AT.DoublePrecision) column
  _ -> Right column


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
  | -- | A column's native values could not be decoded.
    ColumnDecodeFailed !Text !String
  deriving (Show, Eq)


{- | Recover the Arrow-shaped schema implied by a Parquet file's
leaf columns. Uses the Parquet converted-type / logical-type
annotations to pick the right Arrow flavour (UTF-8 vs raw
binary, Date vs Int32, Timestamp vs Int64).
-}
parquetFileArrowSchema :: PR.ParquetFile -> AT.Schema
-- Kept for callers with a known-valid schema. Use the checked variant at
-- file boundaries so malformed trees are reported through Either.
parquetFileArrowSchema = either error id . parquetFileArrowSchemaChecked


-- | Infer a complete schema, reporting malformed physical trees explicitly.
parquetFileArrowSchemaChecked :: PR.ParquetFile -> Either String AT.Schema
parquetFileArrowSchemaChecked pf = do
  let footer = PR.pfFooter pf
      fieldWithMetadata element = do
        field <- schemaElementToArrowField element
        metadata <- Geo.toArrowMetadata footer element
        Right (if V.null metadata then field else field {AT.fieldMetadata = metadata})
  nodes <- AN.plans fieldWithMetadata (P.fmSchema footer)
  Right
    AT.Schema
      { AT.arrowFields = AN.planField <$> nodes
      , AT.arrowEndianness = AT.Little
      , AT.arrowMetadata = V.empty
      , AT.arrowFeatures = V.empty
      }


{- | Map a Parquet 'P.SchemaElement' back to an Arrow leaf
'AT.Field' using its converted-type / logical-type hints.
Mirrors the inverse of 'arrowFieldToSchemaElement'.
-}
schemaElementToArrowField :: P.SchemaElement -> Either String AT.Field
schemaElementToArrowField se = do
  -- FLBA(12) timestamps are valid Parquet, but Arrow's timestamp storage is
  -- signed 64-bit. Never silently erase their logical type into raw bytes.
  encoding <- Timestamp.schemaEncoding se
  case (P.seLogicalType se, P.seConvertedType se) of
    (Just (P.LTTimestamp _ _), _) | encoding == Nothing -> requirePhysical P.PTInt64
    (Just (P.LTTime _ P.LtMillis), _) -> requirePhysical P.PTInt32
    (Just (P.LTTime _ _), _) -> requirePhysical P.PTInt64
    (_, Just P.CTTimestampMillis) -> requirePhysical P.PTInt64
    (_, Just P.CTTimestampMicros) -> requirePhysical P.PTInt64
    (_, Just P.CTTimeMillis) -> requirePhysical P.PTInt32
    (_, Just P.CTTimeMicros) -> requirePhysical P.PTInt64
    _ -> Right ()
  let !name = P.seName se
      !nullable = case P.seRepetition se of
        Just P.Optional -> True
        _ -> False
      !ty = arrowTypeFromSchemaElement se
      metadata =
        if ty == AT.AUtf8 && (P.seLogicalType se == Just P.LTJson || P.seConvertedType se == Just P.CTJson)
          then V.fromList [("ARROW:extension:name", "arrow.json"), ("ARROW:extension:metadata", "{}")]
          else V.empty
  Right $ case encoding of
    Just kind -> Timestamp.timestampField name nullable kind
    Nothing -> AT.Field name nullable ty V.empty Nothing metadata
  where
    requirePhysical expected
      | P.seType se == Just expected = Right ()
      | otherwise = Left ("Parquet.Arrow: unsupported physical storage for TIME/TIMESTAMP in " ++ show (P.seName se))


arrowTypeFromSchemaElement :: P.SchemaElement -> AT.ArrowType
arrowTypeFromSchemaElement se
  | P.seLogicalType se == Just P.LTUnsupported =
      arrowTypeFromSchemaElement (se {P.seLogicalType = Nothing, P.seConvertedType = Nothing})
  | P.seLogicalType se == Just P.LTNull = AT.ANull
  | Just (P.LTDecimal precision scale) <- P.seLogicalType se
  , P.seType se `elem` [Just P.PTInt32, Just P.PTInt64, Just P.PTByteArray, Just P.PTFixedLenByteArray] =
      if precision <= 38
        then AT.ADecimal (fromIntegral precision) (fromIntegral scale)
        else AT.ADecimal256 (fromIntegral precision) (fromIntegral scale)
  | otherwise = case P.seType se of
      Just P.PTInt96 -> AT.AFixedSizeBinary 12
      Just P.PTFixedLenByteArray -> case P.seLogicalType se of
        Just P.LTFloat16 -> AT.AFloatingPoint AT.Half
        _ -> AT.AFixedSizeBinary (maybe 0 fromIntegral (P.seTypeLength se))
      Just P.PTBoolean -> AT.ABool
      Just P.PTInt32 -> case (P.seLogicalType se, P.seConvertedType se) of
        (Just P.LTDate, _) -> AT.ADate AT.DateDay
        (Just (P.LTTime _ unit), _)
          | Just u <- arrowTimeUnit unit -> AT.ATime u 32
        (Just (P.LTInteger w isSigned), _)
          | w <= 32 -> AT.AInt (fromIntegral w) isSigned
        (_, Just P.CTDate) -> AT.ADate AT.DateDay
        (_, Just P.CTTimeMillis) -> AT.ATime AT.Millisecond 32
        (_, Just P.CTInt8) -> AT.AInt 8 True
        (_, Just P.CTInt16) -> AT.AInt 16 True
        (_, Just P.CTUInt8) -> AT.AInt 8 False
        (_, Just P.CTUInt16) -> AT.AInt 16 False
        (_, Just P.CTUInt32) -> AT.AInt 32 False
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
        (_, Just P.CTTimestampMillis) -> AT.ATimestamp AT.Millisecond (Just "UTC")
        (_, Just P.CTTimestampMicros) -> AT.ATimestamp AT.Microsecond (Just "UTC")
        (_, Just P.CTUInt64) -> AT.AInt 64 False
        _ -> AT.AInt 64 True
      Just P.PTFloat -> AT.AFloatingPoint AT.Single
      Just P.PTDouble -> AT.AFloatingPoint AT.DoublePrecision
      Just P.PTByteArray -> case (P.seLogicalType se, P.seConvertedType se) of
        (Just P.LTString, _) -> AT.AUtf8
        (Just P.LTEnum, _) -> AT.AUtf8
        -- Geometry / Geography / Variant + Json / Bson are all
        -- bytes on the wire; expose as ABinary so callers see the
        -- raw WKB / JSON / variant bytes. The LogicalType
        -- annotation survives round-trip for downstream tools.
        (Just P.LTGeometry {}, _) -> AT.ABinary
        (Just P.LTGeography {}, _) -> AT.ABinary
        (Just (P.LTVariant _), _) -> AT.ABinary
        (Just P.LTJson, _) -> AT.AUtf8
        (Just P.LTBson, _) -> AT.ABinary
        (_, Just P.CTUtf8) -> AT.AUtf8
        (_, Just P.CTEnum) -> AT.AUtf8
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
    Just fileIdx -> do
      -- Decode at the file's physical/logical type before coercion. Trying
      -- the target decoder first can silently reinterpret int bytes as
      -- floats or booleans instead of reporting an incompatible projection.
      fileFld <-
        either
          (Left . ColumnDecodeFailed (AT.fieldName fld))
          Right
          (schemaElementToArrowField (V.unsafeIndex fileLeaves fileIdx))
      let incompatible = IncompatibleType (AT.fieldName fld) (AT.fieldType fld)
      let decodingField = if canWidenBinary (AT.fieldType fileFld) (AT.fieldType fld) then fileFld {AT.fieldType = AT.fieldType fld} else fileFld
      native <- case readParquetColumn pf rgIdx fileIdx decodingField of
        Left err -> Left (ColumnDecodeFailed (AT.fieldName fld) err)
        Right col -> Right col
      if not (AT.fieldNullable fld) && AC.nullCount native > 0
        then Left incompatible
        else
          if Timestamp.isTimestampField fileFld || Timestamp.isTimestampField fld
            then either (Left . ColumnDecodeFailed (AT.fieldName fld)) Right (Timestamp.projectTimestampColumn fileFld fld native)
            else
              if AT.fieldType fld == AT.fieldType decodingField
                then Right native
                else coerceColumn (AT.fieldType fld) native `orLeft` incompatible
  where
    orLeft (Right x) _ = Right x
    orLeft (Left _) e = Left e


-- Widening offset storage preserves logical values and aliases payload bytes.
canWidenBinary :: AT.ArrowType -> AT.ArrowType -> Bool
canWidenBinary AT.AUtf8 AT.ALargeUtf8 = True
canWidenBinary AT.ABinary AT.ALargeBinary = True
canWidenBinary _ _ = False


{- | Best-effort column coercion for the projection path. The
coercion table is deliberately narrow: numeric and offset widening
(Int32 → Int64, Float → Double, Utf8/Binary → LargeUtf8/LargeBinary),
and identity reads. Returns
'Left' for unsupported coercions; callers route that to
'IncompatibleType'.
-}
coerceColumn :: AT.ArrowType -> AC.ColumnArray -> Either String AC.ColumnArray
coerceColumn target col = case (target, col) of
  (AT.ALargeUtf8, AC.ColUtf8 validity offsets bytes) -> AC.mkLargeUtf8 validity (VS.map fromIntegral offsets) bytes
  (AT.ALargeBinary, AC.ColBinary validity offsets bytes) -> AC.mkLargeBinary validity (VS.map fromIntegral offsets) bytes
  (AT.ALargeUtf8, AC.ColLargeUtf8 {}) -> Right col
  (AT.ALargeBinary, AC.ColLargeBinary {}) -> Right col
  (AT.AInt 64 True, AC.ColInt32 mv xs) ->
    AC.mkPrim AC.PInt64 mv (VS.map (fromIntegral :: Int32 -> Int64) xs)
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
readParquetColumn pf rgIdx colIdx requested = do
  chunk <- PR.columnChunkSlice pf rgIdx colIdx
  let schema = P.fmSchema (PR.pfFooter pf)
      leaves = V.filter (maybe False (const True) . P.seType) schema
  leaf <- maybe (Left "Parquet.Arrow: missing leaf schema") Right (leaves V.!? colIdx)
  metadata <-
    maybe
      (Left "Parquet.Arrow: missing column metadata")
      Right
      (P.ccMetadata (P.rgColumns (P.fmRowGroups (PR.pfFooter pf) V.! rgIdx) V.! colIdx))
  if P.seType leaf /= Just (P.cmType metadata)
    then Left "Parquet.Arrow: column metadata and schema physical types differ"
    else Right ()
  native <- schemaElementToArrowField leaf
  let codec = P.cmCodec metadata
  timestampCompatible <-
    if Timestamp.isTimestampField native || Timestamp.isTimestampField requested
      then Timestamp.fieldsCompatible native requested
      else Right True
  if (AT.fieldType requested /= AT.fieldType native && not (canWidenBinary (AT.fieldType native) (AT.fieldType requested))) || not timestampCompatible
    then Left "Parquet.Arrow: requested field differs from file type; use schema projection for coercion"
    else Right ()
  let decodingField = if canWidenBinary (AT.fieldType native) (AT.fieldType requested) then native {AT.fieldType = AT.fieldType requested} else native
  (maxRep, maxDef) <- Levels.maxLevelsForColumnPath schema (P.cmPathInSchema metadata)
  if maxRep /= 0
    then Left "Parquet.Arrow: a repeated physical leaf needs parquetRowGroupToArrow to reconstruct its containers"
    else Right ()
  let rows = P.rgNumRows (P.fmRowGroups (PR.pfFooter pf) V.! rgIdx)
  if rows < 0 || (fromIntegral rows :: Word64) > fromIntegral (maxBound :: Int) || P.cmNumValues metadata /= rows
    then Left "Parquet.Arrow: non-repeated column events differ from row count"
    else Right ()
  column <-
    if maxDef > 1 || (maxDef > 0 && P.seRepetition leaf /= Just P.Optional)
      then do
        values <- readLeveledLeaf (if isLargeBinary (AT.fieldType decodingField) then IntMap.singleton colIdx (AT.fieldType decodingField) else IntMap.empty) pf rgIdx colIdx
        AN.assemble
          (V.singleton (Right values))
          (AN.Plan (decodingField {AT.fieldNullable = True}) 0 0 0 maxDef AN.Primitive)
      else readFlat leaf metadata codec chunk decodingField
  if fromIntegral (AC.columnLength column) /= rows
    then Left "Parquet.Arrow: decoded row count differs from column metadata"
    else
      if not (AT.fieldNullable requested) && AC.nullCount column > 0
        then Left "Parquet.Arrow: non-nullable target cannot contain nulls"
        else Right column
  where
    readFlat leaf metadata codec chunk native = case (P.seType leaf, AT.fieldType native) of
      (_, AT.ANull) -> Right (AC.ColNull (fromIntegral (P.rgNumRows (P.fmRowGroups (PR.pfFooter pf) V.! rgIdx))))
      (Just P.PTInt96, _) -> annotated leaf metadata codec chunk
      (Just P.PTFixedLenByteArray, _) -> annotated leaf metadata codec chunk
      (_, AT.ADecimal _ _) -> annotated leaf metadata codec chunk
      (_, AT.ADecimal256 _ _) -> annotated leaf metadata codec chunk
      _
        | AT.fieldNullable native -> liftColumn (ChunkSource codec chunk) native
        | otherwise -> readPlainColumn codec chunk native
    annotated leaf metadata codec chunk = do
      (maxRep, maxDef) <-
        Levels.maxLevelsForColumnPath
          (P.fmSchema (PR.pfFooter pf))
          (P.cmPathInSchema metadata)
      if maxRep /= 0
        then Left "Parquet.Arrow: repeated annotated values need a nested Arrow field"
        else readAnnotatedColumn leaf codec chunk maxDef

-- Physical page decoding remains shared with the flat reader. Only defined
-- values are lifted; the schema plan scatters nulls and builds containers.


readLeveledLeaf :: IntMap.IntMap AT.ArrowType -> PR.ParquetFile -> Int -> Int -> Either String AN.LeafColumn
readLeveledLeaf overrides pf groupIndex columnIndex = do
  let footer = PR.pfFooter pf
      leaves = V.filter (maybe False (const True) . P.seType) (P.fmSchema footer)
  leaf <- maybe (Left "Parquet.Arrow: missing physical schema leaf") Right (leaves V.!? columnIndex)
  group <- maybe (Left "Parquet.Arrow: row group out of range") Right (P.fmRowGroups footer V.!? groupIndex)
  chunkMetadata <-
    maybe
      (Left "Parquet.Arrow: missing column metadata")
      Right
      (P.rgColumns group V.!? columnIndex >>= P.ccMetadata)
  if P.seType leaf /= Just (P.cmType chunkMetadata)
    then Left "Parquet.Arrow: schema and column physical types disagree"
    else Right ()
  (maxRep, maxDef) <- Levels.maxLevelsForColumnPath (P.fmSchema footer) (P.cmPathInSchema chunkMetadata)
  bytes <- PR.columnChunkSlice pf groupIndex columnIndex
  let codec = P.cmCodec chunkMetadata
      nativeType = IntMap.findWithDefault (arrowTypeFromSchemaElement leaf) columnIndex overrides
      completeWith :: (a -> Either String AC.ColumnArray) -> Either String (PR.ColumnLevels a) -> Either String AN.LeafColumn
      completeWith lift decoded = do
        levels <- decoded
        let reps = PR.clRepetition levels
            defs = PR.clDefinition levels
            rows = VP.foldl' (\n rep -> if rep == 0 then n + 1 else n) (0 :: Int64) reps
        if fromIntegral (VP.length defs) /= P.cmNumValues chunkMetadata || rows /= P.rgNumRows group
          then Left "Parquet.Arrow: physical event/row count differs from column metadata"
          else do
            values <- lift (PR.clValues levels)
            Right (AN.LeafColumn reps defs values)
      complete wrap = completeWith (liftDefined leaf . wrap)
  case P.cmType chunkMetadata of
    physical
      | nativeScalarPhysical nativeType == Just physical ->
          completeWith (scalarBuffersColumn nativeType) (PR.readScalarBuffersLevels physical codec maxRep maxDef bytes)
    P.PTInt32 -> complete PW.ColInt32 (PR.readInt32Levels codec maxRep maxDef bytes)
    P.PTInt64 -> complete PW.ColInt64 (PR.readInt64Levels codec maxRep maxDef bytes)
    P.PTFloat -> complete PW.ColFloat (PR.readFloatLevels codec maxRep maxDef bytes)
    P.PTDouble -> complete PW.ColDouble (PR.readDoubleLevels codec maxRep maxDef bytes)
    P.PTBoolean -> completeWith boolBuffersColumn (PR.readBoolBuffersLevels codec maxRep maxDef bytes)
    P.PTByteArray
      | isLargeBinary nativeType ->
          completeWith (largeBinaryBuffersColumn nativeType) (PR.readLargeBinaryBuffersLevels codec maxRep maxDef bytes)
      | isNativeBinary (arrowTypeFromSchemaElement leaf) ->
          completeWith (binaryBuffersColumn (arrowTypeFromSchemaElement leaf)) (PR.readBinaryBuffersLevels codec maxRep maxDef bytes)
      | otherwise -> complete PW.ColByteArray (PR.readByteArrayLevels codec maxRep maxDef bytes)
    P.PTInt96 -> complete PW.ColByteArray (PR.readFixedLevels 12 codec maxRep maxDef bytes)
    P.PTFixedLenByteArray -> case P.seTypeLength leaf of
      Just width -> complete PW.ColByteArray (PR.readFixedLevels (fromIntegral width) codec maxRep maxDef bytes)
      Nothing -> Left "Parquet.Arrow: fixed binary leaf is missing its width"


liftDefined :: P.SchemaElement -> PW.ColumnData -> Either String AC.ColumnArray
liftDefined leaf values = case (arrowTypeFromSchemaElement leaf, values) of
  (AT.ANull, _) -> Right (AC.ColNull (PW.columnDataLength values))
  (AT.AInt 8 True, PW.ColInt32 xs) -> buildDenseChecked AC.PInt8 (VP.length xs) (VP.unsafeIndex xs) (\input value -> fromIntegral value == input) fromIntegral
  (AT.AInt 16 True, PW.ColInt32 xs) -> buildDenseChecked AC.PInt16 (VP.length xs) (VP.unsafeIndex xs) (\input value -> fromIntegral value == input) fromIntegral
  (AT.AInt 32 True, PW.ColInt32 xs) -> Right (AC.primColumn AC.PInt32 (primToStorable xs))
  (AT.AInt 64 True, PW.ColInt64 xs) -> Right (AC.primColumn AC.PInt64 (primToStorable xs))
  (AT.AInt 8 False, PW.ColInt32 xs) -> buildDenseChecked AC.PUInt8 (VP.length xs) (VP.unsafeIndex xs) (\input value -> fromIntegral value == input) fromIntegral
  (AT.AInt 16 False, PW.ColInt32 xs) -> buildDenseChecked AC.PUInt16 (VP.length xs) (VP.unsafeIndex xs) (\input value -> fromIntegral value == input) fromIntegral
  (AT.AInt 32 False, PW.ColInt32 xs) -> Right (AC.primColumn AC.PUInt32 (VS.generate (VP.length xs) (fromIntegral . VP.unsafeIndex xs)))
  (AT.AInt 64 False, PW.ColInt64 xs) -> Right (AC.primColumn AC.PUInt64 (VS.generate (VP.length xs) (fromIntegral . VP.unsafeIndex xs)))
  (AT.AFloatingPoint AT.Single, PW.ColFloat xs) -> Right (AC.primColumn AC.PFloat (primToStorable xs))
  (AT.AFloatingPoint AT.DoublePrecision, PW.ColDouble xs) -> Right (AC.primColumn AC.PDouble (primToStorable xs))
  (AT.ADate AT.DateDay, PW.ColInt32 xs) -> Right (AC.primColumn AC.PDate32 (primToStorable xs))
  (AT.ATime _ 32, PW.ColInt32 xs) -> Right (AC.primColumn AC.PTime32 (primToStorable xs))
  (AT.ATime _ 64, PW.ColInt64 xs) -> Right (AC.primColumn AC.PTime64 (primToStorable xs))
  (AT.ATimestamp _ _, PW.ColInt64 xs) -> Right (AC.primColumn AC.PTimestamp (primToStorable xs))
  (AT.ABool, PW.ColBool xs) -> Right (AC.fromBools xs)
  (AT.AUtf8, PW.ColByteArray xs) -> utf8FromBinary (AC.fromByteStrings xs)
  (AT.ABinary, PW.ColByteArray xs) -> Right (AC.fromByteStrings xs)
  (AT.AFixedSizeBinary width, PW.ColByteArray xs) -> do
    column <- fixedSizeBinary width xs
    encoding <- Timestamp.schemaEncoding leaf
    maybe (Right column) (\kind -> Timestamp.validateTimestampColumn kind column) encoding
  (AT.AFloatingPoint AT.Half, PW.ColByteArray xs) ->
    buildDenseChecked
      AC.PFloat16
      (V.length xs)
      (V.unsafeIndex xs)
      (\bytes _ -> BS.length bytes == 2)
      (\bytes -> AC.Float16 (fromIntegral (BSU.unsafeIndex bytes 0) .|. (fromIntegral (BSU.unsafeIndex bytes 1) `shiftL` 8)))
  (AT.ADecimal precision scale, _)
    | precision >= 1 && precision <= 38 ->
        let bound = 10 ^ precision - 1 :: Integer
            low = AC.decimal128FromInteger (negate bound)
            high = AC.decimal128FromInteger bound
        in decimal
             (AC.PDecimal128 precision scale)
             16
             decimal128Bytes
             (\value -> compare128 value low /= LT && compare128 value high /= GT)
             (\n -> AC.Decimal128 (fromIntegral n) (if n < 0 then maxBound else 0))
  (AT.ADecimal256 precision scale, _)
    | precision >= 1 && precision <= 76 ->
        let bound = 10 ^ precision - 1 :: Integer
            low = AC.decimal256FromInteger (negate bound)
            high = AC.decimal256FromInteger bound
        in decimal
             (AC.PDecimal256 precision scale)
             32
             decimal256Bytes
             (\value -> compare256 value low /= LT && compare256 value high /= GT)
             (\n -> let sign = if n < 0 then maxBound else 0 in AC.Decimal256 (fromIntegral n) sign sign sign)
  _ -> Left "Parquet.Arrow: unsupported physical/logical leaf combination"
  where
    decimal :: Storable d => AC.PrimType d -> Int -> (ByteString -> d) -> (d -> Bool) -> (Int64 -> d) -> Either String AC.ColumnArray
    decimal primitive width decode valid fromInt = case values of
      PW.ColInt32 xs ->
        buildDenseChecked
          primitive
          (VP.length xs)
          (VP.unsafeIndex xs)
          (\_ -> valid)
          (fromInt . fromIntegral)
      PW.ColInt64 xs -> buildDenseChecked primitive (VP.length xs) (VP.unsafeIndex xs) (\_ -> valid) fromInt
      PW.ColByteArray xs ->
        buildDenseChecked
          primitive
          (V.length xs)
          (V.unsafeIndex xs)
          (\bytes value -> not (BS.null bytes) && fitsSignedBytes width bytes && valid value)
          decode
      _ -> Left "Parquet.Arrow: DECIMAL has an invalid physical type"


-- Required single-page PLAIN scalars can already have Arrow's layout.
-- Numeric buffers are aliased (copied once if misaligned), booleans stay
-- bit-packed, and binary values go directly to Arrow offsets/data. Other
-- encodings retain the generic dictionary/delta/multi-page reader.
readPlainColumn :: P.Compression -> ByteString -> AT.Field -> Either String AC.ColumnArray
readPlainColumn codec chunk fld | BS.null chunk = liftColumn (ChunkSource codec chunk) fld
readPlainColumn codec chunk fld = do
  (header, afterHeader) <- Page.readPageHeaderCheckedAt chunk 0
  let onePage =
        maybe
          False
          (\size -> size >= 0 && fromIntegral size == BS.length chunk - afterHeader)
          (Page.phCompressedPageSize header)
      fallback = liftColumn (ChunkSource codec chunk) fld
      binary n raw = do
        buffers <- PR.decodePlainBinaryBuffers n raw
        let construct = if AT.fieldType fld == AT.AUtf8 then AC.mkUtf8 else AC.mkBinary
        construct Nothing (PR.binaryOffsets buffers) (PR.binaryData buffers)
      fixed :: AC.PrimType a -> Int -> Int -> ByteString -> Either String AC.ColumnArray
      fixed tag width n raw
        | targetByteOrder /= LittleEndian = fallback
        | n < 0 || n > BS.length raw `quot` width || n * width /= BS.length raw =
            Left "Parquet.Arrow: PLAIN primitive count/size mismatch"
        | otherwise = AC.withPrim tag (Right (AC.primColumn tag (Buffer.bytesToStorable raw)))
      build n raw = case AT.fieldType fld of
        AT.AUtf8 -> binary n raw
        AT.ABinary -> binary n raw
        AT.AInt 32 True -> fixed AC.PInt32 4 n raw
        AT.AInt 64 True -> fixed AC.PInt64 8 n raw
        AT.AInt 32 False -> fixed AC.PUInt32 4 n raw
        AT.AInt 64 False -> fixed AC.PUInt64 8 n raw
        AT.AFloatingPoint AT.Single -> fixed AC.PFloat 4 n raw
        AT.AFloatingPoint AT.DoublePrecision -> fixed AC.PDouble 8 n raw
        AT.ADate AT.DateDay -> fixed AC.PDate32 4 n raw
        AT.ATime _ 32 -> fixed AC.PTime32 4 n raw
        AT.ATime _ 64 -> fixed AC.PTime64 8 n raw
        AT.ATimestamp _ _ -> fixed AC.PTimestamp 8 n raw
        AT.ABool
          | n >= 0
          , (n + 7) `quot` 8 == BS.length raw ->
              AC.mkBool Nothing =<< AC.mkBitmap raw 0 n
          | otherwise -> Left "Parquet.Arrow: PLAIN BOOLEAN count/size mismatch"
        _ -> fallback
  case Page.phType header of
    Page.PtDataPage page
      | onePage
      , Page.dphEncoding page == 0 -> do
          (_, _, raw, _) <- PR.decompressDataPageBody codec chunk 0
          build (fromIntegral (Page.dphNumValues page)) raw
    Page.PtDataPageV2 page
      | onePage
      , Page.dph2Encoding page == 0
      , Page.dph2NumNulls page == 0
      , Page.dph2RepLevelsLen page == 0
      , Page.dph2DefLevelsLen page == 0 -> do
          (_, _, _, _, raw, _) <- PR.decompressDataPageV2Body codec 0 0 chunk 0
          build (fromIntegral (Page.dph2NumValues page)) raw
    _ -> fallback


boolBuffersColumn :: PR.BoolBuffers -> Either String AC.ColumnArray
boolBuffersColumn values = AC.mkBool Nothing =<< AC.mkBitmap (PR.boolData values) 0 (PR.boolCount values)


-- Both indexed and sequential reads use the same checked buffer decoder.
binaryBuffersColumn :: AT.ArrowType -> PR.BinaryBuffers -> Either String AC.ColumnArray
binaryBuffersColumn typ buffers = case typ of
  AT.AUtf8 -> AC.mkUtf8 Nothing (PR.binaryOffsets buffers) (PR.binaryData buffers)
  AT.ABinary -> AC.mkBinary Nothing (PR.binaryOffsets buffers) (PR.binaryData buffers)
  _ -> Left "Parquet.Arrow: binary buffers need a native binary or UTF-8 field"


largeBinaryBuffersColumn :: AT.ArrowType -> PR.LargeBinaryBuffers -> Either String AC.ColumnArray
largeBinaryBuffersColumn typ buffers = case typ of
  AT.ALargeUtf8 -> AC.mkLargeUtf8 Nothing (PR.binaryOffsets buffers) (PR.binaryData buffers)
  AT.ALargeBinary -> AC.mkLargeBinary Nothing (PR.binaryOffsets buffers) (PR.binaryData buffers)
  _ -> Left "Parquet.Arrow: large binary buffers need a LargeBinary or LargeUtf8 field"


isLargeBinary :: AT.ArrowType -> Bool
isLargeBinary AT.ALargeUtf8 = True
isLargeBinary AT.ALargeBinary = True
isLargeBinary _ = False


isNativeBinary :: AT.ArrowType -> Bool
isNativeBinary AT.AUtf8 = True
isNativeBinary AT.ABinary = True
isNativeBinary _ = False


nativeScalarPhysical :: AT.ArrowType -> Maybe P.ParquetType
nativeScalarPhysical typ = case typ of
  AT.ADecimal32 _ _ -> Just P.PTInt32
  AT.ADecimal64 _ _ -> Just P.PTInt64
  AT.AInt 32 _ -> Just P.PTInt32
  AT.AInt 64 _ -> Just P.PTInt64
  AT.AFloatingPoint AT.Single -> Just P.PTFloat
  AT.AFloatingPoint AT.DoublePrecision -> Just P.PTDouble
  AT.ADate AT.DateDay -> Just P.PTInt32
  AT.ADate AT.DateMillisecond -> Just P.PTInt64
  AT.ATime _ 32 -> Just P.PTInt32
  AT.ATime _ 64 -> Just P.PTInt64
  AT.ATimestamp _ _ -> Just P.PTInt64
  AT.ADuration _ -> Just P.PTInt64
  _ -> Nothing


scalarBuffersColumn :: AT.ArrowType -> PR.ScalarBuffers -> Either String AC.ColumnArray
scalarBuffersColumn typ buffers = case typ of
  AT.ADecimal32 precision scale
    | precision >= 1 && precision <= 9 ->
        let bound = 10 ^ precision - 1 :: Int32
            values = Buffer.bytesToStorable (PR.scalarData buffers)
        in if VS.all (\value -> value >= negate bound && value <= bound) values
             then Right (AC.primColumn (AC.PDecimal32 precision scale) values)
             else Left "Parquet.Arrow: typed Decimal32 exceeds declared precision"
  AT.ADecimal64 precision scale
    | precision >= 10 && precision <= 18 ->
        let bound = 10 ^ precision - 1 :: Int64
            values = Buffer.bytesToStorable (PR.scalarData buffers)
        in if VS.all (\value -> value >= negate bound && value <= bound) values
             then Right (AC.primColumn (AC.PDecimal64 precision scale) values)
             else Left "Parquet.Arrow: typed Decimal64 exceeds declared precision"
  AT.AInt 32 True -> fixed AC.PInt32
  AT.AInt 64 True -> fixed AC.PInt64
  AT.AInt 32 False -> fixed AC.PUInt32
  AT.AInt 64 False -> fixed AC.PUInt64
  AT.AFloatingPoint AT.Single -> fixed AC.PFloat
  AT.AFloatingPoint AT.DoublePrecision -> fixed AC.PDouble
  AT.ADate AT.DateDay -> fixed AC.PDate32
  AT.ADate AT.DateMillisecond -> fixed AC.PDate64
  AT.ATime _ 32 -> fixed AC.PTime32
  AT.ATime _ 64 -> fixed AC.PTime64
  AT.ATimestamp _ _ -> fixed AC.PTimestamp
  AT.ADuration _ -> fixed AC.PDuration
  _ -> Left "Parquet.Arrow: scalar buffers need a matching native primitive field"
  where
    fixed :: AC.PrimType a -> Either String AC.ColumnArray
    fixed tag = AC.withPrim tag (Right (AC.primColumn tag (Buffer.bytesToStorable (PR.scalarData buffers))))


{- | A whole column chunk, decoded through generic per-page dispatchers
which handle every
encoding the spec defines for the physical type (PLAIN,
dictionary, DELTA_*, BYTE_STREAM_SPLIT) and both DATA_PAGE and
DATA_PAGE_V2.
-}
data Source
  = ChunkSource P.Compression ByteString


{- | Decode a flat column from a 'Source' at the Arrow target
type. Required fields read the dense value stream; nullable
fields read the definition-level form. The optional readers
take (max_repetition_level, max_definition_level); for a flat
optional primitive these are (0, 1).

Native scalars retain validated source pages until the final buffer is
built; definition levels scatter nullable values through the shared
assembler. Narrow casts use the primitive-vector readers. Byte arrays
are copied once into the offsets/data buffers.
-}
liftColumn :: Source -> AT.Field -> Either String AC.ColumnArray
liftColumn (ChunkSource codec chunk) fld
  | Just physical <- nativeScalarPhysical (AT.fieldType fld) =
      if AT.fieldNullable fld
        then do
          levels <- PR.readScalarBuffersLevels physical codec 0 1 chunk
          values <- scalarBuffersColumn (AT.fieldType fld) (PR.clValues levels)
          let leaf = AN.LeafColumn (PR.clRepetition levels) (PR.clDefinition levels) values
          AN.assemble (V.singleton (Right leaf)) (AN.Plan fld 0 0 0 1 AN.Primitive)
        else scalarBuffersColumn (AT.fieldType fld) =<< PR.readScalarBuffersColumnChunk physical codec chunk
liftColumn src fld = case AT.fieldType fld of
  AT.AInt 8 True -> narrow32 AC.PInt8 (\input value -> fromIntegral value == input) fromIntegral
  AT.AInt 16 True -> narrow32 AC.PInt16 (\input value -> fromIntegral value == input) fromIntegral
  AT.AInt 32 True -> int32 AC.PInt32
  AT.AInt 64 True -> int64 AC.PInt64
  AT.AInt 8 False -> narrow32 AC.PUInt8 (\input value -> fromIntegral value == input) fromIntegral
  AT.AInt 16 False -> narrow32 AC.PUInt16 (\input value -> fromIntegral value == input) fromIntegral
  AT.AInt 32 False -> cast32 AC.PUInt32 fromIntegral
  AT.AInt 64 False -> cast64 AC.PUInt64 fromIntegral
  AT.AFloatingPoint AT.Single
    | nullable -> AC.fromMaybes AC.PFloat <$> optFloat src
    | otherwise -> AC.primColumn AC.PFloat . primToStorable <$> reqFloat src
  AT.AFloatingPoint AT.DoublePrecision
    | nullable -> AC.fromMaybes AC.PDouble <$> optDouble src
    | otherwise -> AC.primColumn AC.PDouble . primToStorable <$> reqDouble src
  AT.ABool -> case src of
    ChunkSource codec chunk ->
      if nullable
        then do
          levels <- PR.readBoolBuffersLevels codec 0 1 chunk
          values <- boolBuffersColumn (PR.clValues levels)
          let leaf = AN.LeafColumn (PR.clRepetition levels) (PR.clDefinition levels) values
          AN.assemble (V.singleton (Right leaf)) (AN.Plan fld 0 0 0 1 AN.Primitive)
        else boolBuffersColumn =<< PR.readBoolBuffersColumnChunk codec chunk
  AT.AUtf8 -> binary
  AT.ABinary -> binary
  AT.ALargeUtf8 -> largeBinary
  AT.ALargeBinary -> largeBinary
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
          =<< PR.readGenericFixedLenByteArrayColumnChunk w codec chunk
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
    largeBinary = case src of
      ChunkSource codec chunk
        | nullable -> do
            levels <- PR.readLargeBinaryBuffersLevels codec 0 1 chunk
            values <- largeBinaryBuffersColumn (AT.fieldType fld) (PR.clValues levels)
            let physical = AN.LeafColumn (PR.clRepetition levels) (PR.clDefinition levels) values
            AN.assemble (V.singleton (Right physical)) (AN.Plan fld 0 0 0 1 AN.Primitive)
        | otherwise -> largeBinaryBuffersColumn (AT.fieldType fld) =<< PR.readLargeBinaryBuffersColumnChunk codec chunk
    binary = case src of
      ChunkSource codec chunk
        | nullable -> do
            levels <- PR.readBinaryBuffersLevels codec 0 1 chunk
            values <- binaryBuffersColumn (AT.fieldType fld) (PR.clValues levels)
            let physical = AN.LeafColumn (PR.clRepetition levels) (PR.clDefinition levels) values
            AN.assemble (V.singleton (Right physical)) (AN.Plan fld 0 0 0 1 AN.Primitive)
        | otherwise -> binaryBuffersColumn (AT.fieldType fld) =<< PR.readBinaryBuffersColumnChunk codec chunk
    int32 :: AC.PrimType Int32 -> Either String AC.ColumnArray
    int32 t
      | nullable = AC.fromMaybes t <$> optInt32 src
      | otherwise = AC.primColumn t . primToStorable <$> reqInt32 src
    int64 :: AC.PrimType Int64 -> Either String AC.ColumnArray
    int64 t
      | nullable = AC.fromMaybes t <$> optInt64 src
      | otherwise = AC.primColumn t . primToStorable <$> reqInt64 src
    -- Decode only defined values, range-check while narrowing, and scatter
    -- nulls through the same assembler as nested leaves. No Maybe per row.
    narrow32 :: AC.PrimType a -> (Int32 -> a -> Bool) -> (Int32 -> a) -> Either String AC.ColumnArray
    narrow32 tag valid convert = AC.withPrim tag $ case src of
      ChunkSource codec bytes -> do
        let maxDef = if nullable then 1 else 0
        levels <- PR.readInt32Levels codec 0 maxDef bytes
        let values = PR.clValues levels
        dense <- buildDenseChecked tag (VP.length values) (VP.unsafeIndex values) valid convert
        let leaf = AN.LeafColumn (PR.clRepetition levels) (PR.clDefinition levels) dense
        AN.assemble (V.singleton (Right leaf)) (AN.Plan fld 0 0 0 maxDef AN.Primitive)
    {-# INLINE narrow32 #-}
    cast32 :: AC.PrimType a -> (Int32 -> a) -> Either String AC.ColumnArray
    cast32 t f =
      AC.withPrim t $
        if nullable
          then AC.fromMaybes t . V.map (fmap f) <$> optInt32 src
          else do
            xs <- reqInt32 src
            Right (AC.primColumn t (VS.generate (VP.length xs) (f . VP.unsafeIndex xs)))
    cast64 :: AC.PrimType a -> (Int64 -> a) -> Either String AC.ColumnArray
    cast64 t f =
      AC.withPrim t $
        if nullable
          then AC.fromMaybes t . V.map (fmap f) <$> optInt64 src
          else do
            xs <- reqInt64 src
            Right (AC.primColumn t (VS.generate (VP.length xs) (f . VP.unsafeIndex xs)))


reqInt32 :: Source -> Either String (VP.Vector Int32)
reqInt32 (ChunkSource c bs) = PR.readGenericInt32ColumnChunk c bs


reqInt64 :: Source -> Either String (VP.Vector Int64)
reqInt64 (ChunkSource c bs) = PR.readGenericInt64ColumnChunk c bs


reqFloat :: Source -> Either String (VP.Vector Float)
reqFloat (ChunkSource c bs) = PR.readGenericFloatColumnChunk c bs


reqDouble :: Source -> Either String (VP.Vector Double)
reqDouble (ChunkSource c bs) = PR.readGenericDoubleColumnChunk c bs


optInt32 :: Source -> Either String (V.Vector (Maybe Int32))
optInt32 (ChunkSource c bs) = PR.readGenericInt32OptionalColumnChunk c 0 1 bs


optInt64 :: Source -> Either String (V.Vector (Maybe Int64))
optInt64 (ChunkSource c bs) = PR.readGenericInt64OptionalColumnChunk c 0 1 bs


optFloat :: Source -> Either String (V.Vector (Maybe Float))
optFloat (ChunkSource c bs) = PR.readGenericFloatOptionalColumnChunk c 0 1 bs


optDouble :: Source -> Either String (V.Vector (Maybe Double))
optDouble (ChunkSource c bs) = PR.readGenericDoubleOptionalColumnChunk c 0 1 bs


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
invalid UTF-8 is rejected rather than silently replacing bytes with
U+FFFD and changing the stored data.
-}
utf8FromBinary :: AC.ColumnArray -> Either String AC.ColumnArray
utf8FromBinary col = case col of
  AC.ColBinary mv offs dat -> AC.mkUtf8 mv offs dat
  _ -> Left ("Parquet.Arrow: expected a binary column, got " <> AC.columnTag col)


-- Decode schema-annotated primitives through the same dictionary / page
-- machinery as ordinary columns. Definition levels come from the full path.
readAnnotatedColumn
  :: P.SchemaElement -> P.Compression -> ByteString -> Int -> Either String AC.ColumnArray
readAnnotatedColumn leaf codec chunk maxDef = case arrowTypeFromSchemaElement leaf of
  AT.ADecimal precision scale
    | precision >= 1 && precision <= 38 ->
        let bound = 10 ^ precision - 1 :: Integer
            low = AC.decimal128FromInteger (negate bound)
            high = AC.decimal128FromInteger bound
            valid value = compare128 value low /= LT && compare128 value high /= GT
        in decimalAs
             (AC.PDecimal128 precision scale)
             16
             decimal128Bytes
             valid
             (\n -> AC.Decimal128 (fromIntegral n) (if n < 0 then maxBound else 0))
             (AC.Decimal128 0 0)
  AT.ADecimal256 precision scale
    | precision >= 1 && precision <= 76 ->
        let bound = 10 ^ precision - 1 :: Integer
            low = AC.decimal256FromInteger (negate bound)
            high = AC.decimal256FromInteger bound
            valid value = compare256 value low /= LT && compare256 value high /= GT
        in decimalAs
             (AC.PDecimal256 precision scale)
             32
             decimal256Bytes
             valid
             (\n -> let sign = if n < 0 then maxBound else 0 in AC.Decimal256 (fromIntegral n) sign sign sign)
             (AC.Decimal256 0 0 0 0)
  AT.AFixedSizeBinary width -> do
    column <- if nullable
      then fixedOptional width >>= AC.fromMaybeFixedSizeBinary width
      else fixedRequired width >>= fixedSizeBinary width
    encoding <- Timestamp.schemaEncoding leaf
    maybe (Right column) (\kind -> Timestamp.validateTimestampColumn kind column) encoding
  AT.AFloatingPoint AT.Half ->
    if nullable
      then do
        values <- fixedOptional 2
        buildOptionalChecked AC.PFloat16 values (\bytes _ -> BS.length bytes == 2) half (AC.Float16 0)
      else do
        values <- fixedRequired 2
        buildDenseChecked
          AC.PFloat16
          (V.length values)
          (V.unsafeIndex values)
          (\bytes _ -> BS.length bytes == 2)
          half
  _ -> Left "Parquet.Arrow: annotated primitive is not representable by this Arrow type"
  where
    nullable = maxDef > 0
    fixedRequired width = PR.readGenericFixedLenByteArrayColumnChunk width codec chunk
    fixedOptional width = PR.readGenericFixedLenByteArrayOptionalColumnChunk width codec 0 maxDef chunk
    half bytes =
      AC.Float16
        ( fromIntegral (BSU.unsafeIndex bytes 0)
            .|. (fromIntegral (BSU.unsafeIndex bytes 1) `shiftL` 8)
        )
    decimalAs
      :: Storable d
      => AC.PrimType d
      -> Int
      -> (ByteString -> d)
      -> (d -> Bool)
      -> (Int64 -> d)
      -> d
      -> Either String AC.ColumnArray
    decimalAs primitive width decode valid fromInt zero = case P.seType leaf of
      Just P.PTInt32 ->
        if nullable
          then do
            values <- PR.readGenericInt32OptionalColumnChunk codec 0 maxDef chunk
            buildOptionalChecked primitive values (\_ -> valid) (fromInt . fromIntegral) zero
          else do
            values <- PR.readGenericInt32ColumnChunk codec chunk
            buildDenseChecked
              primitive
              (VP.length values)
              (VP.unsafeIndex values)
              (\_ -> valid)
              (fromInt . fromIntegral)
      Just P.PTInt64 ->
        if nullable
          then do
            values <- PR.readGenericInt64OptionalColumnChunk codec 0 maxDef chunk
            buildOptionalChecked primitive values (\_ -> valid) fromInt zero
          else do
            values <- PR.readGenericInt64ColumnChunk codec chunk
            buildDenseChecked primitive (VP.length values) (VP.unsafeIndex values) (\_ -> valid) fromInt
      Just P.PTByteArray ->
        if nullable
          then do
            values <- PR.readGenericByteArrayOptionalColumnChunk codec 0 maxDef chunk
            buildOptionalChecked primitive values (validBytes width valid) decode zero
          else do
            values <- PR.readGenericByteArrayColumnChunk codec chunk
            buildDenseChecked primitive (V.length values) (V.unsafeIndex values) (validBytes width valid) decode
      Just P.PTFixedLenByteArray -> case P.seTypeLength leaf of
        Just physicalWidth ->
          if nullable
            then do
              values <- fixedOptional (fromIntegral physicalWidth)
              buildOptionalChecked primitive values (validBytes width valid) decode zero
            else do
              values <- fixedRequired (fromIntegral physicalWidth)
              buildDenseChecked primitive (V.length values) (V.unsafeIndex values) (validBytes width valid) decode
        Nothing -> Left "Parquet.Arrow: missing decimal fixed byte width"
      _ -> Left "Parquet.Arrow: DECIMAL physical type is unsupported"
    validBytes width valid bytes value =
      not (BS.null bytes)
        && fitsSignedBytes width bytes
        && valid value


-- Page-level Either only: the per-value loop uses a Boolean branch and
-- writes directly to a storable vector. No per-row Either/Maybe is created.
buildDenseChecked
  :: Storable b
  => AC.PrimType b
  -> Int
  -> (Int -> a)
  -> (a -> b -> Bool)
  -> (a -> b)
  -> Either String AC.ColumnArray
buildDenseChecked primitive count get valid convert = runST $ do
  output <- VSM.new count
  let loop !i
        | i == count = pure True
        | otherwise =
            let input = get i
                value = convert input
            in if valid input value
                 then VSM.unsafeWrite output i value >> loop (i + 1)
                 else pure False
  good <- loop 0
  if good
    then Right . AC.primColumn primitive <$> VS.unsafeFreeze output
    else pure (Left "Parquet.Arrow: annotated value exceeds its physical/logical range")
{-# INLINE buildDenseChecked #-}


buildOptionalChecked
  :: Storable b
  => AC.PrimType b
  -> V.Vector (Maybe a)
  -> (a -> b -> Bool)
  -> (a -> b)
  -> b
  -> Either String AC.ColumnArray
buildOptionalChecked primitive values valid convert zero = do
  vector <- runST $ do
    output <- VSM.new count
    let loop !i
          | i == count = pure True
          | otherwise = case V.unsafeIndex values i of
              Nothing -> VSM.unsafeWrite output i zero >> loop (i + 1)
              Just input ->
                let value = convert input
                in if valid input value
                     then VSM.unsafeWrite output i value >> loop (i + 1)
                     else pure False
    good <- loop 0
    if good
      then Right <$> VS.unsafeFreeze output
      else pure (Left "Parquet.Arrow: annotated value exceeds its physical/logical range")
  AC.mkPrim primitive (AC.validityGenerate count present) vector
  where
    count = V.length values
    present i = case V.unsafeIndex values i of Just _ -> True; Nothing -> False
{-# INLINE buildOptionalChecked #-}


-- Read a sign-extended limb from the end of a big-endian two's-complement
-- byte string. Decimal precision is checked against precomputed word bounds;
-- Integer is used once per column, never inside the per-value loop.
decimalLimb :: ByteString -> Int -> Word64
decimalLimb bytes limb = go 0 0
  where
    count = BS.length bytes
    extension = if count > 0 && BSU.unsafeIndex bytes 0 .&. 0x80 /= 0 then 0xff else 0
    start = count - (limb + 1) * 8
    go !j !acc
      | j == 8 = acc
      | otherwise =
          let index = start + j
              byte = if index < 0 then extension else BSU.unsafeIndex bytes index
          in go (j + 1) ((acc `shiftL` 8) .|. fromIntegral byte)
{-# INLINE decimalLimb #-}


decimal128Bytes :: ByteString -> AC.Decimal128
decimal128Bytes bytes = AC.Decimal128 (decimalLimb bytes 0) (decimalLimb bytes 1)
{-# INLINE decimal128Bytes #-}


decimal256Bytes :: ByteString -> AC.Decimal256
decimal256Bytes bytes =
  AC.Decimal256
    (decimalLimb bytes 0)
    (decimalLimb bytes 1)
    (decimalLimb bytes 2)
    (decimalLimb bytes 3)
{-# INLINE decimal256Bytes #-}


fitsSignedBytes :: Int -> ByteString -> Bool
fitsSignedBytes width bytes
  | extra <= 0 = True
  | otherwise = go 0
  where
    extra = BS.length bytes - width
    sign = if BSU.unsafeIndex bytes extra .&. 0x80 /= 0 then 0xff else 0
    go !i
      | i == extra = True
      | BSU.unsafeIndex bytes i == sign = go (i + 1)
      | otherwise = False
{-# INLINE fitsSignedBytes #-}


compare128 :: AC.Decimal128 -> AC.Decimal128 -> Ordering
compare128 (AC.Decimal128 lo hi) (AC.Decimal128 lo' hi') =
  case compare (fromIntegral hi :: Int64) (fromIntegral hi') of
    EQ -> compare lo lo'
    order -> order
{-# INLINE compare128 #-}


compare256 :: AC.Decimal256 -> AC.Decimal256 -> Ordering
compare256 (AC.Decimal256 a b c d) (AC.Decimal256 a' b' c' d') =
  case compare (fromIntegral d :: Int64) (fromIntegral d') of
    EQ -> case compare c c' of
      EQ -> case compare b b' of EQ -> compare a a'; order -> order
      order -> order
    order -> order
{-# INLINE compare256 #-}


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

-- ============================================================
-- Arrow → Parquet.Nested (nested types)
-- ============================================================

{- | Map an Arrow 'AT.Field' onto a 'PN.NestedSchema' tree
suitable for 'Parquet.HighLevel.encodeParquetNested'. Handles
flat primitives (wraps them in 'PN.NSRequired' / 'PN.NSOptional'
based on 'fieldNullable'), struct, list and map types. Integer and
temporal annotations match the flat bridge. Pass this same field to
'columnArrayToNestedRows' so schema and payload normalization agree.
-}
arrowFieldToNestedSchema
  :: AT.Field -> Either String PN.NestedSchema
arrowFieldToNestedSchema f
  | Just _ <- AT.fieldDictionary f = Left "Parquet.Arrow: nested dictionary fields are not supported"
  | VariantSchema.isVariantField f = do
      storage <- VariantSchema.compileVariantStorageSchema f
      pure ((if AT.fieldNullable f then PN.NSOptional else PN.NSRequired) (PN.NSVariantTyped storage))
  | otherwise = do
      core <- case AT.fieldType f of
        _ | FileArrow.isFileField f -> PN.NSFile <$> FileArrow.fileSchemaFromField f
        AT.AStruct ->
          PN.NSStruct
            <$> V.mapM
              (\child -> (,) (AT.fieldName child) <$> arrowFieldToNestedSchema child)
              (AT.fieldChildren f)
        AT.AList -> listSchema
        AT.ALargeList -> listSchema
        AT.AMap _ -> case V.toList (AT.fieldChildren f) of
          [entries]
            | AT.fieldType entries == AT.AStruct
            , not (AT.fieldNullable entries)
            , [key, value] <- V.toList (AT.fieldChildren entries)
            , not (AT.fieldNullable key) ->
                PN.NSMap <$> arrowFieldToNestedSchema key <*> arrowFieldToNestedSchema value
          _ -> Left "Parquet.Arrow: a map needs a required entries struct and required key"
        _ -> PN.NSPrimitive <$> primitiveNestedType f
      Right (if AT.fieldNullable f then PN.NSOptional core else PN.NSRequired core)
  where
    listSchema = case V.toList (AT.fieldChildren f) of
      [child] -> PN.NSList <$> arrowFieldToNestedSchema child
      _ -> Left "Parquet.Arrow: a list must have exactly one child field"


-- Reuse flat lowering's type/unit decisions, including its explicit
-- rejection of durations and invalid time-unit/width combinations.
primitiveNestedType :: AT.Field -> Either String PN.LeafType
primitiveNestedType field
  | Just (DecimalParameters precision scale) <- decimalParameters (AT.fieldType field) = do
      AT.validateDecimalType (AT.fieldType field)
      if scale < 0 || scale > precision
        then Left "Parquet.Arrow: Parquet DECIMAL scale must be between zero and precision"
        else Right (PN.LtDecimal (fromIntegral precision) (fromIntegral scale))
  | Timestamp.isTimestampField field = do
      encoding <- Timestamp.timestampEncoding field
      case encoding of
        Just (Timestamp.SignedTimestamp96 adjusted unit) -> Right (PN.LtTimestamp96 adjusted unit)
        Just Timestamp.JulianInt96 -> Right PN.LtInt96
        Nothing -> Left "Parquet.Arrow: missing timestamp encoding"
  | AT.AFixedSizeBinary width <- AT.fieldType field =
      if width <= 0 || toInteger width > toInteger (maxBound :: Int32)
        then Left "Parquet.Arrow: fixed-byte width must fit a positive INT32"
        else
          if V.elem ("ARROW:extension:name", "arrow.uuid") (AT.fieldMetadata field)
            then
              let names = V.map snd (V.filter ((== "ARROW:extension:name") . fst) (AT.fieldMetadata field))
                  parameters = V.map snd (V.filter ((== "ARROW:extension:metadata") . fst) (AT.fieldMetadata field))
              in if width == 16 && names == V.singleton "arrow.uuid" && V.length parameters <= 1 && V.all T.null parameters
                   then Right PN.LtUuid
                   else Left "Parquet.Arrow: UUID requires16bytes and unique parameterless extension metadata"
            else Right (PN.LtFixedBinary (fromIntegral width))
  | AT.AUtf8View <- AT.fieldType field = Right PN.LtString
  | AT.ABinaryView <- AT.fieldType field = Right PN.LtBinary
  | otherwise = primitiveNestedTypeBasic field


data DecimalParameters = DecimalParameters
  { parameterPrecision :: {-# UNPACK #-} !Int
  , parameterScale :: {-# UNPACK #-} !Int
  }


decimalParameters :: AT.ArrowType -> Maybe DecimalParameters
decimalParameters typ = case typ of
  AT.ADecimal32 precision scale -> Just (DecimalParameters precision scale)
  AT.ADecimal64 precision scale -> Just (DecimalParameters precision scale)
  AT.ADecimal precision scale -> Just (DecimalParameters precision scale)
  AT.ADecimal256 precision scale -> Just (DecimalParameters precision scale)
  _ -> Nothing


primitiveNestedTypeBasic :: AT.Field -> Either String PN.LeafType
primitiveNestedTypeBasic field = do
  physical <- arrowFieldToSchemaElement field
  case (P.seType physical, P.seLogicalType physical) of
    (_, Just (P.LTInteger width signed)) -> Right (PN.LtInteger width signed)
    (_, Just P.LTFloat16) -> Right PN.LtFloat16
    (_, Just P.LTDate) -> Right PN.LtDate
    (_, Just (P.LTTime adjusted unit)) -> Right (PN.LtTime adjusted unit)
    (_, Just (P.LTTimestamp adjusted unit)) -> Right (PN.LtTimestamp adjusted unit)
    (_, Just P.LTString) -> Right PN.LtString
    (_, Just P.LTJson) -> Right PN.LtJson
    (Just P.PTInt32, Nothing) -> Right PN.LtInt32
    (Just P.PTInt64, Nothing) -> Right PN.LtInt64
    (Just P.PTFloat, Nothing) -> Right PN.LtFloat
    (Just P.PTDouble, Nothing) -> Right PN.LtDouble
    (Just P.PTBoolean, Nothing) -> Right PN.LtBool
    (Just P.PTByteArray, Nothing) -> Right PN.LtBinary
    _ -> Left "Parquet.Arrow: unsupported nested primitive annotation"


{- | Lower an Arrow column using its field's types and temporal units.
Only values reachable through present parents are examined. Required
children may carry null/poison storage beneath a null parent; such slots
have no Parquet value. Units are normalized in the same way as flat
lowering (DATE64 to days, seconds to milliseconds), without truncation.
-}
columnArrayToNestedRows
  :: AT.Field -> AC.ColumnArray -> Either String (V.Vector PN.NestedRow)
columnArrayToNestedRows field input = do
  _ <- arrowFieldToNestedSchema field
  column <- VariantArray.alignVariantColumns field input
  -- Validate shapes once; enforce nullability only for visible rows.
  validateColumns (V.singleton (relaxNullability field)) (V.singleton column)
  plan <- nestedRowPlan field column
  case AC.columnLength column of
    I# count -> case Generate.generate# count (nestedRowAt# plan) of
      (# message | #) -> Left message
      (# | rows #) -> Right rows
  where
    relaxNullability f =
      let children = V.map relaxNullability (AT.fieldChildren f)
          kept = if AT.fieldType f == AT.ARunEndEncoded && not (V.null children) then children V.// [(0, (V.head children) {AT.fieldNullable = False})] else children
      in f {AT.fieldNullable = True, AT.fieldChildren = kept}


newtype NestedRowPlan = NestedRowPlan
  { nestedRowAt# :: Int# -> (# String | PN.NestedRow #)
  }





nestedRowPlan :: AT.Field -> AC.ColumnArray -> Either String NestedRowPlan
nestedRowPlan field column
  | VariantSchema.isVariantField field = do
      _ <- VariantSchema.compileVariantStorageSchema field
      -- Keep source nullability so a visible required child is checked.
      -- Only metadata's storage encoding changes for the native row view.
      let children = V.map (\child -> if AT.fieldName child == "metadata" then child {AT.fieldType = AT.ABinary, AT.fieldDictionary = Nothing, AT.fieldChildren = V.empty} else child) (AT.fieldChildren field)
          raw = field {AT.fieldMetadata = V.filter (\(key, _) -> key /= "ARROW:extension:name" && key /= "ARROW:extension:metadata") (AT.fieldMetadata field), AT.fieldChildren = children}
      nestedRowPlan raw column
nestedRowPlan field column = do
  present <- case column of
    AC.ColStruct {} | FileArrow.isFileField field -> do
      schema <- FileArrow.fileSchemaFromField field
      reader <- File.compileFileReader schema column
      pure $ \index -> case File.fileReferenceAt# reader index of
        (# message | | #) -> (# message | #)
        (# | reference | #) -> (# | PN.NRFile reference #)
        (# | | () #) -> (# | PN.NRNull #)
    AC.ColInt8 _ xs -> pure (primitive (PN.LvInt32 . fromIntegral) xs)
    AC.ColInt16 _ xs -> pure (primitive (PN.LvInt32 . fromIntegral) xs)
    AC.ColInt32 _ xs -> pure (primitive PN.LvInt32 xs)
    AC.ColInt64 _ xs -> pure (primitive PN.LvInt64 xs)
    AC.ColUInt8 _ xs -> pure (primitive (PN.LvInt32 . fromIntegral) xs)
    AC.ColUInt16 _ xs -> pure (primitive (PN.LvInt32 . fromIntegral) xs)
    AC.ColUInt32 _ xs -> pure (primitive (PN.LvInt32 . fromIntegral) xs)
    AC.ColUInt64 _ xs -> pure (primitive (PN.LvInt64 . fromIntegral) xs)
    AC.ColDate32 _ xs -> pure (primitive PN.LvInt32 xs)
    AC.ColDate64 _ xs -> pure (primitive PN.LvInt64 xs)
    AC.ColTime32 _ xs -> pure (primitive PN.LvInt32 xs)
    AC.ColTime64 _ xs -> pure (primitive PN.LvInt64 xs)
    AC.ColTimestamp _ xs -> pure (primitive PN.LvInt64 xs)
    AC.ColFloat16 _ xs -> pure (primitive (\(AC.Float16 bits) -> PN.LvFloat16 bits) xs)
    AC.ColFloat _ xs -> pure (primitive PN.LvFloat xs)
    AC.ColDouble _ xs -> pure (primitive PN.LvDouble xs)
    AC.ColDecimal32 precision _ _ xs -> pure (decimal32Rows precision xs)
    AC.ColDecimal64 precision _ _ xs -> pure (decimal64Rows precision xs)
    AC.ColDecimal128 precision _ _ xs -> pure (decimal128Rows precision xs)
    AC.ColDecimal256 precision _ _ xs -> pure (decimal256Rows precision xs)
    AC.ColFixedSizeBinary {} -> pure (\index -> fromMaybeValue# PN.LvFixedBinary (AC.anyBytesAt column (I# index)))
    AC.ColUtf8View {} -> pure (\index -> fromMaybeValue# PN.LvString (AC.anyTextAt column (I# index)))
    AC.ColBinaryView {} -> pure (\index -> fromMaybeValue# PN.LvBinary (AC.anyBytesAt column (I# index)))
    AC.ColDictionary {} | AT.fieldType field == AT.ABinary -> metadataPlan
    AC.ColRunEndEncoded {} | AT.fieldType field == AT.ABinary -> metadataPlan
    AC.ColStruct _ _ children -> do
      plans <- V.zipWithM (\f (_, c) -> nestedRowPlan f c) (AT.fieldChildren field) children
      pure $ \index -> case V.length plans of
        I# count -> case Generate.generate# count (\child -> nestedRowAt# (V.unsafeIndex plans (I# child)) index) of
          (# message | #) -> (# message | #)
          (# | rows #) -> (# | PN.NRStruct rows #)
    AC.ColList _ offsets child -> listPlan offsets child
    AC.ColLargeList _ offsets child -> listPlan offsets child
    AC.ColListView _ offsets sizes child -> do
      plan <- nestedRowPlan (V.head (AT.fieldChildren field)) child
      let !childRows = AC.columnLength child
      pure $ \index ->
        let !start = fromIntegral (VS.unsafeIndex offsets (I# index))
            !count = fromIntegral (VS.unsafeIndex sizes (I# index))
        in if start < 0 || count < 0 || start > childRows || count > childRows - start
             then (# "Parquet.Arrow: nested list-view range is outside its column" | #)
             else case start of
               I# start# -> case count of
                 I# count# -> case Generate.generate# count# (\offset -> nestedRowAt# plan (start# +# offset)) of
                   (# message | #) -> (# message | #)
                   (# | rows #) -> (# | PN.NRList rows #)
    AC.ColMap _ offsets keys values -> do
      let entries = V.unsafeHead (AT.fieldChildren field)
          keyField = V.unsafeIndex (AT.fieldChildren entries) 0
          valueField = V.unsafeIndex (AT.fieldChildren entries) 1
          !childRows = AC.columnLength keys
          !sorted = case AT.fieldType field of AT.AMap True -> True; _ -> False
      keyPlan <- nestedRowPlan keyField keys
      valuePlan <- nestedRowPlan valueField values
      let row index = case checkedNestedRange# offsets childRows index of
            (# message | #) -> (# message | #)
            (# | (# start, count #) #) ->
              case Generate.generate#
                count
                ( \offset ->
                    case nestedRowAt# keyPlan (start +# offset) of
                      (# message | #) -> (# message | #)
                      (# | !key #) -> case nestedRowAt# valuePlan (start +# offset) of
                        (# message | #) -> (# message | #)
                        (# | !value #) -> (# | (key, value) #)
                ) of
                (# message | #) -> (# message | #)
                (# | pairs #) -> (# | PN.NRMapEntries pairs #)
      pure $ \index ->
        if sorted
          then case AC.validateMapKeysSorted (AC.sliceColumnArray (I# index) 1 column) of
            Left message -> (# message | #)
            Right () -> row index
          else row index
    _
      | Just values <- AC.asBool column -> pure (\index -> fromMaybeValue# PN.LvBool (AC.boolArrayAt values (I# index)))
      | Just values <- AC.asUtf8 column -> pure (\index -> fromMaybeValue# PN.LvString (AC.unsafeTextAt values (I# index)))
      | Just values <- AC.asLargeUtf8 column -> pure (\index -> fromMaybeValue# PN.LvString (AC.unsafeTextAt values (I# index)))
      | Just values <- AC.asBinary column -> pure (\index -> fromMaybeValue# PN.LvBinary (AC.unsafeBytesAt values (I# index)))
      | Just values <- AC.asLargeBinary column -> pure (\index -> fromMaybeValue# PN.LvBinary (AC.unsafeBytesAt values (I# index)))
      | otherwise -> Left ("Parquet.Arrow: unsupported nested column " ++ AC.columnTag column)
  Right $ NestedRowPlan $ \index ->
    if AC.isValidAt (AC.validity column) (I# index)
      then present index
      else
        if AT.fieldNullable field
          then (# | PN.NRNull #)
          else (# "Parquet.Arrow: null at required nested field " ++ show (AT.fieldName field) | #)
  where
    metadataPlan = do
      reader <- VariantArray.compileBytes column
      pure $ \index -> case VariantArray.bytesAt# reader index of
        (# | () #) -> (# "Parquet.Arrow: present Variant root has null metadata" | #)
        (# bytes | #) -> (# | PN.NRLeaf (PN.LvBinary bytes) #)
    primitive :: Storable a => (a -> PN.LeafValue) -> VS.Vector a -> Int# -> (# String | PN.NestedRow #)
    primitive wrap values index = case normalizeNestedValue# field (wrap (VS.unsafeIndex values (I# index))) of
      (# message | #) -> (# message | #)
      (# | value #) -> let !row = PN.NRLeaf value in (# | row #)
    {-# INLINE primitive #-}
    listPlan :: (Storable o, Integral o) => VS.Vector o -> AC.ColumnArray -> Either String (Int# -> (# String | PN.NestedRow #))
    listPlan offsets child = do
      plan <- nestedRowPlan (V.unsafeHead (AT.fieldChildren field)) child
      let !childRows = AC.columnLength child
      pure $ \index -> case checkedNestedRange# offsets childRows index of
        (# message | #) -> (# message | #)
        (# | (# start, count #) #) -> case Generate.generate# count (\offset -> nestedRowAt# plan (start +# offset)) of
          (# message | #) -> (# message | #)
          (# | rows #) -> (# | PN.NRList rows #)
    {-# INLINE listPlan #-}


-- Precompute precision bounds once per column and retain concrete workers.
-- Decimal narrowing is checked before conversion, including at negative
-- extremes; null or poisoned child slots are never visited by the parent plan.
decimal32Rows :: Int -> VS.Vector Int32 -> Int# -> (# String | PN.NestedRow #)
decimal32Rows precision values =
  let !bound = fromInteger (10 ^ precision) :: Int32
  in \index ->
       let !value = VS.unsafeIndex values (I# index)
       in if value > negate bound && value < bound
            then (# | PN.NRLeaf (PN.LvInt32 value) #)
            else (# "Parquet.Arrow: decimal exceeds its declared precision" | #)


decimal64Rows :: Int -> VS.Vector Int64 -> Int# -> (# String | PN.NestedRow #)
decimal64Rows precision values =
  let !bound = fromInteger (10 ^ precision) :: Int64
  in \index ->
       let !value = VS.unsafeIndex values (I# index)
       in if value > negate bound && value < bound
            then (# | PN.NRLeaf (if precision <= 9 then PN.LvInt32 (fromIntegral value) else PN.LvInt64 value) #)
            else (# "Parquet.Arrow: decimal exceeds its declared precision" | #)


decimal128Rows :: Int -> VS.Vector AC.Decimal128 -> Int# -> (# String | PN.NestedRow #)
decimal128Rows precision values =
  let !bound = 10 ^ precision
      !low = AC.decimal128FromInteger (negate bound)
      !high = AC.decimal128FromInteger bound
  in \index ->
       let !value@(AC.Decimal128 limb _) = VS.unsafeIndex values (I# index)
       in if compare128 value low == GT && compare128 value high == LT
            then (# | PN.NRLeaf (if precision <= 9 then PN.LvInt32 (fromIntegral limb) else if precision <= 18 then PN.LvInt64 (fromIntegral limb) else PN.LvFixedBinary (decimal128BigEndian value)) #)
            else (# "Parquet.Arrow: decimal exceeds its declared precision" | #)


decimal256Rows :: Int -> VS.Vector AC.Decimal256 -> Int# -> (# String | PN.NestedRow #)
decimal256Rows precision values =
  let !bound = 10 ^ precision
      !low = AC.decimal256FromInteger (negate bound)
      !high = AC.decimal256FromInteger bound
  in \index ->
       let !value@(AC.Decimal256 limb limb1 _ _) = VS.unsafeIndex values (I# index)
       in if compare256 value low == GT && compare256 value high == LT
            then (# | PN.NRLeaf (if precision <= 9 then PN.LvInt32 (fromIntegral limb) else if precision <= 18 then PN.LvInt64 (fromIntegral limb) else if precision <= 38 then PN.LvFixedBinary (decimal128BigEndian (AC.Decimal128 limb limb1)) else PN.LvFixedBinary (decimal256BigEndian value)) #)
            else (# "Parquet.Arrow: decimal exceeds its declared precision" | #)


decimal128BigEndian :: AC.Decimal128 -> ByteString
decimal128BigEndian (AC.Decimal128 low high) = BSI.unsafeCreate 16 $ \ptr -> do
  pokeByteOff ptr 0 (byteSwap64 high)
  pokeByteOff ptr 8 (byteSwap64 low)


decimal256BigEndian :: AC.Decimal256 -> ByteString
decimal256BigEndian (AC.Decimal256 limb0 limb1 limb2 limb3) = BSI.unsafeCreate 32 $ \ptr -> do
  pokeByteOff ptr 0 (byteSwap64 limb3)
  pokeByteOff ptr 8 (byteSwap64 limb2)
  pokeByteOff ptr 16 (byteSwap64 limb1)
  pokeByteOff ptr 24 (byteSwap64 limb0)


fromMaybeValue# :: (a -> PN.LeafValue) -> Maybe a -> (# String | PN.NestedRow #)
fromMaybeValue# wrap value = case value of
  Nothing -> (# "Parquet.Arrow: missing nested value in a present slot" | #)
  Just present -> let !row = PN.NRLeaf (wrap present) in (# | row #)
{-# INLINE fromMaybeValue# #-}


checkedNestedRange# :: (Storable o, Integral o) => VS.Vector o -> Int -> Int# -> (# String | (# Int#, Int# #) #)
checkedNestedRange# offsets childRows index =
  let !first = VS.unsafeIndex offsets (I# index)
      !lastOffset = VS.unsafeIndex offsets (I# (index +# 1#))
      !start = fromIntegral first :: Int
      !end = fromIntegral lastOffset :: Int
  in if fromIntegral start /= first || fromIntegral end /= lastOffset || start < 0 || end < start || end > childRows
       then (# "Parquet.Arrow: nested child range is outside its column" | #)
       else case start of I# start# -> case end - start of I# count# -> (# | (# start#, count# #) #)
{-# INLINE checkedNestedRange# #-}


normalizeNestedValue# :: AT.Field -> PN.LeafValue -> (# String | PN.LeafValue #)
normalizeNestedValue# field value = case (AT.fieldType field, value) of
  (AT.ADate AT.DateMillisecond, PN.LvInt64 milliseconds) ->
    let (days, rest) = milliseconds `quotRem` 86400000
    in if rest == 0 && days >= fromIntegral (minBound :: Int32) && days <= fromIntegral (maxBound :: Int32)
         then (# | PN.LvInt32 (fromIntegral days) #)
         else (# message | #)
  (AT.ATime AT.Second 32, PN.LvInt32 seconds)
    | seconds >= 0 && seconds < 86400 -> (# | PN.LvInt32 (seconds * 1000) #)
    | otherwise -> (# message | #)
  (AT.ATime AT.Millisecond 32, PN.LvInt32 milliseconds)
    | milliseconds >= 0 && milliseconds < 86400000 -> (# | value #)
    | otherwise -> (# message | #)
  (AT.ATime AT.Microsecond 64, PN.LvInt64 microseconds)
    | microseconds >= 0 && microseconds < 86400000000 -> (# | value #)
    | otherwise -> (# message | #)
  (AT.ATime AT.Nanosecond 64, PN.LvInt64 nanoseconds)
    | nanoseconds >= 0 && nanoseconds < 86400000000000 -> (# | value #)
    | otherwise -> (# message | #)
  (AT.ATimestamp AT.Second _, PN.LvInt64 seconds)
    | seconds >= (minBound :: Int64) `quot` 1000 && seconds <= (maxBound :: Int64) `quot` 1000 ->
        (# | PN.LvInt64 (seconds * 1000) #)
    | otherwise -> (# message | #)
  _ -> (# | value #)
  where
    message = "Parquet.Arrow: nested temporal value loses precision or exceeds its range"
{-# INLINE normalizeNestedValue# #-}


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
  let !rgs = P.fmRowGroups (PR.pfFooter pf)
      !nRg = V.length rgs
      keep i =
        Pred.evalRowGroup (PR.pfFooter pf) predicate (V.unsafeIndex rgs i)
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


{- | Read one column with page-level predicate pushdown.

Looks up the column chunk's 'OffsetIndex' + 'ColumnIndex',
evaluates the predicate against the 'ColumnIndex' to produce
a per-page keep mask, then decodes only retained pages using the shared
level-preserving readers. Whole retained pages are returned; apply an
additional row filter if exact predicate matches are required.

Native logical types and schema definition levels are used before checking
the target's nullability or applying supported numeric widening. Nullable
ancestors, dictionary encoding and fixed/decimal values use the same path.

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
readParquetColumnWithPagePruning pf rgIdx colIdx requested predicate = do
  let metadata = PR.pfFooter pf
      schema = P.fmSchema metadata
      leaves = V.filter (maybe False (const True) . P.seType) schema
  group <- maybe (Left "Parquet.Arrow: row group out of range") Right (P.fmRowGroups metadata V.!? rgIdx)
  column <- maybe (Left "Parquet.Arrow: column out of range") Right (P.rgColumns group V.!? colIdx)
  cm <- maybe (Left "Parquet.Arrow: missing column metadata") Right (P.ccMetadata column)
  leaf <- maybe (Left "Parquet.Arrow: missing physical leaf") Right (leaves V.!? colIdx)
  if P.seType leaf /= Just (P.cmType cm)
    then Left "Parquet.Arrow: schema and column physical types disagree"
    else Right ()
  native <- schemaElementToArrowField leaf
  (maxRep, maxDef) <- Levels.maxLevelsForColumnPath schema (P.cmPathInSchema cm)
  if maxRep /= 0
    then Left "Parquet.Arrow: repeated fields need row-group container reconstruction; page pruning is not supported"
    else Right ()
  if P.rgNumRows group < 0 || P.cmNumValues cm /= P.rgNumRows group
    then Left "Parquet.Arrow: non-repeated column events differ from row count"
    else Right ()
  -- Validate the requested type even if every data page will be skipped.
  compatible <-
    if Timestamp.isTimestampField native || Timestamp.isTimestampField requested
      then Timestamp.fieldsCompatible native requested
      else Right (AT.fieldType requested == AT.fieldType native || canWiden (AT.fieldType native) (AT.fieldType requested))
  if not compatible
    then Left "Parquet.Arrow: incompatible page-pruning target type"
    else Right ()
  let decodingField = if canWidenBinary (AT.fieldType native) (AT.fieldType requested) then native {AT.fieldType = AT.fieldType requested} else native
  indexes <- loadIndices pf rgIdx colIdx
  case indexes of
    Nothing -> do
      decoded <- readParquetColumn pf rgIdx colIdx (decodingField {AT.fieldNullable = True})
      result <- project native decoded
      Right (Nothing, result)
    Just index -> do
      context <- maybe (Left "Parquet.Arrow: missing statistics context") Right (Pred.statisticsContext metadata colIdx)
      let keep = (== Pred.PMaybeKeep) <$> Pred.evalPagesByColumnIndex context (pruningBounds index) predicate
          selection =
            PR.PageSelection
              (P.oiPageLocations (pruningOffsets index))
              keep
              (P.cmDictionaryPageOffset cm)
              (Just (P.rgNumRows group))
          total = V.length keep
          retained = V.foldl' (\n yes -> n + if yes then 1 else 0) 0 keep
          codec = P.cmCodec cm
          completeWith lift dense decoded =
            if maxDef == 0
              then dense >>= lift
              else do
                levels <- decoded
                values <- lift (PR.clValues levels)
                let physical = AN.LeafColumn (PR.clRepetition levels) (PR.clDefinition levels) values
                AN.assemble
                  (V.singleton (Right physical))
                  (AN.Plan (decodingField {AT.fieldNullable = True}) 0 0 0 maxDef AN.Primitive)
          complete wrap = completeWith (liftDefined leaf . wrap)
      prepared <- PR.prepareColumnSelection pf rgIdx colIdx selection
      let file = PR.preparedSelectionBytes prepared
          selected = PR.preparedSelection prepared
      decoded <- case P.cmType cm of
        physical
          | nativeScalarPhysical (AT.fieldType native) == Just physical ->
              completeWith
                (scalarBuffersColumn (AT.fieldType native))
                (PR.readScalarBuffersSelectedValues physical codec file selected)
                (PR.readScalarBuffersSelectedLevels physical codec maxRep maxDef file selected)
        P.PTInt32 -> complete PW.ColInt32 (PR.readInt32SelectedValues codec file selected) (PR.readInt32SelectedLevels codec maxRep maxDef file selected)
        P.PTInt64 -> complete PW.ColInt64 (PR.readInt64SelectedValues codec file selected) (PR.readInt64SelectedLevels codec maxRep maxDef file selected)
        P.PTFloat -> complete PW.ColFloat (PR.readFloatSelectedValues codec file selected) (PR.readFloatSelectedLevels codec maxRep maxDef file selected)
        P.PTDouble -> complete PW.ColDouble (PR.readDoubleSelectedValues codec file selected) (PR.readDoubleSelectedLevels codec maxRep maxDef file selected)
        P.PTBoolean -> completeWith boolBuffersColumn (PR.readBoolBuffersSelectedValues codec file selected) (PR.readBoolBuffersSelectedLevels codec maxRep maxDef file selected)
        P.PTByteArray
          | isLargeBinary (AT.fieldType decodingField) ->
              completeWith
                (largeBinaryBuffersColumn (AT.fieldType decodingField))
                (PR.readLargeBinaryBuffersSelectedValues codec file selected)
                (PR.readLargeBinaryBuffersSelectedLevels codec maxRep maxDef file selected)
          | isNativeBinary (AT.fieldType native) ->
              completeWith
                (binaryBuffersColumn (AT.fieldType native))
                (PR.readBinaryBuffersSelectedValues codec file selected)
                (PR.readBinaryBuffersSelectedLevels codec maxRep maxDef file selected)
          | otherwise -> complete PW.ColByteArray (PR.readByteArraySelectedValues codec file selected) (PR.readByteArraySelectedLevels codec maxRep maxDef file selected)
        P.PTInt96 -> complete PW.ColByteArray (PR.readFixedSelectedValues 12 codec file selected) (PR.readFixedSelectedLevels 12 codec maxRep maxDef file selected)
        P.PTFixedLenByteArray -> case P.seTypeLength leaf of
          Just width -> complete PW.ColByteArray (PR.readFixedSelectedValues (fromIntegral width) codec file selected) (PR.readFixedSelectedLevels (fromIntegral width) codec maxRep maxDef file selected)
          Nothing -> Left "Parquet.Arrow: fixed binary leaf is missing its width"
      result <- project native decoded
      Right (Just (retained, total), result)
  where
    canWiden (AT.AInt 32 True) (AT.AInt 64 True) = True
    canWiden (AT.AFloatingPoint AT.Single) (AT.AFloatingPoint AT.DoublePrecision) = True
    canWiden actual target = canWidenBinary actual target
    project native values = do
      if not (AT.fieldNullable requested) && AC.nullCount values /= 0
        then Left "Parquet.Arrow: non-nullable page-pruning target cannot contain nulls"
        else Right ()
      if Timestamp.isTimestampField native || Timestamp.isTimestampField requested
        then Timestamp.projectTimestampColumn native requested values
        else
          if AT.fieldType requested == AT.fieldType native
            then Right values
            else coerceColumn (AT.fieldType requested) values


data PruningIndex = PruningIndex
  { pruningOffsets :: !P.OffsetIndex
  , pruningBounds :: !P.ColumnIndex
  }


loadIndices :: PR.ParquetFile -> Int -> Int -> Either String (Maybe PruningIndex)
loadIndices pf rgIdx colIdx = do
  bounds <- PI.readColumnIndex pf rgIdx colIdx
  case bounds of
    Nothing -> Right Nothing
    Just ci -> do
      offsets <- PI.readOffsetIndex pf rgIdx colIdx
      case offsets of
        Just oi
          | V.length (P.oiPageLocations oi) /= V.length (P.ciNullPages ci) ->
              Left "Parquet.Arrow: column/offset index page counts disagree"
          | otherwise -> Right (Just (PruningIndex oi ci))
        Nothing -> Right Nothing


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
