{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Arrow ↔ Parquet column-data bridge.
--
-- Lets callers keep a single in-memory representation
-- ('Arrow.Column.ColumnArray') and choose Parquet at the wire-
-- format level, mirroring how
-- @pyarrow.parquet.write_table(table, path)@ works in Python: a
-- 'pa.Table' goes in, Parquet bytes come out.
--
-- @
-- -- Arrow → Parquet
-- let !(schema, rowGroups) = 'arrowToParquet' arrowSchema arrowBatches
--     bytes                = 'Parquet.HighLevel.encodeParquet'
--                              'Parquet.HighLevel.defaultWriteOptions'
--                              schema rowGroups
--
-- -- Parquet → Arrow (one row group at a time)
-- pf <- 'Parquet.HighLevel.decodeParquet' bytes
-- batch <- 'parquetRowGroupToArrow' arrowSchema pf 0
-- @
--
-- The bridge currently covers the flat-primitive shape Parquet's
-- writer ('Parquet.Write.ColumnData') natively supports: 'Int8',
-- 'Int16', 'Int32', 'Int64', 'UInt8'..'UInt64', 'Float', 'Double',
-- 'Bool', 'Utf8', 'Binary', plus their nullable variants. Nested
-- columns (struct / list / map / union / dictionary / view / REE)
-- aren't in Parquet's flat data plane, so they fall through to a
-- Left at translation time. Support for nested shredding via
-- "Parquet.Nested" is a separate item.
module Parquet.Arrow
  ( -- * Arrow → Parquet (flat, required + nullable)
    arrowToParquet
  , arrowToParquetMixed
  , columnArrayToColumnData
  , columnArrayToParquetColumn
    -- * Arrow → Parquet (nested, via Parquet.Nested)
  , arrowFieldToNestedSchema
  , columnArrayToNestedRows
    -- * Parquet → Arrow
  , parquetRowGroupToArrow
  , parquetRowGroupToArrowProjected
  , readParquetColumn
  , parquetFileArrowSchema
  , ProjectionError (..)
    -- * Streaming reader (one row group at a time)
  , streamRowGroups
  , streamRowGroupsIter
  , streamRowGroupsProjectedIter
  , streamRowGroupsFilteredIter
  , streamRowGroupsProjectedFilteredIter
  , numRowGroups
    -- * Page-index-driven page skipping
  , readParquetColumnWithPagePruning
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as MVP
import qualified Data.Vector.Storable.Mutable as MVS
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Foreign.Storable (Storable)
import Columnar.Bit (Bit (..))
import qualified Arrow.View as AV
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Arrow.Column as AC
import qualified Arrow.Types as AT

import qualified Columnar.NullableBuild as NB
import qualified Columnar.Stream as IS

import qualified Parquet.Nested as PN
import qualified Parquet.PageIndex as PI
import qualified Parquet.Predicate as Pred
import qualified Parquet.Read as PR
import qualified Parquet.Types as P
import qualified Parquet.Write as PW

-- ============================================================
-- Arrow → Parquet
-- ============================================================

-- | Lower an Arrow schema + a sequence of column-major batches
-- to the inputs 'Parquet.HighLevel.encodeParquet' expects.
--
-- Each Arrow batch becomes one Parquet row group. Returns 'Left'
-- if any column type isn't representable in Parquet's flat data
-- plane (struct / list / dictionary / view / REE — see the
-- module docs for the supported subset).
arrowToParquet
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String (V.Vector P.SchemaElement, [V.Vector PW.ColumnData])
arrowToParquet sch batches = do
  let !leafFields = arrowFieldsToLeaves (AT.arrowFields sch)
      !rootElem   = P.SchemaElement
                      { P.seName        = "schema"
                      , P.seRepetition  = Nothing
                      , P.seType        = Nothing
                      , P.seNumChildren = Just (fromIntegral (V.length leafFields))
                      , P.seConvertedType = Nothing
                      , P.seLogicalType = Nothing
                      , P.seFieldId     = Nothing
                      }
  schemaElems <- V.mapM arrowFieldToSchemaElement leafFields
  let !pSchema = V.cons rootElem schemaElems
  rgData <- mapM (V.mapM columnArrayToColumnData) batches
  Right (pSchema, rgData)

-- | Like 'arrowToParquet' but returns 'PW.ParquetColumn' values
-- so nullable Arrow columns lower to 'PW.PCOptional' instead of
-- being dropped through 'optionalColumnPresentValues'. Pair with
-- 'Parquet.Write.buildParquetFileMixed' to write a Parquet file
-- that actually carries the nulls; 'Parquet.HighLevel.encodeParquet'
-- auto-routes through this path when any input column is
-- nullable.
arrowToParquetMixed
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String (V.Vector P.SchemaElement, [V.Vector PW.ParquetColumn])
arrowToParquetMixed sch batches = do
  let !leafFields = arrowFieldsToLeaves (AT.arrowFields sch)
      !rootElem   = P.SchemaElement
                      { P.seName          = "schema"
                      , P.seRepetition    = Nothing
                      , P.seType          = Nothing
                      , P.seNumChildren   = Just (fromIntegral (V.length leafFields))
                      , P.seConvertedType = Nothing
                      , P.seLogicalType   = Nothing
                      , P.seFieldId       = Nothing
                      }
  schemaElems <- V.mapM arrowFieldToSchemaElement leafFields
  let !pSchema = V.cons rootElem schemaElems
  rgData <- mapM (V.mapM columnArrayToParquetColumn) batches
  Right (pSchema, rgData)

-- | Dispatch a single Arrow column onto the 'PW.ParquetColumn'
-- sum. Nullable variants map to 'PW.PCOptional'; required ones
-- reuse 'columnArrayToColumnData' and wrap the result in
-- 'PW.PCRequired'.
columnArrayToParquetColumn
  :: AC.ColumnArray -> Either String PW.ParquetColumn
columnArrayToParquetColumn col = case col of
  -- Nullable primitives now go straight through: 'OptInt32'
  -- etc. hold 'AC.NullableView' directly, so the bridge is a
  -- no-op constructor application.
  AC.ColInt32Maybe v  -> Right $ PW.PCOptional (PW.OptInt32 v)
  AC.ColInt64Maybe v  -> Right $ PW.PCOptional (PW.OptInt64 v)
  AC.ColFloatMaybe v  -> Right $ PW.PCOptional (PW.OptFloat v)
  AC.ColDoubleMaybe v -> Right $ PW.PCOptional (PW.OptDouble v)
  AC.ColBoolMaybe v ->
    Right $ PW.PCOptional (PW.OptBool (AV.nullableBoolViewToMaybeVector v))
  AC.ColUtf8Maybe v ->
    Right $ PW.PCOptional (PW.OptByteArray
      (V.map (fmap TE.encodeUtf8) (AV.nullableUtf8ViewToMaybeVector v)))
  AC.ColBinaryMaybe v ->
    Right $ PW.PCOptional (PW.OptByteArray
      (AV.nullableBinaryViewToMaybeVector v))
  AC.ColLargeUtf8Maybe v ->
    Right $ PW.PCOptional (PW.OptByteArray
      (V.map (fmap TE.encodeUtf8) (AV.nullableLargeUtf8ViewToMaybeVector v)))
  AC.ColLargeBinaryMaybe v ->
    Right $ PW.PCOptional (PW.OptByteArray
      (AV.nullableLargeBinaryViewToMaybeVector v))
  -- Int8 / Int16 / UInt* nullable: widen to Int32 / Int64 via
  -- 'nvMap' (one O(N) VS.map over the values vector; validity
  -- bitmap aliased into the result).
  AC.ColInt8Maybe v  ->
    Right $ PW.PCOptional (PW.OptInt32 (AC.nvMap (fromIntegral :: Int8 -> Int32)  v))
  AC.ColInt16Maybe v ->
    Right $ PW.PCOptional (PW.OptInt32 (AC.nvMap (fromIntegral :: Int16 -> Int32) v))
  AC.ColUInt8Maybe v ->
    Right $ PW.PCOptional (PW.OptInt32 (AC.nvMap (fromIntegral :: Word8 -> Int32)  v))
  AC.ColUInt16Maybe v ->
    Right $ PW.PCOptional (PW.OptInt32 (AC.nvMap (fromIntegral :: Word16 -> Int32) v))
  AC.ColUInt32Maybe v ->
    Right $ PW.PCOptional (PW.OptInt32 (AC.nvMap (fromIntegral :: Word32 -> Int32) v))
  AC.ColUInt64Maybe v ->
    Right $ PW.PCOptional (PW.OptInt64 (AC.nvMap (fromIntegral :: Word64 -> Int64) v))
  -- Temporal nullable: payload is already Int32 / Int64 in the
  -- view, so this is just a constructor switch.
  AC.ColDate32Maybe v    -> Right $ PW.PCOptional (PW.OptInt32 v)
  AC.ColDate64Maybe v    -> Right $ PW.PCOptional (PW.OptInt64 v)
  AC.ColTime32Maybe v    -> Right $ PW.PCOptional (PW.OptInt32 v)
  AC.ColTime64Maybe v    -> Right $ PW.PCOptional (PW.OptInt64 v)
  AC.ColTimestampMaybe v -> Right $ PW.PCOptional (PW.OptInt64 v)
  AC.ColDurationMaybe v  -> Right $ PW.PCOptional (PW.OptInt64 v)
  -- Required columns: just delegate.
  _ ->
    PW.PCRequired <$> columnArrayToColumnData col

-- | Project the (potentially-nested) Arrow field tree to a flat
-- list of leaves. Today we only support flat schemas (the bridge
-- doesn't round-trip nested types); nested fields fall through to
-- 'columnArrayToColumnData' which then reports a clean 'Left'.
arrowFieldsToLeaves :: V.Vector AT.Field -> V.Vector AT.Field
arrowFieldsToLeaves = V.filter (V.null . AT.fieldChildren)

-- | Translate an Arrow leaf 'Field' into a Parquet
-- 'SchemaElement'.
arrowFieldToSchemaElement
  :: AT.Field -> Either String P.SchemaElement
arrowFieldToSchemaElement f = do
  pType <- case AT.fieldType f of
    AT.AInt 8  _      -> Right P.PTInt32
    AT.AInt 16 _      -> Right P.PTInt32
    AT.AInt 32 _      -> Right P.PTInt32
    AT.AInt 64 _      -> Right P.PTInt64
    AT.ABool          -> Right P.PTBoolean
    AT.AFloatingPoint AT.Single          -> Right P.PTFloat
    AT.AFloatingPoint AT.DoublePrecision -> Right P.PTDouble
    AT.AUtf8          -> Right P.PTByteArray
    AT.ABinary        -> Right P.PTByteArray
    AT.ALargeUtf8     -> Right P.PTByteArray
    AT.ALargeBinary   -> Right P.PTByteArray
    -- Temporal types: map to INT32 (Date, Time-millis) or INT64
    -- (Date64, Time-micros/nanos, Timestamp, Duration). Logical
    -- / converted types are set below.
    AT.ADate AT.DateDay         -> Right P.PTInt32
    AT.ADate AT.DateMillisecond -> Right P.PTInt64
    AT.ATime _ 32     -> Right P.PTInt32
    AT.ATime _ 64     -> Right P.PTInt64
    AT.ATimestamp _ _ -> Right P.PTInt64
    AT.ADuration _    -> Right P.PTInt64
    other ->
      Left $ "Parquet.Arrow: Arrow type "
             <> show other <> " has no Parquet flat-primitive equivalent"
  let !rep = if AT.fieldNullable f then P.Optional else P.Required
      !logical = case AT.fieldType f of
        AT.AUtf8                           -> Just P.LTString
        AT.ALargeUtf8                      -> Just P.LTString
        AT.ADate _                         -> Just P.LTDate
        AT.ATime AT.Millisecond _          -> Just (P.LTTime False P.LtMillis)
        AT.ATime AT.Microsecond _          -> Just (P.LTTime False P.LtMicros)
        AT.ATime AT.Nanosecond _           -> Just (P.LTTime False P.LtNanos)
        AT.ATime AT.Second _               -> Just (P.LTTime False P.LtMillis)
          -- Parquet doesn't model second precision; widen.
        AT.ATimestamp AT.Millisecond mtz   ->
          Just (P.LTTimestamp (isJustUtc mtz) P.LtMillis)
        AT.ATimestamp AT.Microsecond mtz   ->
          Just (P.LTTimestamp (isJustUtc mtz) P.LtMicros)
        AT.ATimestamp AT.Nanosecond mtz    ->
          Just (P.LTTimestamp (isJustUtc mtz) P.LtNanos)
        AT.ATimestamp AT.Second mtz        ->
          Just (P.LTTimestamp (isJustUtc mtz) P.LtMillis)
        _                                  -> Nothing
      isJustUtc Nothing  = False
      isJustUtc (Just _) = True
  Right P.SchemaElement
    { P.seName          = AT.fieldName f
    , P.seRepetition    = Just rep
    , P.seType          = Just pType
    , P.seNumChildren   = Nothing
    , P.seConvertedType = case AT.fieldType f of
        AT.AUtf8       -> Just P.CTUtf8
        AT.ALargeUtf8  -> Just P.CTUtf8
        AT.ADate _     -> Just P.CTDate
        AT.ATime _ 32  -> Just P.CTTimeMillis
        AT.ATime _ 64  -> Just P.CTTimeMicros
        AT.ATimestamp AT.Millisecond _ -> Just P.CTTimestampMillis
        AT.ATimestamp AT.Microsecond _ -> Just P.CTTimestampMicros
        _              -> Nothing
    , P.seLogicalType   = logical
    , P.seFieldId       = Nothing
    }

-- | Lower one Arrow column to Parquet's 'ColumnData'. Nullable
-- variants are flattened to the present-only values vector with
-- nulls dropped — Parquet's 'ColumnData' is the non-nullable
-- shape; the 'Parquet.Write.OptionalColumn' path handles nullable
-- variants but the high-level API doesn't currently route through
-- it. Future work: return an @Either ColumnData OptionalColumn@
-- so callers can select the right Parquet writer.
columnArrayToColumnData
  :: AC.ColumnArray -> Either String PW.ColumnData
columnArrayToColumnData = \case
  AC.ColInt8   v -> Right $ PW.ColInt32 (VS.map fromIntegral v)
  AC.ColInt16  v -> Right $ PW.ColInt32 (VS.map fromIntegral v)
  AC.ColInt32  v -> Right $ PW.ColInt32 v
  AC.ColInt64  v -> Right $ PW.ColInt64 v
  AC.ColUInt8  v -> Right $ PW.ColInt32 (VS.map (fromIntegral :: Word8 -> Int32) v)
  AC.ColUInt16 v -> Right $ PW.ColInt32 (VS.map (fromIntegral :: Word16 -> Int32) v)
  AC.ColUInt32 v -> Right $ PW.ColInt32 (VS.map (fromIntegral :: Word32 -> Int32) v)
  AC.ColUInt64 v -> Right $ PW.ColInt64 (VS.map (fromIntegral :: Word64 -> Int64) v)
  AC.ColFloat  v -> Right $ PW.ColFloat v
  AC.ColDouble v -> Right $ PW.ColDouble v
  AC.ColBool   v -> Right $ PW.ColBool
                              (V.generate (VU.length v)
                                          (\i -> unBit (VU.unsafeIndex v i)))
  AC.ColUtf8      v -> Right $ PW.ColByteArray
                                 (V.map TE.encodeUtf8 (AV.utf8ViewToVector v))
  AC.ColLargeUtf8 v -> Right $ PW.ColByteArray
                                 (V.map TE.encodeUtf8 (AV.largeUtf8ViewToVector v))
  AC.ColBinary    v -> Right $ PW.ColByteArray (AV.binaryViewToVector v)
  AC.ColLargeBinary v -> Right $ PW.ColByteArray (AV.largeBinaryViewToVector v)
  -- Temporal types: lower to the natural Parquet physical type
  -- the schema element declared (Int32 for Date32 / Time32, Int64
  -- for Date64 / Time64 / Timestamp / Duration).
  AC.ColDate32 v   -> Right $ PW.ColInt32 v
  AC.ColDate64 v   -> Right $ PW.ColInt64 v
  AC.ColTime32 v   -> Right $ PW.ColInt32 v
  AC.ColTime64 v   -> Right $ PW.ColInt64 v
  AC.ColTimestamp v -> Right $ PW.ColInt64 v
  AC.ColDuration  v -> Right $ PW.ColInt64 v
  -- Nullable: drop nulls, emit only the present values. The
  -- writer's high-level path treats every column as
  -- (Required+ColumnData); proper Optional support requires a
  -- distinct OptionalColumn lowering that the writer's nested
  -- Parquet.Write.encodeOptionalColumnPage path consumes. We
  -- preserve the nullability in the schema (Repetition=Optional)
  -- but drop the nulls here for now; round-trip will recover the
  -- present values with no NULL slots.
  AC.ColInt8Maybe   v -> Right $ PW.ColInt32 (nvPresentVP (fromIntegral :: Int8  -> Int32) v)
  AC.ColInt16Maybe  v -> Right $ PW.ColInt32 (nvPresentVP (fromIntegral :: Int16 -> Int32) v)
  AC.ColInt32Maybe  v -> Right $ PW.ColInt32 (nvPresentVP id v)
  AC.ColInt64Maybe  v -> Right $ PW.ColInt64 (nvPresentVP id v)
  AC.ColUInt8Maybe  v -> Right $ PW.ColInt32 (nvPresentVP (fromIntegral :: Word8  -> Int32) v)
  AC.ColUInt16Maybe v -> Right $ PW.ColInt32 (nvPresentVP (fromIntegral :: Word16 -> Int32) v)
  AC.ColUInt32Maybe v -> Right $ PW.ColInt32 (nvPresentVP (fromIntegral :: Word32 -> Int32) v)
  AC.ColUInt64Maybe v -> Right $ PW.ColInt64 (nvPresentVP (fromIntegral :: Word64 -> Int64) v)
  AC.ColFloatMaybe  v -> Right $ PW.ColFloat  (nvPresentVP id v)
  AC.ColDoubleMaybe v -> Right $ PW.ColDouble (nvPresentVP id v)
  AC.ColBoolMaybe   v -> Right $ PW.ColBool
                                    (NB.presentValuesV (AV.nullableBoolViewToMaybeVector v))
  AC.ColUtf8Maybe   v -> Right $ PW.ColByteArray
                                    (NB.presentValuesVMap TE.encodeUtf8
                                       (AV.nullableUtf8ViewToMaybeVector v))
  AC.ColLargeUtf8Maybe v -> Right $ PW.ColByteArray
                                       (NB.presentValuesVMap TE.encodeUtf8
                                          (AV.nullableLargeUtf8ViewToMaybeVector v))
  AC.ColBinaryMaybe v -> Right $ PW.ColByteArray
                                    (NB.presentValuesV (AV.nullableBinaryViewToMaybeVector v))
  AC.ColLargeBinaryMaybe v -> Right $ PW.ColByteArray
                                       (NB.presentValuesV
                                          (AV.nullableLargeBinaryViewToMaybeVector v))
  other -> Left $ "Parquet.Arrow: Arrow column shape "
                  <> show other
                  <> " has no flat Parquet equivalent (nested types "
                  <> "go through Parquet.Nested, dictionary columns "
                  <> "should pre-resolve to their values column)"

-- | Drop nulls from a 'NullableView' and emit a dense
-- @VP.Vector@ of the remaining present values (with element
-- transformation). Lives here because the Parquet writer's
-- @ColumnData@ shape is still @VP.Vector@-backed; once that
-- migrates this helper goes away.
{-# INLINE boxedToBitVec #-}
boxedToBitVec :: V.Vector Bool -> VU.Vector Bit
boxedToBitVec v = VU.generate (V.length v) (\i -> Bit (V.unsafeIndex v i))

{-# INLINE nvPresentVP #-}
nvPresentVP
  :: (Storable a, Storable b)
  => (a -> b) -> AC.NullableView a -> VS.Vector b
nvPresentVP f nv =
  let !values   = AC.nvValues nv
      !validity = AC.nvValidity nv
      !n        = VS.length values
      !allValid = VU.null validity
  in VS.create $ do
       buf <- MVS.unsafeNew n
       let go !i !w
             | i >= n = pure (MVS.unsafeSlice 0 w buf)
             | otherwise =
                 let !present = allValid
                              || unBit (VU.unsafeIndex validity i)
                 in if present
                      then do
                        MVS.unsafeWrite buf w (f (VS.unsafeIndex values i))
                        go (i + 1) (w + 1)
                      else go (i + 1) w
       _ <- go 0 0
       pure buf

-- ============================================================
-- Parquet → Arrow
-- ============================================================

-- | Read columns of a Parquet row group and lift them to Arrow
-- column shapes, driven by the caller-supplied /target/ Arrow
-- schema.
--
-- Lookup is by column name (Parquet's footer carries unique leaf
-- names), so the target schema may be narrower than the file
-- (projection), in a different order than the file (reordering),
-- or request wider types than the file (coercion: see
-- 'coerceColumn' below for the supported widening rules).
--
-- Error cases:
--
--   * 'MissingColumn' — the target requests a column name that
--     isn't present in the file. Callers that want null-fill
--     semantics should catch this constructor and synthesize a
--     null column.
--   * 'IncompatibleType' — the file carries the column but the
--     target's type isn't reachable by the coercion table.
parquetRowGroupToArrow
  :: AT.Schema    -- ^ Target Arrow schema
  -> PR.ParquetFile
  -> Int          -- ^ Row group index
  -> Either ProjectionError (V.Vector AC.ColumnArray)
parquetRowGroupToArrow target pf rgIdx = do
  let !fileLeaves = V.filter (maybe False (const True) . P.seType)
                             (P.fmSchema (PR.pfFooter pf))
      !nameToIdx  = Map.fromList
        [ (P.seName se, i) | (i, se) <- V.toList (V.indexed fileLeaves) ]
  V.mapM (readOneProjected pf rgIdx fileLeaves nameToIdx)
         (AT.arrowFields target)

-- ============================================================
-- Projection / schema evolution
-- ============================================================

-- | Why a projection request couldn't be satisfied.
data ProjectionError
  = MissingColumn !Text
    -- ^ The target schema references a column name that isn't
    -- present in the Parquet footer. A real schema-evolution
    -- implementation would fill this column with nulls; callers
    -- that want that behaviour can catch this constructor and
    -- synthesize a null column.
  | IncompatibleType !Text !AT.ArrowType
    -- ^ The target schema requests a type the physical Parquet
    -- column can't be coerced to. The bridge's coercion table is
    -- intentionally narrow: numeric widening (Int32 → Int64,
    -- Float → Double), identity reads for equal types.
  deriving (Show, Eq)

-- | Recover the Arrow-shaped schema implied by a Parquet file's
-- leaf columns. Uses the Parquet converted-type / logical-type
-- annotations to pick the right Arrow flavour (UTF-8 vs raw
-- binary, Date vs Int32, Timestamp vs Int64).
parquetFileArrowSchema :: PR.ParquetFile -> AT.Schema
parquetFileArrowSchema pf =
  let !leaves = V.filter (maybe False (const True) . P.seType)
                         (P.fmSchema (PR.pfFooter pf))
  in AT.Schema
       { AT.arrowFields     = V.map schemaElementToArrowField leaves
       , AT.arrowEndianness = AT.Little
       , AT.arrowMetadata   = V.empty
       , AT.arrowFeatures   = V.empty
       }

-- | Map a Parquet 'P.SchemaElement' back to an Arrow leaf
-- 'AT.Field' using its converted-type / logical-type hints.
-- Mirrors the inverse of 'arrowFieldToSchemaElement'.
schemaElementToArrowField :: P.SchemaElement -> AT.Field
schemaElementToArrowField se =
  let !name     = P.seName se
      !nullable = case P.seRepetition se of
                    Just P.Optional -> True
                    _                -> False
      !ty = arrowTypeFromSchemaElement se
  in AT.Field name nullable ty V.empty Nothing V.empty

arrowTypeFromSchemaElement :: P.SchemaElement -> AT.ArrowType
arrowTypeFromSchemaElement se = case P.seType se of
  Just P.PTInt96     ->
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
      Just P.LTUUID    -> AT.AFixedSizeBinary 16
      _                -> AT.AFixedSizeBinary 16
  Just P.PTBoolean   -> AT.ABool
  Just P.PTInt32     -> case (P.seLogicalType se, P.seConvertedType se) of
    (Just P.LTDate, _)               -> AT.ADate AT.DateDay
    (Just (P.LTTime _ unit), _)
      | Just u <- arrowTimeUnit unit -> AT.ATime u 32
    (Just (P.LTInteger w isSigned), _)
      | w <= 32                      -> AT.AInt (fromIntegral w) isSigned
    (_,  Just P.CTDate)              -> AT.ADate AT.DateDay
    (_,  Just P.CTTimeMillis)        -> AT.ATime AT.Millisecond 32
    _                                -> AT.AInt 32 True
  Just P.PTInt64     -> case (P.seLogicalType se, P.seConvertedType se) of
    (Just (P.LTTime _ unit), _)
      | Just u <- arrowTimeUnit unit -> AT.ATime u 64
    (Just (P.LTTimestamp adj unit), _)
      | Just u <- arrowTimeUnit unit ->
          AT.ATimestamp u
            (if adj then Just (T.pack "UTC") else Nothing)
    (Just (P.LTInteger 64 isSigned), _)
                                     -> AT.AInt 64 isSigned
    (_, Just P.CTTimeMicros)         -> AT.ATime AT.Microsecond 64
    (_, Just P.CTTimestampMillis)    -> AT.ATimestamp AT.Millisecond Nothing
    (_, Just P.CTTimestampMicros)    -> AT.ATimestamp AT.Microsecond Nothing
    _                                -> AT.AInt 64 True
  Just P.PTFloat     -> AT.AFloatingPoint AT.Single
  Just P.PTDouble    -> AT.AFloatingPoint AT.DoublePrecision
  Just P.PTByteArray -> case (P.seLogicalType se, P.seConvertedType se) of
    (Just P.LTString, _)             -> AT.AUtf8
    -- Geometry / Geography / Variant + Json / Bson are all
    -- bytes on the wire; expose as ABinary so callers see the
    -- raw WKB / JSON / variant bytes. The LogicalType
    -- annotation survives round-trip for downstream tools.
    (Just P.LTGeometry, _)           -> AT.ABinary
    (Just P.LTGeography, _)          -> AT.ABinary
    (Just (P.LTVariant _), _)        -> AT.ABinary
    (Just P.LTJson, _)               -> AT.AUtf8
    (Just P.LTBson, _)               -> AT.ABinary
    (_,  Just P.CTUtf8)              -> AT.AUtf8
    (_,  Just P.CTJson)              -> AT.AUtf8
    (_,  Just P.CTBson)              -> AT.ABinary
    _                                -> AT.ABinary
  _                  -> AT.ABinary  -- FIXED_LEN_BYTE_ARRAY / Int96 fallback

arrowTimeUnit :: P.LtTimeUnit -> Maybe AT.TimeUnit
arrowTimeUnit P.LtMillis = Just AT.Millisecond
arrowTimeUnit P.LtMicros = Just AT.Microsecond
arrowTimeUnit P.LtNanos  = Just AT.Nanosecond

-- | Core per-column reader used by 'parquetRowGroupToArrow'.
-- Looks up the column by name, reads it at the file's native
-- Arrow type, then coerces to the target type via 'coerceColumn'
-- if needed.
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
        Left _    ->
          -- Direct read failed — load at the file's native type
          -- and coerce to the target.
          let !fileFld = schemaElementToArrowField
                           (V.unsafeIndex fileLeaves fileIdx)
          in case readParquetColumn pf rgIdx fileIdx fileFld of
               Left  _  -> Left (IncompatibleType (AT.fieldName fld) (AT.fieldType fld))
               Right c' -> coerceColumn (AT.fieldType fld) c'
                             `orLeft` IncompatibleType (AT.fieldName fld)
                                                       (AT.fieldType fld)
  where
    orLeft (Right x) _ = Right x
    orLeft (Left  _) e = Left e

-- | Best-effort column coercion for the projection path. The
-- coercion table is deliberately narrow: numeric widening
-- (Int32 → Int64, Float → Double) and identity reads. Returns
-- 'Left' for unsupported coercions; callers route that to
-- 'IncompatibleType'.
coerceColumn :: AT.ArrowType -> AC.ColumnArray -> Either String AC.ColumnArray
coerceColumn target col = case (target, col) of
  (AT.AInt 64 True, AC.ColInt32 v) ->
    Right $ AC.ColInt64 (VS.map (fromIntegral :: Int32 -> Int64) v)
  (AT.AInt 64 False, AC.ColInt32 v) ->
    Right $ AC.ColUInt64 (VS.map (fromIntegral :: Int32 -> Word64) v)
  (AT.AFloatingPoint AT.DoublePrecision, AC.ColFloat v) ->
    Right $ AC.ColDouble (VS.map (realToFrac :: Float -> Double) v)
  _ -> Left ("coerceColumn: " ++ show target ++ " <- " ++ show col)

-- | Read one column chunk and project it into a 'ColumnArray'.
-- Dispatches on the Arrow target type + nullability; falls back
-- to a clean 'Left' for shapes the bridge doesn't yet cover
-- (nullable strings need definition-level decoding which is the
-- caller's job today via 'Parquet.Read.readPlain*OptionalColumnChunk').
readParquetColumn
  :: PR.ParquetFile
  -> Int   -- ^ row-group index
  -> Int   -- ^ column index within the row group
  -> AT.Field
  -> Either String AC.ColumnArray
readParquetColumn pf rgIdx colIdx fld = do
  chunk <- PR.columnChunkSlice pf rgIdx colIdx
  let !codec = chunkCodec pf rgIdx colIdx
      !nullable = AT.fieldNullable fld
  -- Use the generic per-page dispatchers throughout: they handle
  -- every encoding the spec defines for the matching physical
  -- type (PLAIN, dictionary, DELTA_*, BYTE_STREAM_SPLIT) and
  -- both DATA_PAGE and DATA_PAGE_V2.
  case AT.fieldType fld of
    -- Non-nullable primitives. Parquet's reader now returns
    -- 'VS.Vector' directly, so the bridge is a no-op
    -- constructor application -- no per-chunk VS.convert
    -- copy.
    AT.AInt 32 True | not nullable ->
      AC.ColInt32 <$> PR.readGenericInt32ColumnChunk codec chunk
    AT.AInt 64 True | not nullable ->
      AC.ColInt64 <$> PR.readGenericInt64ColumnChunk codec chunk
    AT.AFloatingPoint AT.Single | not nullable ->
      AC.ColFloat <$> PR.readGenericFloatColumnChunk codec chunk
    AT.AFloatingPoint AT.DoublePrecision | not nullable ->
      AC.ColDouble <$> PR.readGenericDoubleColumnChunk codec chunk
    AT.ABool | not nullable ->
      -- BV path: per-page decode is one Bit.fromByteStringN
      -- on the wire bytes (Parquet's PLAIN BOOLEAN already
      -- stores LSB-first packed bits, which is exactly the
      -- VU.Vector Bit storage). No V.Vector Bool intermediate.
      AC.ColBool <$> PR.readGenericBoolColumnChunkBV codec chunk
    -- BV path: per-page writes offsets + dense data straight
    -- into the BinaryView buffers, no V.Vector ByteString
    -- intermediate. UTF-8 reinterpretation is zero-copy via
    -- AV.binaryViewToUtf8View (UTF-8 validation runs lazily on
    -- first per-row access through the Utf8View text-array
    -- cache).
    AT.AUtf8 | not nullable ->
      AC.ColUtf8 . AV.binaryViewToUtf8View
        <$> PR.readGenericByteArrayColumnChunkBV codec chunk
    AT.ABinary | not nullable ->
      AC.ColBinary
        <$> PR.readGenericByteArrayColumnChunkBV codec chunk
    -- Temporal non-nullable: read the underlying int stream and
    -- cast to the Arrow column flavour.
    AT.ADate AT.DateDay | not nullable ->
      AC.ColDate32 <$> PR.readGenericInt32ColumnChunk codec chunk
    AT.ADate AT.DateMillisecond | not nullable ->
      AC.ColDate64 <$> PR.readGenericInt64ColumnChunk codec chunk
    AT.ATime _ 32 | not nullable ->
      AC.ColTime32 <$> PR.readGenericInt32ColumnChunk codec chunk
    AT.ATime _ 64 | not nullable ->
      AC.ColTime64 <$> PR.readGenericInt64ColumnChunk codec chunk
    AT.ATimestamp _ _ | not nullable ->
      AC.ColTimestamp <$> PR.readGenericInt64ColumnChunk codec chunk
    AT.ADuration _ | not nullable ->
      AC.ColDuration <$> PR.readGenericInt64ColumnChunk codec chunk

    -- INT96 (legacy 12-byte timestamp) and FIXED_LEN_BYTE_ARRAY
    -- (UUIDs / float16 / decimal128 in fixed form). Both are
    -- exposed via 'ColFixedSizeBinary'; the bridge currently
    -- only handles the required-page case (PLAIN encoding).
    AT.AFixedSizeBinary 12 | not nullable ->
      AC.ColFixedSizeBinary 12 <$> PR.readPlainInt96ColumnChunk codec chunk
    AT.AFixedSizeBinary n | not nullable ->
      AC.ColFixedSizeBinary n <$>
        PR.readPlainFixedLenByteArrayColumnChunk n codec chunk

    -- Nullable primitives + temporals. The @*Optional@ readers
    -- take (max_repetition_level, max_definition_level); for a
    -- flat optional primitive these are (0, 1) — our bridge
    -- doesn't currently emit nested-optional columns through
    -- this path, so we hardcode the flat-optional pair.
    -- Nullable primitives + temporals: the Parquet reader
    -- returns @V.Vector (Maybe a)@; we lift to NullableView at
    -- the bridge with a sentinel of 0 for the null slots
    -- (consumers must check the validity bitmap before reading).
    -- Nullable primitives go through the flat-shape NV readers:
    -- per-page validity bits and dense storable values are
    -- written straight into VS / VU buffers and concatenated
    -- across pages with VS.concat / VU.concat. No
    -- V.Vector (Maybe a) intermediate, no per-row Just/Nothing
    -- box, no second walk through 'nvFromMaybeVector'.
    AT.AInt 32 True | nullable ->
      AC.ColInt32Maybe <$> PR.readGenericInt32OptionalColumnChunkNV codec 0 1 chunk
    AT.AInt 64 True | nullable ->
      AC.ColInt64Maybe <$> PR.readGenericInt64OptionalColumnChunkNV codec 0 1 chunk
    AT.AFloatingPoint AT.Single | nullable ->
      AC.ColFloatMaybe <$> PR.readGenericFloatOptionalColumnChunkNV codec 0 1 chunk
    AT.AFloatingPoint AT.DoublePrecision | nullable ->
      AC.ColDoubleMaybe <$> PR.readGenericDoubleOptionalColumnChunkNV codec 0 1 chunk
    AT.ABool | nullable ->
      -- Same NV fast path: writes validity bits + value bits
      -- straight into bit-packed VU buffers as the page is
      -- decoded; no V.Vector (Maybe Bool), no per-row pointer.
      AC.ColBoolMaybe <$> PR.readGenericBoolOptionalColumnChunkNV codec 0 1 chunk
    AT.AUtf8 | nullable -> do
      -- NV path: per-page builder writes validity, offsets,
      -- and contiguous data bytes straight into the
      -- NullableBinaryView buffers. Reinterpret the
      -- resulting binary view as a UTF-8 view (zero copy --
      -- both wrap the same offsets + data buffers).
      nbv <- PR.readGenericByteArrayOptionalColumnChunkNV codec 0 1 chunk
      Right $ AC.ColUtf8Maybe (AV.nullableBinaryToUtf8View nbv)
    AT.ABinary | nullable ->
      AC.ColBinaryMaybe
        <$> PR.readGenericByteArrayOptionalColumnChunkNV codec 0 1 chunk
    -- Same NV-flat fast path for nullable temporals — these
    -- all carry an underlying Int32 or Int64 stream.
    AT.ADate AT.DateDay | nullable ->
      AC.ColDate32Maybe <$> PR.readGenericInt32OptionalColumnChunkNV codec 0 1 chunk
    AT.ADate AT.DateMillisecond | nullable ->
      AC.ColDate64Maybe <$> PR.readGenericInt64OptionalColumnChunkNV codec 0 1 chunk
    AT.ATime _ 32 | nullable ->
      AC.ColTime32Maybe <$> PR.readGenericInt32OptionalColumnChunkNV codec 0 1 chunk
    AT.ATime _ 64 | nullable ->
      AC.ColTime64Maybe <$> PR.readGenericInt64OptionalColumnChunkNV codec 0 1 chunk
    AT.ATimestamp _ _ | nullable ->
      AC.ColTimestampMaybe <$> PR.readGenericInt64OptionalColumnChunkNV codec 0 1 chunk
    AT.ADuration _ | nullable ->
      AC.ColDurationMaybe <$> PR.readGenericInt64OptionalColumnChunkNV codec 0 1 chunk

    other ->
      Left $ "Parquet.Arrow: column type "
             <> show other
             <> " (nullable=" <> show nullable
             <> ") not yet supported by the read bridge; use the "
             <> "specialised readers in Parquet.Read"

-- | Look up the column's 'Compression' codec from the footer.
-- | Compute @max_repetition_level@ for a column chunk by
-- walking its 'cmPathInSchema' back through the file's schema
-- and counting 'Repeated' elements. Returns 'Nothing' if the
-- chunk's metadata is absent or the path can't be resolved
-- (in which case the caller should treat it as conservatively
-- repetition-bearing).
--
-- Spec: parquet-format/Nested-Layout.md, "Computing levels".
-- max_repetition_level = number of @REPEATED@ elements on the
-- path from the root to (and including) the leaf.
columnMaxRepetitionLevel :: PR.ParquetFile -> Int -> Int -> Maybe Int
columnMaxRepetitionLevel pf rgIdx colIdx = do
  rg <- (P.fmRowGroups (PR.pfFooter pf)) V.!? rgIdx
  cc <- P.rgColumns rg V.!? colIdx
  cm <- P.ccMetadata cc
  let !path     = P.cmPathInSchema cm
      !schemaV  = P.fmSchema (PR.pfFooter pf)
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
            _                                               -> 0

chunkCodec :: PR.ParquetFile -> Int -> Int -> P.Compression
chunkCodec pf rgIdx colIdx =
  let fm   = PR.pfFooter pf
      rgs  = P.fmRowGroups fm
      rg   = V.unsafeIndex rgs rgIdx
      cols = P.rgColumns rg
      cc   = V.unsafeIndex cols colIdx
  in case P.ccMetadata cc of
       Just md -> P.cmCodec md
       Nothing -> P.Uncompressed

-- | UTF-8 decode that swallows invalid sequences (replacing them
-- with U+FFFD). Real Arrow strings should always be valid UTF-8;
-- we don't want a single bad byte to fail an entire batch read.
decodeUtf8Lossy :: ByteString -> T.Text
decodeUtf8Lossy bs = case TE.decodeUtf8' bs of
  Right t -> t
  Left  _ -> TE.decodeUtf8With TE.lenientDecode bs

-- ============================================================
-- Arrow → Parquet.Nested (nested types)
-- ============================================================

-- | Map an Arrow 'AT.Field' onto a 'PN.NestedSchema' tree
-- suitable for 'Parquet.HighLevel.encodeParquetNested'. Handles
-- flat primitives (wraps them in 'PN.NSRequired' / 'PN.NSOptional'
-- based on 'fieldNullable'), struct, and list types.
arrowFieldToNestedSchema
  :: AT.Field -> Either String PN.NestedSchema
arrowFieldToNestedSchema f = do
  core <- case AT.fieldType f of
    AT.AInt 8  _      -> Right (PN.NSPrimitive PN.LtInt32)
    AT.AInt 16 _      -> Right (PN.NSPrimitive PN.LtInt32)
    AT.AInt 32 _      -> Right (PN.NSPrimitive PN.LtInt32)
    AT.AInt 64 _      -> Right (PN.NSPrimitive PN.LtInt64)
    AT.ABool          -> Right (PN.NSPrimitive PN.LtBool)
    AT.AFloatingPoint AT.Single          -> Right (PN.NSPrimitive PN.LtFloat)
    AT.AFloatingPoint AT.DoublePrecision -> Right (PN.NSPrimitive PN.LtDouble)
    AT.AUtf8          -> Right (PN.NSPrimitive PN.LtString)
    AT.ABinary        -> Right (PN.NSPrimitive PN.LtBinary)
    AT.ALargeUtf8     -> Right (PN.NSPrimitive PN.LtString)
    AT.ALargeBinary   -> Right (PN.NSPrimitive PN.LtBinary)
    AT.ADate _        -> Right (PN.NSPrimitive PN.LtInt32)
    AT.ATime _ 32     -> Right (PN.NSPrimitive PN.LtInt32)
    AT.ATime _ 64     -> Right (PN.NSPrimitive PN.LtInt64)
    AT.ATimestamp _ _ -> Right (PN.NSPrimitive PN.LtInt64)
    AT.ADuration _    -> Right (PN.NSPrimitive PN.LtInt64)
    AT.AStruct -> do
      childSchemas <- V.mapM
        (\c -> do
            inner <- arrowFieldToNestedSchema c
            Right (AT.fieldName c, inner))
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
        [structField] | V.length (AT.fieldChildren structField) == 2 ->
          let !kf = V.unsafeIndex (AT.fieldChildren structField) 0
              !vf = V.unsafeIndex (AT.fieldChildren structField) 1
          in do
            kSch <- arrowFieldToNestedSchema kf
            vSch <- arrowFieldToNestedSchema vf
            Right (PN.NSMap kSch vSch)
        _ -> Left "Parquet.Arrow: AMap's child must be a struct with exactly (key, value)"
    other -> Left $ "Parquet.Arrow.arrowFieldToNestedSchema: "
                    ++ show other
                    ++ " not yet supported"
  Right $ if AT.fieldNullable f
            then PN.NSOptional core
            else PN.NSRequired core

-- | Lower an Arrow 'AC.ColumnArray' to a row-major vector of
-- 'PN.NestedRow' entries. Handles struct, list, and flat
-- primitives; the row count matches the column's logical length.
columnArrayToNestedRows
  :: AC.ColumnArray -> Either String (V.Vector PN.NestedRow)
columnArrayToNestedRows col = case col of
  AC.ColInt32 v  -> Right (V.generate (VS.length v)
                             (\i -> PN.NRLeaf (PN.LvInt32 (VS.unsafeIndex v i))))
  AC.ColInt64 v  -> Right (V.generate (VS.length v)
                             (\i -> PN.NRLeaf (PN.LvInt64 (VS.unsafeIndex v i))))
  AC.ColFloat v  -> Right (V.generate (VS.length v)
                             (\i -> PN.NRLeaf (PN.LvFloat (VS.unsafeIndex v i))))
  AC.ColDouble v -> Right (V.generate (VS.length v)
                             (\i -> PN.NRLeaf (PN.LvDouble (VS.unsafeIndex v i))))
  AC.ColBool v   -> Right (V.generate (VU.length v)
                             (\i -> PN.NRLeaf (PN.LvBool (unBit (VU.unsafeIndex v i)))))
  AC.ColUtf8 v   -> Right (V.map (PN.NRLeaf . PN.LvString)
                                  (AV.utf8ViewToVector v))
  AC.ColBinary v -> Right (V.map (PN.NRLeaf . PN.LvBinary)
                                  (AV.binaryViewToVector v))
  AC.ColLargeUtf8 v -> Right (V.map (PN.NRLeaf . PN.LvString)
                                     (AV.largeUtf8ViewToVector v))
  AC.ColLargeBinary v -> Right (V.map (PN.NRLeaf . PN.LvBinary)
                                       (AV.largeBinaryViewToVector v))

  -- Nullable primitives: NRNull for Nothing, NRLeaf for Just.
  AC.ColInt32Maybe v -> Right (V.map (maybe PN.NRNull (PN.NRLeaf . PN.LvInt32)) (AC.nvToMaybeVector v))
  AC.ColInt64Maybe v -> Right (V.map (maybe PN.NRNull (PN.NRLeaf . PN.LvInt64)) (AC.nvToMaybeVector v))
  AC.ColFloatMaybe v -> Right (V.map (maybe PN.NRNull (PN.NRLeaf . PN.LvFloat)) (AC.nvToMaybeVector v))
  AC.ColDoubleMaybe v -> Right (V.map (maybe PN.NRNull (PN.NRLeaf . PN.LvDouble)) (AC.nvToMaybeVector v))
  AC.ColBoolMaybe v -> Right (V.map (maybe PN.NRNull (PN.NRLeaf . PN.LvBool))
                                      (AV.nullableBoolViewToMaybeVector v))
  AC.ColUtf8Maybe v -> Right (V.map (maybe PN.NRNull (PN.NRLeaf . PN.LvString))
                                     (AV.nullableUtf8ViewToMaybeVector v))
  AC.ColBinaryMaybe v -> Right (V.map (maybe PN.NRNull (PN.NRLeaf . PN.LvBinary))
                                       (AV.nullableBinaryViewToMaybeVector v))

  -- Struct: each row is an NRStruct of field values indexed in
  -- declared order. Every child must yield the same row count.
  AC.ColStruct childCols -> do
    childRows <- V.mapM (columnArrayToNestedRows . snd) childCols
    let !n = if V.null childRows then 0 else V.length (V.head childRows)
    when' (V.any ((/= n) . V.length) childRows) $
      Left "Parquet.Arrow: struct children have mismatched row counts"
    Right $ V.generate n
              (\i -> PN.NRStruct (V.map (V.! i) childRows))

  -- List: offsets[i..i+1] delimit the slice of the child column
  -- belonging to row i.
  AC.ColList offs child -> do
    childRows <- columnArrayToNestedRows child
    let !n = max 0 (VS.length offs - 1)
    Right $ V.generate n $ \i ->
      let !start = fromIntegral (VS.unsafeIndex offs i) :: Int
          !end   = fromIntegral (VS.unsafeIndex offs (i + 1)) :: Int
      in  PN.NRList (V.slice start (end - start) childRows)

  AC.ColLargeList offs child -> do
    childRows <- columnArrayToNestedRows child
    let !n = max 0 (VS.length offs - 1)
    Right $ V.generate n $ \i ->
      let !start = fromIntegral (VS.unsafeIndex offs i) :: Int
          !end   = fromIntegral (VS.unsafeIndex offs (i + 1)) :: Int
      in  PN.NRList (V.slice start (end - start) childRows)

  other -> Left $ "Parquet.Arrow.columnArrayToNestedRows: "
                    ++ show other
                    ++ " not yet supported (nullable-list, map, union, "
                    ++ "dictionary, view, REE, interval)"
  where
    when' True  e = e
    when' False _ = Right ()

-- ============================================================
-- Streaming reader
-- ============================================================

-- | Number of row groups in the file. Useful as a loop bound for
-- 'parquetRowGroupToArrow' / 'streamRowGroups'.
numRowGroups :: PR.ParquetFile -> Int
numRowGroups pf =
  V.length (P.fmRowGroups (PR.pfFooter pf))

-- | Lazily project every row group of a Parquet file into Arrow
-- columns, mirroring pyarrow's @ParquetFile.iter_batches()@. The
-- resulting list defers the per-row-group decode until the
-- consumer pulls the corresponding @Either@; failed row groups
-- surface their parse error in the @Left@ slot without aborting
-- the rest of the stream.
--
-- @
-- pf <- 'Parquet.HighLevel.decodeParquet' bytes
-- forM_ ('streamRowGroups' arrowSchema pf) $ \\rg -> case rg of
--   Right cols -> consume cols
--   Left  err  -> log err
-- @
streamRowGroups
  :: AT.Schema
  -> PR.ParquetFile
  -> [Either String (V.Vector AC.ColumnArray)]
streamRowGroups sch pf =
  [ case parquetRowGroupToArrow sch pf i of
      Right cols -> Right cols
      Left  err  -> Left (show err)
  | i <- [0 .. numRowGroups pf - 1]
  ]

-- | Iterator-shaped variant of 'streamRowGroups'. Each
-- 'IS.iterStep' decodes one row group on demand. Use
-- 'Columnar.Stream.iterTake' / 'Columnar.Stream.iterFold' /
-- friends to drive it without materialising every row group up
-- front.
--
-- Behaves like 'streamRowGroups' for the per-row-group decode
-- (same target Arrow schema controls projection / coercion);
-- the difference is that decoding errors halt the iterator at
-- the failing step instead of being threaded through a list.
streamRowGroupsIter
  :: AT.Schema
  -> PR.ParquetFile
  -> IS.Iter (V.Vector AC.ColumnArray)
streamRowGroupsIter sch pf =
  IS.iterFromIndexed (numRowGroups pf) $ \i ->
    case parquetRowGroupToArrow sch pf i of
      Right cols -> Right cols
      Left  err  -> Left (show err)

-- | Like 'streamRowGroupsIter' but projects each row group to a
-- subset of named columns (and an optional reordering). The
-- caller supplies the target Arrow schema /before/ projection;
-- this helper extracts the named subset and runs the
-- per-row-group decode against the narrower schema, so only the
-- requested columns are read off disk.
--
-- Names not present in the source schema cause every iterator
-- step to fail with the same error (matching the
-- 'Arrow.Stream.streamReaderProjectedIter' shape).
streamRowGroupsProjectedIter
  :: AT.Schema
  -> [Text]
  -> PR.ParquetFile
  -> IS.Iter (V.Vector AC.ColumnArray)
streamRowGroupsProjectedIter sch names pf =
  case projectFields names sch of
    Left e          -> IS.iterUnfold () (\_ -> Left e)
    Right narrowSch -> streamRowGroupsIter narrowSch pf

-- | Decode a single row group with column projection. Equivalent
-- to @'parquetRowGroupToArrow' (projectSchema names target) pf
-- rgIdx@ but checks the projection up front so the error path is
-- single-shot rather than per-column.
parquetRowGroupToArrowProjected
  :: AT.Schema
  -> [Text]
  -> PR.ParquetFile
  -> Int
  -> Either ProjectionError (V.Vector AC.ColumnArray)
parquetRowGroupToArrowProjected target names pf rgIdx = do
  narrow <- case projectFields names target of
    Right s -> Right s
    Left  _ -> Left (MissingColumn (T.pack "<projection>"))
  parquetRowGroupToArrow narrow pf rgIdx

-- | Iterator over row groups that drops any row group whose
-- statistics prove the predicate matches no rows.
--
-- Skipping is /sound/: only row groups whose
-- 'Parquet.Predicate.evalRowGroup' returns 'Pred.PSkip' are
-- elided. Row groups whose statistics are missing or
-- inconclusive are decoded normally and yielded as iterator
-- elements.
--
-- Returns the iterator paired with the planning summary
-- @(totalRowGroups, skippedRowGroups)@ so callers can log how
-- effective the predicate was without holding onto the file.
streamRowGroupsFilteredIter
  :: AT.Schema
  -> Pred.Predicate
  -> PR.ParquetFile
  -> (Int, Int, IS.Iter (V.Vector AC.ColumnArray))
streamRowGroupsFilteredIter sch predicate pf =
  let !leafNames = leafColumnNames pf
      !rgs       = P.fmRowGroups (PR.pfFooter pf)
      !nRg       = V.length rgs
      keep i =
        Pred.evalRowGroup leafNames predicate (V.unsafeIndex rgs i)
          == Pred.PMaybeKeep
      !kept   = V.filter keep (V.enumFromN 0 nRg)
      !nKept  = V.length kept
      !nSkip  = nRg - nKept
      step k =
        let !i = V.unsafeIndex kept k
        in case parquetRowGroupToArrow sch pf i of
             Right cols -> Right cols
             Left  err  -> Left (show err)
  in (nRg, nSkip, IS.iterFromIndexed nKept step)

-- | Combination of 'streamRowGroupsProjectedIter' and
-- 'streamRowGroupsFilteredIter': only decodes the named
-- columns of row groups whose statistics survive the
-- predicate.
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

-- | Leaf column names of a 'PR.ParquetFile' in the same order
-- the row groups' @rgColumns@ vectors use. Built from the
-- footer's flat schema (skipping the synthetic root struct).
leafColumnNames :: PR.ParquetFile -> V.Vector Text
leafColumnNames pf =
  V.map P.seName
        (V.filter (maybe False (const True) . P.seType)
                  (P.fmSchema (PR.pfFooter pf)))

-- | Read one column with page-level predicate pushdown.
--
-- Looks up the column chunk's 'OffsetIndex' + 'ColumnIndex',
-- evaluates the predicate against the 'ColumnIndex' to produce
-- a per-page keep mask, then decodes only the surviving pages
-- using the file-offset-based 'PR.readGeneric*SelectedPages'
-- family.
--
-- Handles both required and (top-level-)nullable columns
-- automatically, dispatching to the optional-pages decoder
-- when 'AT.fieldNullable' is set.
--
-- Returns 'Right (Nothing, ...)' when the column doesn't carry
-- a 'ColumnIndex' / 'OffsetIndex' pair (page-level pruning isn't
-- possible without the page-index region — fall back to
-- 'readParquetColumn'). Returns 'Right (Just (kept, total), col)'
-- when pruning ran, with @kept@ pages decoded out of @total@.
--
-- /Repeated columns are rejected/: a column whose path crosses
-- a 'Repeated' schema element (lists, maps, or any field with
-- @max_repetition_level > 0@) carries per-page repetition
-- streams that change the row count of every kept page. The
-- simple keep-mask shape this function exposes can't model
-- that, so it returns 'Left' with a typed error rather than
-- silently producing the wrong row count. Callers reading
-- nested columns should fall through to 'readParquetColumn'
-- (no pushdown) or to 'Parquet.Nested' for the Dremel-shredded
-- read path.
readParquetColumnWithPagePruning
  :: PR.ParquetFile
  -> Int                  -- ^ row-group index
  -> Int                  -- ^ column index within the row group
  -> AT.Field             -- ^ Arrow target field
  -> Pred.PColPredicate   -- ^ predicate to push down to the page index
  -> Either String (Maybe (Int, Int), AC.ColumnArray)
readParquetColumnWithPagePruning pf rgIdx colIdx fld predicate = do
  -- Refuse repeated columns up front (see haddock).
  case columnMaxRepetitionLevel pf rgIdx colIdx of
    Just rep | rep > 0 ->
      Left $ "Parquet.Arrow.readParquetColumnWithPagePruning: column "
              ++ show colIdx ++ " has max_repetition_level=" ++ show rep
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
          !keep      = V.map (== Pred.PMaybeKeep) decisions
          !total     = V.length keep
          !nKept     = V.length (V.filter id keep)
          !codec     = chunkCodec pf rgIdx colIdx
          !fileBs    = PR.pfBytes pf
          !locs      = P.oiPageLocations oi
      col <- if AT.fieldNullable fld
               then decodeSelectedOptionalColumn codec fileBs locs keep fld
               else decodeSelectedColumn codec fileBs locs keep fld
      Right (Just (nKept, total), col)
  where
    mapLeftShow :: Either e a -> Either String a
    mapLeftShow (Right x) = Right x
    mapLeftShow (Left _ ) = Left "Parquet.Arrow: page-pruning fallback failed"

loadIndices
  :: PR.ParquetFile
  -> Int -> Int
  -> Either String (Maybe (P.OffsetIndex, P.ColumnIndex, P.ParquetType))
loadIndices pf rgIdx colIdx = do
  mOff <- PI.readOffsetIndex pf rgIdx colIdx
  mCol <- PI.readColumnIndex pf rgIdx colIdx
  case (mOff, mCol) of
    (Just oi, Just ci) -> do
      let !rgs  = P.fmRowGroups (PR.pfFooter pf)
          !rg   = V.unsafeIndex rgs rgIdx
          !cc   = V.unsafeIndex (P.rgColumns rg) colIdx
      case P.ccMetadata cc of
        Just md -> Right (Just (oi, ci, P.cmType md))
        Nothing -> Right Nothing
    _ -> Right Nothing

decodeSelectedColumn
  :: P.Compression
  -> ByteString
  -> V.Vector P.PageLocation
  -> V.Vector Bool
  -> AT.Field
  -> Either String AC.ColumnArray
decodeSelectedColumn codec fileBs locs keep fld = case AT.fieldType fld of
  AT.AInt 32 True -> AC.ColInt32 . VS.convert <$> PR.readGenericInt32SelectedPages  codec fileBs locs keep
  AT.AInt 64 True -> AC.ColInt64 . VS.convert <$> PR.readGenericInt64SelectedPages  codec fileBs locs keep
  AT.AFloatingPoint AT.Single          ->
    AC.ColFloat . VS.convert <$> PR.readGenericFloatSelectedPages  codec fileBs locs keep
  AT.AFloatingPoint AT.DoublePrecision ->
    AC.ColDouble . VS.convert <$> PR.readGenericDoubleSelectedPages codec fileBs locs keep
  AT.ABool        -> AC.ColBool . boxedToBitVec <$> PR.readGenericBoolSelectedPages   codec fileBs locs keep
  AT.AUtf8        -> do
    bs <- PR.readGenericByteArraySelectedPages codec fileBs locs keep
    Right $ AC.ColUtf8 (AV.utf8ViewFromVector (V.map decodeUtf8Lossy bs))
  AT.ABinary      -> AC.ColBinary . AV.binaryViewFromVector
                        <$> PR.readGenericByteArraySelectedPages codec fileBs locs keep
  AT.ADate AT.DateDay         ->
    AC.ColDate32   . VS.convert <$> PR.readGenericInt32SelectedPages codec fileBs locs keep
  AT.ADate AT.DateMillisecond ->
    AC.ColDate64   . VS.convert <$> PR.readGenericInt64SelectedPages codec fileBs locs keep
  AT.ATime _ 32 ->
    AC.ColTime32   . VS.convert <$> PR.readGenericInt32SelectedPages codec fileBs locs keep
  AT.ATime _ 64 ->
    AC.ColTime64   . VS.convert <$> PR.readGenericInt64SelectedPages codec fileBs locs keep
  AT.ATimestamp _ _ ->
    AC.ColTimestamp . VS.convert <$> PR.readGenericInt64SelectedPages codec fileBs locs keep
  AT.ADuration _ ->
    AC.ColDuration  . VS.convert <$> PR.readGenericInt64SelectedPages codec fileBs locs keep
  other ->
    Left $ "Parquet.Arrow: page-pruning bridge doesn't yet cover "
            ++ show other

-- | Page-pruning variant for nullable columns. Same shape as
-- 'decodeSelectedColumn' but routes through the
-- @readGenericXxxOptionalSelectedPages@ family which carries
-- per-page def-level streams.
decodeSelectedOptionalColumn
  :: P.Compression
  -> ByteString
  -> V.Vector P.PageLocation
  -> V.Vector Bool
  -> AT.Field
  -> Either String AC.ColumnArray
decodeSelectedOptionalColumn codec fileBs locs keep fld = case AT.fieldType fld of
  AT.AInt 32 True ->
    AC.ColInt32Maybe . AC.nvFromMaybeVector 0
      <$> PR.readGenericInt32OptionalSelectedPages codec 0 1 fileBs locs keep
  AT.AInt 64 True ->
    AC.ColInt64Maybe . AC.nvFromMaybeVector 0
      <$> PR.readGenericInt64OptionalSelectedPages codec 0 1 fileBs locs keep
  AT.AFloatingPoint AT.Single ->
    AC.ColFloatMaybe . AC.nvFromMaybeVector 0
      <$> PR.readGenericFloatOptionalSelectedPages codec 0 1 fileBs locs keep
  AT.AFloatingPoint AT.DoublePrecision ->
    AC.ColDoubleMaybe . AC.nvFromMaybeVector 0
      <$> PR.readGenericDoubleOptionalSelectedPages codec 0 1 fileBs locs keep
  AT.ABool ->
    AC.ColBoolMaybe . AV.nullableBoolViewFromMaybeVector
      <$> PR.readGenericBoolOptionalSelectedPages codec 0 1 fileBs locs keep
  AT.AUtf8 -> do
    bs <- PR.readGenericByteArrayOptionalSelectedPages
            codec 0 1 fileBs locs keep
    Right $ AC.ColUtf8Maybe (AV.nullableUtf8ViewFromMaybeVector
                               (V.map (fmap decodeUtf8Lossy) bs))
  AT.ABinary ->
    AC.ColBinaryMaybe . AV.nullableBinaryViewFromMaybeVector
      <$> PR.readGenericByteArrayOptionalSelectedPages codec 0 1 fileBs locs keep
  AT.ADate AT.DateDay ->
    AC.ColDate32Maybe . AC.nvFromMaybeVector 0
      <$> PR.readGenericInt32OptionalSelectedPages codec 0 1 fileBs locs keep
  AT.ADate AT.DateMillisecond ->
    AC.ColDate64Maybe . AC.nvFromMaybeVector 0
      <$> PR.readGenericInt64OptionalSelectedPages codec 0 1 fileBs locs keep
  AT.ATime _ 32 ->
    AC.ColTime32Maybe . AC.nvFromMaybeVector 0
      <$> PR.readGenericInt32OptionalSelectedPages codec 0 1 fileBs locs keep
  AT.ATime _ 64 ->
    AC.ColTime64Maybe . AC.nvFromMaybeVector 0
      <$> PR.readGenericInt64OptionalSelectedPages codec 0 1 fileBs locs keep
  AT.ATimestamp _ _ ->
    AC.ColTimestampMaybe . AC.nvFromMaybeVector 0
      <$> PR.readGenericInt64OptionalSelectedPages codec 0 1 fileBs locs keep
  AT.ADuration _ ->
    AC.ColDurationMaybe . AC.nvFromMaybeVector 0
      <$> PR.readGenericInt64OptionalSelectedPages codec 0 1 fileBs locs keep
  other ->
    Left $ "Parquet.Arrow: nullable page-pruning bridge doesn't yet cover "
            ++ show other

-- | Build a sub-schema by name. Preserves the order of @names@.
projectFields :: [Text] -> AT.Schema -> Either String AT.Schema
projectFields names sch =
  let !fields = AT.arrowFields sch
      !byName = Map.fromList
        [ (AT.fieldName f, f) | f <- V.toList fields ]
      pickOne nm = case Map.lookup nm byName of
        Just f  -> Right f
        Nothing -> Left $ "Parquet.Arrow: projected column "
                          ++ show nm ++ " not present in target schema"
  in do
    fs <- traverse pickOne names
    Right sch { AT.arrowFields = V.fromList fs }
