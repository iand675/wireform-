{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Hedgehog generators for columnar-format round-trip
properties. Three layered generators:

    'genCrossFormat'     -- shapes every format round-trips identically
    'genArrowOnly'       -- Arrow's full ColumnArray coverage
    'genParquetBridge'   -- what the Parquet Arrow-bridge supports
    'genORCBridge'       -- what the ORC Arrow-bridge supports

Each generator produces a 'Plan' list + batches, sharing the
underlying 'ColumnPlan' types so bodies of properties stay
small. See the per-generator haddock for the rationale behind
each shape's inclusion / exclusion.
-}
module Test.Columnar.Gen (
  -- * Plans
  ColumnPlan (..),
  columnPlanToField,
  columnPlanToColumnArray,

  -- * Generators
  genCrossFormat,
  genArrowOnly,
  genParquetBridge,
  genORCBridge,
) where

import Arrow.Column qualified as AC
import Arrow.Types qualified as AT
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range


-- ============================================================
-- Plans
-- ============================================================

{- | Decoupled description of a single column that lets us emit
both the 'AT.Field' and the 'AC.ColumnArray' for a batch from
one source of truth. Generating the two sides independently
would let them drift (field type disagreeing with column
constructor).
-}
data ColumnPlan
  = PlanInt32 !T.Text !Bool
  | PlanInt64 !T.Text !Bool
  | PlanInt16 !T.Text !Bool
  | PlanBool !T.Text !Bool
  | PlanFloat !T.Text !Bool
  | PlanDouble !T.Text !Bool
  | PlanUtf8 !T.Text !Bool
  | PlanBinary !T.Text !Bool
  | PlanDate32 !T.Text !Bool
  | PlanTimestamp !T.Text !Bool
  deriving (Show, Eq)


-- | Project to the Arrow schema field advertised for this plan.
columnPlanToField :: ColumnPlan -> AT.Field
columnPlanToField cp = case cp of
  PlanInt32 n b -> field n b (AT.AInt 32 True)
  PlanInt64 n b -> field n b (AT.AInt 64 True)
  PlanInt16 n b -> field n b (AT.AInt 16 True)
  PlanBool n b -> field n b AT.ABool
  PlanFloat n b -> field n b (AT.AFloatingPoint AT.Single)
  PlanDouble n b -> field n b (AT.AFloatingPoint AT.DoublePrecision)
  PlanUtf8 n b -> field n b AT.AUtf8
  PlanBinary n b -> field n b AT.ABinary
  PlanDate32 n b -> field n b (AT.ADate AT.DateDay)
  PlanTimestamp n b -> field n b (AT.ATimestamp AT.Microsecond Nothing)
  where
    field name nullable ty =
      AT.Field
        { AT.fieldName = name
        , AT.fieldNullable = nullable
        , AT.fieldType = ty
        , AT.fieldChildren = V.empty
        , AT.fieldDictionary = Nothing
        , AT.fieldMetadata = V.empty
        }


{- | Generate a 'AC.ColumnArray' holding @nRows@ values matching
the plan. Nullable variants include nulls at ~30% density.
-}
columnPlanToColumnArray :: Int -> ColumnPlan -> Gen AC.ColumnArray
columnPlanToColumnArray nRows cp = case cp of
  PlanInt32 _ False ->
    AC.primColumn AC.PInt32 . VS.fromList <$> rows genInt32
  PlanInt32 _ True ->
    AC.fromMaybes AC.PInt32 . V.fromList <$> rows (genMaybe genInt32)
  PlanInt64 _ False ->
    AC.primColumn AC.PInt64 . VS.fromList <$> rows genInt64
  PlanInt64 _ True ->
    AC.fromMaybes AC.PInt64 . V.fromList <$> rows (genMaybe genInt64)
  PlanInt16 _ False ->
    AC.primColumn AC.PInt16 . VS.fromList <$> rows genInt16
  PlanInt16 _ True ->
    AC.fromMaybes AC.PInt16 . V.fromList <$> rows (genMaybe genInt16)
  PlanBool _ False ->
    AC.fromBools . V.fromList <$> rows Gen.bool
  PlanBool _ True ->
    AC.fromMaybeBools . V.fromList <$> rows (genMaybe Gen.bool)
  PlanFloat _ False ->
    AC.primColumn AC.PFloat . VS.fromList <$> rows genFloat
  PlanFloat _ True ->
    AC.fromMaybes AC.PFloat . V.fromList <$> rows (genMaybe genFloat)
  PlanDouble _ False ->
    AC.primColumn AC.PDouble . VS.fromList <$> rows genDouble
  PlanDouble _ True ->
    AC.fromMaybes AC.PDouble . V.fromList <$> rows (genMaybe genDouble)
  PlanUtf8 _ False ->
    AC.fromTexts . V.fromList <$> rows genText
  PlanUtf8 _ True ->
    AC.fromMaybeTexts . V.fromList <$> rows (genMaybe genText)
  PlanBinary _ False ->
    AC.fromByteStrings . V.fromList <$> rows genBytes
  PlanBinary _ True ->
    AC.fromMaybeByteStrings . V.fromList <$> rows (genMaybe genBytes)
  PlanDate32 _ False ->
    AC.primColumn AC.PDate32 . VS.fromList <$> rows genDays
  PlanDate32 _ True ->
    AC.fromMaybes AC.PDate32 . V.fromList <$> rows (genMaybe genDays)
  PlanTimestamp _ False ->
    AC.primColumn AC.PTimestamp . VS.fromList <$> rows genMicros
  PlanTimestamp _ True ->
    AC.fromMaybes AC.PTimestamp . V.fromList <$> rows (genMaybe genMicros)
  where
    rows g = Gen.list (Range.singleton nRows) g


-- ============================================================
-- Primitive value generators
-- ============================================================

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g =
  Gen.frequency
    [ (3, pure Nothing)
    , (7, Just <$> g)
    ]


genInt16 :: Gen Int16
genInt16 = Gen.int16 (Range.linearFrom 0 minBound maxBound)


genInt32 :: Gen Int32
genInt32 = Gen.int32 (Range.linearFrom 0 minBound maxBound)


genInt64 :: Gen Int64
genInt64 = Gen.int64 (Range.linearFrom 0 minBound maxBound)


-- Float / Double ranges avoid NaN + Infinity (which don't survive
-- equality round-trips by definition).
genFloat :: Gen Float
genFloat = Gen.float (Range.linearFrac (-1e6) 1e6)


genDouble :: Gen Double
genDouble = Gen.double (Range.linearFrac (-1e9) 1e9)


-- UTF-8 strings restricted to alphanumerics (readable failures,
-- no interior surrogates).
genText :: Gen T.Text
genText = Gen.text (Range.linear 0 20) Gen.alphaNum


-- Binary: printable-ASCII bytes. ORC's DIRECT_V2 string writer
-- round-trips arbitrary bytes through UTF-8 decoding with
-- replacement; arbitrary high bytes wouldn't survive. Narrowing
-- to printable-ASCII is the LCD across all three formats.
genBytes :: Gen ByteString
genBytes =
  BS.pack <$> Gen.list (Range.linear 0 32) (Gen.word8 (Range.linear 0x20 0x7e))


-- | Days since Unix epoch, plausible 1970..2100 window.
genDays :: Gen Int32
genDays = Gen.int32 (Range.linear 0 47_482)


-- | Microseconds since Unix epoch, 1970..2100.
genMicros :: Gen Int64
genMicros = Gen.int64 (Range.linear 0 4_102_444_800_000_000)


genColumnName :: Gen T.Text
genColumnName = Gen.text (Range.linear 1 6) Gen.alphaNum


-- ============================================================
-- Schema assembly
-- ============================================================

{- | Generate @n@ distinct-by-name plans using the plan-ctor
generator the caller supplies. Regenerates a plan whose name
collides with one already accepted.
-}
uniqueByName
  :: (T.Text -> Bool -> Gen ColumnPlan)
  -- ^ how to construct a single plan for a given name + nullability
  -> Int
  -> Gen [ColumnPlan]
uniqueByName mk = go []
  where
    go acc 0 = pure (reverse acc)
    go acc n = do
      name <- genColumnName
      if name `elem` map planName acc
        then go acc n
        else do
          nullable <- Gen.bool
          p <- mk name nullable
          go (p : acc) (n - 1)

    planName p = case p of
      PlanInt32 n _ -> n
      PlanInt64 n _ -> n
      PlanInt16 n _ -> n
      PlanBool n _ -> n
      PlanFloat n _ -> n
      PlanDouble n _ -> n
      PlanUtf8 n _ -> n
      PlanBinary n _ -> n
      PlanDate32 n _ -> n
      PlanTimestamp n _ -> n


{- | Wrap a list of plans as an Arrow 'AT.Schema' and generate
1..3 batches of 1..20 rows each.
-}
schemaAndBatches
  :: [ColumnPlan] -> Gen (AT.Schema, [V.Vector AC.ColumnArray])
schemaAndBatches plans = do
  nBatches <- Gen.int (Range.linear 1 3)
  batches <- Gen.list (Range.singleton nBatches) (genBatch plans)
  let !sch =
        AT.Schema
          { AT.arrowFields = V.fromList (map columnPlanToField plans)
          , AT.arrowEndianness = AT.Little
          , AT.arrowMetadata = V.empty
          , AT.arrowFeatures = V.empty
          }
  pure (sch, batches)


genBatch :: [ColumnPlan] -> Gen (V.Vector AC.ColumnArray)
genBatch plans = do
  nRows <- Gen.int (Range.linear 1 20)
  cols <- traverse (columnPlanToColumnArray nRows) plans
  pure (V.fromList cols)


-- ============================================================
-- Per-target generators
-- ============================================================

{- | Shapes that every format (Arrow stream, Arrow file, Parquet,
ORC) round-trips /identically/, preserving nullability + width.

Why this list:

  * Int32 / Int64: native physical types in all three.
  * Bool / Float / Double: native.
  * Utf8: LogicalType LTString on Parquet, TKString on ORC.
  * Date32: INT32 + LogicalType LTDate on Parquet, TKDate on ORC.
  * Timestamp(microsecond): INT64 + LogicalType on Parquet,
    TKTimestamp on ORC.

/Non-null/ only, the Parquet Arrow-bridge currently drops
nulls on write. Nullable variants are exercised by the
per-format generators below.
-}
genCrossFormat :: Gen (AT.Schema, [V.Vector AC.ColumnArray])
genCrossFormat = do
  nCols <- Gen.int (Range.linear 1 4)
  plans <- uniqueByName mk nCols
  schemaAndBatches plans
  where
    mk name _ = do
      ctor <-
        Gen.element
          [ PlanInt32
          , PlanInt64
          , PlanBool
          , PlanFloat
          , PlanDouble
          , PlanUtf8
          , PlanDate32
          , PlanTimestamp
          ]
      pure (ctor name False)


{- | Shapes the Arrow format (stream + file) round-trips; nullable
variants + Int16 + Binary all survive.
-}
genArrowOnly :: Gen (AT.Schema, [V.Vector AC.ColumnArray])
genArrowOnly = do
  nCols <- Gen.int (Range.linear 1 5)
  plans <- uniqueByName mk nCols
  schemaAndBatches plans
  where
    mk name nullable = do
      ctor <-
        Gen.element
          [ PlanInt32
          , PlanInt64
          , PlanInt16
          , PlanBool
          , PlanFloat
          , PlanDouble
          , PlanUtf8
          , PlanBinary
          , PlanDate32
          , PlanTimestamp
          ]
      pure (ctor name nullable)


{- | Shapes the Parquet bridge supports today (flat primitives
plus temporals, required + nullable, nullable columns now
route through 'Parquet.HighLevel.encodeParquetMixed' + the
@*Optional@ reader dispatch so nulls round-trip through
definition-level streams).
-}
genParquetBridge :: Gen (AT.Schema, [V.Vector AC.ColumnArray])
genParquetBridge = do
  nCols <- Gen.int (Range.linear 1 5)
  plans <- uniqueByName mk nCols
  schemaAndBatches plans
  where
    mk name nullable = do
      ctor <-
        Gen.element
          [ PlanInt32
          , PlanInt64
          , PlanBool
          , PlanFloat
          , PlanDouble
          , PlanUtf8
          , PlanBinary
          , PlanDate32
          , PlanTimestamp
          ]
      pure (ctor name nullable)


{- | Shapes the ORC bridge supports today (flat primitives +
temporals, nullable via PRESENT stream). Int16 excluded
because ORC's TKShort round-trips through Int32 internally.
-}
genORCBridge :: Gen (AT.Schema, [V.Vector AC.ColumnArray])
genORCBridge = do
  nCols <- Gen.int (Range.linear 1 5)
  plans <- uniqueByName mk nCols
  schemaAndBatches plans
  where
    mk name nullable = do
      ctor <-
        Gen.element
          [ PlanInt32
          , PlanInt64
          , PlanBool
          , PlanFloat
          , PlanDouble
          , PlanUtf8
          , PlanBinary
          , PlanDate32
          , PlanTimestamp
          ]
      pure (ctor name nullable)
