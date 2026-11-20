{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

{- | Predicate pushdown helpers for Parquet readers.

The Parquet spec already gives the reader several places to
avoid touching column data:

  * Row-group 'Statistics' carry @min_value@ / @max_value@ /
    @null_count@ for every column chunk.
  * Page-level 'ColumnIndex' carries the same triple per page,
    plus a 'BoundaryOrder' hint and an optional null-pages
    bitmap.
  * Per-column 'BloomFilter' (Sbbf) lets the reader prove a
    value is /not/ present in the column chunk.

This module bundles the three signals behind a single
'Predicate' ADT so callers can express filters once and ask
the planner whether a row group / page can be skipped without
decoding it.

The vocabulary is deliberately narrow — equality, range, and
membership over the same scalar value space (@PValue@) the
writer's statistics use. Compound boolean structure
(@'PAnd'@, @'POr'@, @'PNot'@) composes up from there.

@
import qualified Parquet.Predicate as Pred

let !p = 'PAnd' ('PCol' "ts" ('PGtEq' ('PVInt64' 1700000000)))
               ('PCol' "ts" ('PLt'   ('PVInt64' 1700100000)))
skipRowGroup :: 'P.RowGroup' -> Bool
skipRowGroup rg = 'evalRowGroup' fileMetadata p rg == 'PSkip'
@

The evaluator never produces a /false negative/ skip
(returning 'PSkip' when in fact some rows match) — if there's
any uncertainty (missing statistics, types we don't compare)
the answer is 'PMaybeKeep' and the caller falls back to
decoding the column chunk normally.
-}
module Parquet.Predicate (
  -- * Predicate vocabulary

  --

  {- | Re-exported from "Columnar.Predicate" so the same
  @PValue@ / @PColPredicate@ / @Predicate@ surface drives
  skip decisions in both Parquet and ORC.
  -}
  Predicate (..),
  PColPredicate (..),
  PValue (..),

  -- * Evaluation results
  Decision (..),
  combineDecisions,
  StatisticsContext (..),
  statisticsContext,

  -- * Statistics-based skipping
  evalRowGroup,
  evalColumnChunk,

  -- * Page-index-based skipping
  evalPagesByColumnIndex,

  -- * Bloom-filter membership
  evalBloomChunk,

  -- * Conversions
  decodePValueLE,
  decodePValueForSchema,
  minimumStatistic,
  maximumStatistic,
) where

import Arrow.Column qualified as AC
import Columnar.Predicate (
  Decision (..),
  PColPredicate (..),
  PValue (..),
  Predicate (..),
  combineDecisions,
  evalRange,
 )
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import Parquet.BloomFilter qualified as Bloom
import Parquet.Decimal (decodeDecimalInteger)
import Parquet.Types qualified as P


-- ============================================================
-- Statistics-based skipping
-- ============================================================

{- | Logical type plus the file's declared ordering. Missing orders cannot
give meaning to modern bounds; well-defined legacy numeric bounds remain
usable independently of this optional declaration.
-}
data StatisticsContext = StatisticsContext
  { statisticsField :: !P.SchemaElement
  , statisticsOrder :: !(Maybe P.ColumnOrder)
  }
  deriving (Show, Eq)


statisticsContext :: P.FileMetadata -> Int -> Maybe StatisticsContext
statisticsContext metadata index = do
  field <- V.filter (maybe False (const True) . P.seType) (P.fmSchema metadata) V.!? index
  Just (StatisticsContext field (P.fmColumnOrders metadata >>= (V.!? index)))


modernOrderSupported :: StatisticsContext -> Bool
modernOrderSupported context =
  P.hasDefinedStatisticsOrder field && case statisticsOrder context of
    Just P.TypeDefinedOrder -> P.seType field /= Just P.PTInt96
    Just P.IEEE754TotalOrder ->
      P.seType field `elem` [Just P.PTFloat, Just P.PTDouble]
        || P.seLogicalType field == Just P.LTFloat16
    Just P.Int96TimestampOrder -> P.seType field == Just P.PTInt96
    _ -> False
  where
    field = statisticsField context


{- | Decide whether an entire row group can be skipped.

Walks the predicate tree, looking up each leaf's column by
column path in the file schema. The schema retains logical annotations
so decimal and unsigned bounds use their numeric ordering. Missing columns / missing stats degrade
gracefully to 'PMaybeKeep'.
-}
evalRowGroup
  :: P.FileMetadata
  -- ^ File schema including logical annotations; nested columns use dotted paths.
  -> Predicate
  -> P.RowGroup
  -> Decision
evalRowGroup metadata p rg = evalPredicate lookupStats p
  where
    leaves = V.filter (maybe False (const True) . P.seType) (P.fmSchema metadata)
    lookupStats name = do
      let matches =
            V.findIndices
              ( \column -> case P.ccMetadata column of
                  Just columnMetadata ->
                    P.cmPathInSchema columnMetadata == V.singleton name
                      || name == T.intercalate "." (V.toList (P.cmPathInSchema columnMetadata))
                  _ -> False
              )
              (P.rgColumns rg)
      index <-
        if V.length leaves == V.length (P.rgColumns rg) && V.length matches == 1
          then Just (V.head matches)
          else Nothing
      context <- statisticsContext metadata index
      let field = statisticsField context
      column <- P.rgColumns rg V.!? index
      columnMetadata <- P.ccMetadata column
      statistics <- P.cmStatistics columnMetadata
      if P.seType field == Just (P.cmType columnMetadata)
        then Just (context, statistics)
        else Nothing


{- | Decide whether one column chunk can be skipped, given the
predicate fragment that applies to its column. Useful when
the caller has already projected the predicate per column.
-}
evalColumnChunk
  :: StatisticsContext
  -> P.ColumnChunk
  -> PColPredicate
  -> Decision
evalColumnChunk context cc cp = case P.ccMetadata cc of
  Nothing -> PMaybeKeep
  Just md -> case P.cmStatistics md of
    Nothing -> PMaybeKeep
    Just stats | P.seType (statisticsField context) == Just (P.cmType md) -> evalLeaf context stats cp
    _ -> PMaybeKeep


evalPredicate
  :: (Text -> Maybe (StatisticsContext, P.Statistics))
  -> Predicate
  -> Decision
evalPredicate _ PTrue = PMaybeKeep
evalPredicate _ PFalse = PSkip
evalPredicate look (PCol name cp) = case look name of
  Nothing -> PMaybeKeep
  Just (ty, stats) -> evalLeaf ty stats cp
evalPredicate look (PAnd a b) =
  combineDecisions (evalPredicate look a) (evalPredicate look b)
evalPredicate look (POr a b) =
  -- For OR we can only skip if /both/ disjuncts skip. The
  -- conservative answer otherwise is 'PMaybeKeep'.
  case (evalPredicate look a, evalPredicate look b) of
    (PSkip, PSkip) -> PSkip
    _ -> PMaybeKeep
evalPredicate look (PNot inner) = case evalPredicate look inner of
  -- Negation can't be pushed in general (a 'PSkip' on the
  -- inner predicate doesn't imply skipping for the negation).
  -- The conservative answer is always 'PMaybeKeep'.
  _ -> case inner of
    PFalse -> PMaybeKeep
    _ -> PMaybeKeep


-- ============================================================
-- Leaf predicates
-- ============================================================

{- | Evaluate one column-leaf predicate against the column
chunk's 'Statistics'. The 'ParquetType' tells us how to
decode the bytes inside @statMinValue@ / @statMaxValue@.
-}
evalLeaf :: StatisticsContext -> P.Statistics -> PColPredicate -> Decision
evalLeaf _ stats PIsNull
  -- IS NULL is skippable iff null_count is known to be zero.
  | P.statNullCount stats == Just 0 = PSkip
  | otherwise = PMaybeKeep
evalLeaf _ stats PIsNotNull
  -- IS NOT NULL is skippable iff every row is null. Without a
  -- per-chunk row count we can't be sure; fall back to maybe.
  | otherwise = case P.statNullCount stats of
      Just _ -> PMaybeKeep -- conservative
      Nothing -> PMaybeKeep
evalLeaf field stats cp =
  case (minimumStatistic field stats, maximumStatistic field stats) of
    (Just mn, Just mx) -> evalRange mn mx cp
    _ -> PMaybeKeep


minimumStatistic :: StatisticsContext -> P.Statistics -> Maybe PValue
minimumStatistic field stats = statisticValue field (P.statMinValue stats) (P.statMin stats)


maximumStatistic :: StatisticsContext -> P.Statistics -> Maybe PValue
maximumStatistic field stats = statisticValue field (P.statMaxValue stats) (P.statMax stats)


statisticValue :: StatisticsContext -> Maybe ByteString -> Maybe ByteString -> Maybe PValue
statisticValue context current legacy =
  ( case current of
      Just value | modernOrderSupported context -> Just value
      _ | legacyOrderMatches -> legacy
      _ -> Nothing
  )
    >>= decodePValueForSchema field
  where
    field = statisticsField context
    legacyOrderMatches =
      P.seType field `elem` (Just <$> [P.PTBoolean, P.PTInt32, P.PTInt64, P.PTFloat, P.PTDouble]) && case P.seLogicalType field of
        Just (P.LTInteger _ False) -> False
        Just P.LTDecimal {} -> P.seType field `elem` [Just P.PTInt32, Just P.PTInt64]
        _ -> P.seConvertedType field `notElem` (Just <$> [P.CTUInt8, P.CTUInt16, P.CTUInt32, P.CTUInt64, P.CTDecimal])


-- ============================================================
-- Min / max decoding
-- ============================================================

{- | Decode a min/max byte payload according to its
'P.ParquetType'. Mirrors what the writer's
@statisticsFor*@ helpers in "Parquet.Write" produced.
-}
decodePValueLE :: P.ParquetType -> ByteString -> Maybe PValue
decodePValueLE ty bs = case ty of
  P.PTBoolean ->
    case BS.uncons bs of
      Just (b, rest) | BS.null rest && b <= 1 -> Just (PVBool (b /= 0))
      Just _ -> Nothing
      Nothing -> Nothing
  P.PTInt32
    | BS.length bs == 4 ->
        Just (PVInt32 (fromIntegral (readLE32 bs 0)))
  P.PTInt64
    | BS.length bs == 8 ->
        Just (PVInt64 (fromIntegral (readLE64 bs 0)))
  P.PTFloat
    | BS.length bs == 4 ->
        let value = castWord32ToFloat (readLE32 bs 0)
        in if isNaN value then Nothing else Just (PVFloat value)
  P.PTDouble
    | BS.length bs == 8 ->
        let value = castWord64ToDouble (readLE64 bs 0)
        in if isNaN value then Nothing else Just (PVDouble value)
  P.PTByteArray ->
    -- Stats store the bytes without the PLAIN length prefix
    -- (per spec). Treat them as opaque binary; consumers can
    -- coerce to text if they know the column is UTF-8.
    Just (PVBinary bs)
  P.PTFixedLenByteArray ->
    Just (PVBinary bs)
  _ -> Nothing


{- | Decode statistics using the logical annotation as well as its carrier.
Exact decimal scalars cover the Arrow decimal256 precision range (76 digits).
-}
decodePValueForSchema :: P.SchemaElement -> ByteString -> Maybe PValue
decodePValueForSchema field bytes
  | not (P.hasDefinedStatisticsOrder field) = Nothing
  | otherwise = case P.seLogicalType field of
      Just P.LTFloat16
        | BS.length bytes == 2 ->
            let bits = fromIntegral (BS.index bytes 0) .|. (fromIntegral (BS.index bytes 1) `shiftL` 8)
                value = AC.float16ToDouble (AC.Float16 bits)
            in if isNaN value then Nothing else Just (PVDouble value)
      Just P.LTFloat16 -> Nothing
      Just P.LTTimestamp {} | P.seType field /= Just P.PTInt64 -> Nothing
      Just (P.LTDecimal precision scale)
        | precision > 0 && precision <= 76 && scale >= 0 && scale <= precision -> do
            value <- case P.seType field of
              Just P.PTInt32 | BS.length bytes == 4 -> Just (fromIntegral (fromIntegral (readLE32 bytes 0) :: Int32))
              Just P.PTInt64 | BS.length bytes == 8 -> Just (fromIntegral (fromIntegral (readLE64 bytes 0) :: Int64))
              Just P.PTByteArray -> decodeDecimalInteger bytes
              Just P.PTFixedLenByteArray -> decodeDecimalInteger bytes
              _ -> Nothing
            Just (PVDecimal (fromInteger value / fromInteger (10 ^ scale)))
        | otherwise -> Nothing
      _
        | unsigned -> case P.seType field of
            Just P.PTInt32 | BS.length bytes == 4 -> Just (PVUnsigned (fromIntegral (readLE32 bytes 0)))
            Just P.PTInt64 | BS.length bytes == 8 -> Just (PVUnsigned (readLE64 bytes 0))
            _ -> Nothing
        | text -> either (const Nothing) (Just . PVText) (TE.decodeUtf8' bytes)
        | otherwise -> P.seType field >>= (`decodePValueLE` bytes)
  where
    unsigned = case P.seLogicalType field of
      Just (P.LTInteger _ signed) -> not signed
      _ -> P.seConvertedType field `elem` (Just <$> [P.CTUInt8, P.CTUInt16, P.CTUInt32, P.CTUInt64])
    text =
      P.seType field == Just P.PTByteArray
        && ( P.seLogicalType field `elem` [Just P.LTString, Just P.LTEnum, Just P.LTJson]
               || P.seConvertedType field `elem` [Just P.CTUtf8, Just P.CTEnum, Just P.CTJson]
           )


readLE32 :: ByteString -> Int -> Word32
readLE32 bs o =
  let b0 = fromIntegral (BS.index bs o) :: Word32
      b1 = fromIntegral (BS.index bs (o + 1)) :: Word32
      b2 = fromIntegral (BS.index bs (o + 2)) :: Word32
      b3 = fromIntegral (BS.index bs (o + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)


readLE64 :: ByteString -> Int -> Word64
readLE64 bs o =
  let lo = fromIntegral (readLE32 bs o) :: Word64
      hi = fromIntegral (readLE32 bs (o + 4)) :: Word64
  in lo .|. (hi `shiftL` 32)


-- ============================================================
-- Page-index-based skipping
-- ============================================================

{- | Per-page skip decisions for one column chunk, derived from
the chunk's 'P.ColumnIndex'. The result vector is parallel
to @ciNullPages@ / @ciMinValues@ / @ciMaxValues@: an entry
of 'PSkip' means the corresponding 'P.PageLocation' (in the
chunk's matching 'P.OffsetIndex') can be skipped without
decoding.

Pages whose @null_pages@ slot is 'True' (the page contains
only nulls) are skipped for /any/ value-style predicate
('PEq', 'PLt', …); 'PIsNull' keeps them.
-}
evalPagesByColumnIndex
  :: StatisticsContext
  -> P.ColumnIndex
  -> PColPredicate
  -> V.Vector Decision
evalPagesByColumnIndex context ci cp =
  let !nulls = P.ciNullPages ci
      !mins = P.ciMinValues ci
      !maxs = P.ciMaxValues ci
      !n = V.length nulls
  in if V.length mins /= n || V.length maxs /= n
       then V.replicate n PMaybeKeep
       else V.generate n $ \i ->
         let !isNull = V.unsafeIndex nulls i
         in if isNull
              then case cp of
                PIsNull -> PMaybeKeep
                PIsNotNull -> PSkip
                _ -> PSkip
              else case ( decodeBound (V.unsafeIndex mins i)
                        , decodeBound (V.unsafeIndex maxs i)
                        ) of
                (Just mn, Just mx) -> evalRange mn mx cp
                _ -> PMaybeKeep
  where
    decodeBound bytes
      | modernOrderSupported context = decodePValueForSchema (statisticsField context) bytes
      | otherwise = Nothing


-- ============================================================
-- Bloom-filter membership
-- ============================================================

{- | Probe a column chunk's bloom filter for membership of the
value(s) referenced by the predicate. Returns 'PSkip' when
the bloom proves the column /cannot/ contain any matching
value; 'PMaybeKeep' otherwise.

Only @PEq@ and @PIn@ are bloom-checkable; other predicates
always degrade to 'PMaybeKeep' here (range / nullity have
to use min/max stats instead).
-}
evalBloomChunk
  :: P.SchemaElement
  -> Bloom.Sbbf
  -> PColPredicate
  -> Decision
evalBloomChunk field sbbf = \case
  PEq v -> case bloomCheckVal field sbbf v of
    True -> PMaybeKeep
    False -> PSkip
  PIn vs ->
    -- Skip iff every candidate is rejected by the filter.
    if all (not . bloomCheckVal field sbbf) vs
      then PSkip
      else PMaybeKeep
  _ -> PMaybeKeep


{- | Bloom-filter check for one PValue. Encodes the value into
its PLAIN-form byte payload (matching what the writer
inserts) and probes the filter.
-}
bloomCheckVal :: P.SchemaElement -> Bloom.Sbbf -> PValue -> Bool
bloomCheckVal field sbbf value = case value of
  PVFloat f | f == 0 -> check (PVFloat 0.0) || check (PVFloat (-0.0))
  PVDouble d | d == 0 -> check (PVDouble 0.0) || check (PVDouble (-0.0))
  _ -> check value
  where
    -- Unknown representations cannot prove absence. Variable-width decimal
    -- bytes may have redundant sign extension, so one hash is insufficient.
    check v = maybe True (\bytes -> Bloom.sbbfCheck bytes sbbf) (encodePlain field v)


encodePlain :: P.SchemaElement -> PValue -> Maybe ByteString
encodePlain field value
  | not (P.hasCompatiblePhysicalType field) = Nothing
  | Just P.LTDecimal {} <- P.seLogicalType field
  , notDecimal value =
      Nothing
  | otherwise = case (P.seType field, value) of
      (Just P.PTInt32, PVInt32 n) -> Just (word32LE (fromIntegral n))
      (Just P.PTInt64, PVInt64 n) -> Just (word64LE (fromIntegral n))
      (Just P.PTInt32, PVUnsigned n) | unsigned && n <= fromIntegral (maxBound :: Word32) -> Just (word32LE (fromIntegral n))
      (Just P.PTInt64, PVUnsigned n) | unsigned -> Just (word64LE n)
      (Just P.PTFloat, PVFloat f) -> Just (word32LE (castFloatToWord32 f))
      (Just P.PTDouble, PVDouble d) -> Just (word64LE (castDoubleToWord64 d))
      (Just P.PTBoolean, PVBool b) -> Just (BS.singleton (if b then 1 else 0))
      (Just P.PTByteArray, PVText t) -> Just (TE.encodeUtf8 t)
      (Just P.PTByteArray, PVBinary bytes) -> Just bytes
      (Just P.PTFixedLenByteArray, PVBinary bytes) -> Just bytes
      (_, PVDecimal decimal) -> case P.seLogicalType field of
        Just (P.LTDecimal _ scale)
          | scale >= 0 && scale <= 76 ->
              let scaled = decimal * fromInteger (10 ^ scale)
                  integer = numerator scaled
              in if denominator scaled /= 1
                   then Nothing
                   else case P.seType field of
                     Just P.PTInt32
                       | integer >= fromIntegral (minBound :: Int32) && integer <= fromIntegral (maxBound :: Int32) ->
                           Just (word32LE (fromIntegral integer))
                     Just P.PTInt64
                       | integer >= fromIntegral (minBound :: Int64) && integer <= fromIntegral (maxBound :: Int64) ->
                           Just (word64LE (fromIntegral integer))
                     _ -> Nothing
        _ -> Nothing
      _ -> Nothing
  where
    notDecimal PVDecimal {} = False
    notDecimal _ = True
    unsigned = case P.seLogicalType field of
      Just (P.LTInteger _ signed) -> not signed
      _ -> P.seConvertedType field `elem` (Just <$> [P.CTUInt8, P.CTUInt16, P.CTUInt32, P.CTUInt64])


word32LE :: Word32 -> ByteString
word32LE w =
  BS.pack
    [ fromIntegral w
    , fromIntegral (w `quot` 0x100)
    , fromIntegral (w `quot` 0x10000)
    , fromIntegral (w `quot` 0x1000000)
    ]


word64LE :: Word64 -> ByteString
word64LE w =
  BS.pack
    [ fromIntegral w
    , fromIntegral (w `quot` 0x100)
    , fromIntegral (w `quot` 0x10000)
    , fromIntegral (w `quot` 0x1000000)
    , fromIntegral (w `quot` 0x100000000)
    , fromIntegral (w `quot` 0x10000000000)
    , fromIntegral (w `quot` 0x1000000000000)
    , fromIntegral (w `quot` 0x100000000000000)
    ]
