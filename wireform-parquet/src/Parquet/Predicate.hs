{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | Predicate pushdown helpers for Parquet readers.
--
-- The Parquet spec already gives the reader several places to
-- avoid touching column data:
--
--   * Row-group 'Statistics' carry @min_value@ / @max_value@ /
--     @null_count@ for every column chunk.
--   * Page-level 'ColumnIndex' carries the same triple per page,
--     plus a 'BoundaryOrder' hint and an optional null-pages
--     bitmap.
--   * Per-column 'BloomFilter' (Sbbf) lets the reader prove a
--     value is /not/ present in the column chunk.
--
-- This module bundles the three signals behind a single
-- 'Predicate' ADT so callers can express filters once and ask
-- the planner whether a row group / page can be skipped without
-- decoding it.
--
-- The vocabulary is deliberately narrow — equality, range, and
-- membership over the same scalar value space (@PValue@) the
-- writer's statistics use. Compound boolean structure
-- (@'PAnd'@, @'POr'@, @'PNot'@) composes up from there.
--
-- @
-- import qualified Parquet.Predicate as Pred
--
-- let !p = 'PAnd' ('PCol' "ts" ('PGtEq' ('PVInt64' 1700000000)))
--                ('PCol' "ts" ('PLt'   ('PVInt64' 1700100000)))
-- skipRowGroup :: 'P.RowGroup' -> Bool
-- skipRowGroup rg = 'evalRowGroup' columnNames p rg == 'PSkip'
-- @
--
-- The evaluator never produces a /false negative/ skip
-- (returning 'PSkip' when in fact some rows match) — if there's
-- any uncertainty (missing statistics, types we don't compare)
-- the answer is 'PMaybeKeep' and the caller falls back to
-- decoding the column chunk normally.
module Parquet.Predicate
  ( -- * Predicate vocabulary
    --
    -- | Re-exported from "Columnar.Predicate" so the same
    -- @PValue@ / @PColPredicate@ / @Predicate@ surface drives
    -- skip decisions in both Parquet and ORC.
    Predicate (..)
  , PColPredicate (..)
  , PValue (..)
    -- * Evaluation results
  , Decision (..)
  , combineDecisions
    -- * Statistics-based skipping
  , evalRowGroup
  , evalColumnChunk
    -- * Page-index-based skipping
  , evalPagesByColumnIndex
    -- * Bloom-filter membership
  , evalBloomChunk
    -- * Conversions
  , decodePValueLE
  ) where

import Columnar.Predicate
  ( Decision (..)
  , PColPredicate (..)
  , PValue (..)
  , Predicate (..)
  , combineDecisions
  , evalRange
  , pvLess
  , pvLessEq
  , pvEq
  )

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.Storable (pokeByteOff)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import GHC.Float (castFloatToWord32, castDoubleToWord64, castWord32ToFloat, castWord64ToDouble)

import qualified Parquet.BloomFilter as Bloom
import qualified Parquet.Types as P

-- ============================================================
-- Statistics-based skipping
-- ============================================================

-- | Decide whether an entire row group can be skipped.
--
-- Walks the predicate tree, looking up each leaf's column by
-- name in the column-name vector (parallel to the row group's
-- 'rgColumns' vector — typically the leaf names from
-- 'fmSchema'). Missing columns / missing stats degrade
-- gracefully to 'PMaybeKeep'.
evalRowGroup
  :: V.Vector Text
  -- ^ Column names parallel to @rgColumns@. Use
  -- @'Parquet.Footer.leafColumnNames'@ or build from
  -- 'P.fmSchema'.
  -> Predicate
  -> P.RowGroup
  -> Decision
evalRowGroup colNames p rg = evalPredicate (lookupChunkStats colNames rg) p

-- | Decide whether one column chunk can be skipped, given the
-- predicate fragment that applies to its column. Useful when
-- the caller has already projected the predicate per column.
evalColumnChunk
  :: P.ColumnChunk
  -> PColPredicate
  -> Decision
evalColumnChunk cc cp = case P.ccMetadata cc of
  Nothing -> PMaybeKeep
  Just md -> case P.cmStatistics md of
    Nothing    -> PMaybeKeep
    Just stats -> evalLeaf (P.cmType md) stats cp

evalPredicate
  :: (Text -> Maybe (P.ParquetType, P.Statistics))
  -> Predicate
  -> Decision
evalPredicate _ PTrue  = PMaybeKeep
evalPredicate _ PFalse = PSkip
evalPredicate look (PCol name cp) = case look name of
  Nothing             -> PMaybeKeep
  Just (ty, stats)    -> evalLeaf ty stats cp
evalPredicate look (PAnd a b) =
  combineDecisions (evalPredicate look a) (evalPredicate look b)
evalPredicate look (POr a b) =
  -- For OR we can only skip if /both/ disjuncts skip. The
  -- conservative answer otherwise is 'PMaybeKeep'.
  case (evalPredicate look a, evalPredicate look b) of
    (PSkip, PSkip) -> PSkip
    _              -> PMaybeKeep
evalPredicate look (PNot inner) = case evalPredicate look inner of
  -- Negation can't be pushed in general (a 'PSkip' on the
  -- inner predicate doesn't imply skipping for the negation).
  -- The conservative answer is always 'PMaybeKeep'.
  _ -> case inner of
    PFalse -> PMaybeKeep
    _      -> PMaybeKeep

-- | Look up the @(type, statistics)@ pair for one column-chunk
-- slot of a row group. Index by the caller-supplied name
-- vector so one row group's columns stay aligned with the file
-- footer's leaf order.
lookupChunkStats
  :: V.Vector Text
  -> P.RowGroup
  -> Text
  -> Maybe (P.ParquetType, P.Statistics)
lookupChunkStats colNames rg name = do
  i <- V.findIndex (== name) colNames
  cc <- safeIndex i (P.rgColumns rg)
  md <- P.ccMetadata cc
  st <- P.cmStatistics md
  Just (P.cmType md, st)

safeIndex :: Int -> V.Vector a -> Maybe a
safeIndex i v
  | i >= 0 && i < V.length v = Just (V.unsafeIndex v i)
  | otherwise                = Nothing

-- ============================================================
-- Leaf predicates
-- ============================================================

-- | Evaluate one column-leaf predicate against the column
-- chunk's 'Statistics'. The 'ParquetType' tells us how to
-- decode the bytes inside @statMinValue@ / @statMaxValue@.
evalLeaf :: P.ParquetType -> P.Statistics -> PColPredicate -> Decision
evalLeaf _ stats PIsNull
  -- IS NULL is skippable iff null_count is known to be zero.
  | P.statNullCount stats == Just 0 = PSkip
  | otherwise                       = PMaybeKeep
evalLeaf _ stats PIsNotNull
  -- IS NOT NULL is skippable iff every row is null. Without a
  -- per-chunk row count we can't be sure; fall back to maybe.
  | otherwise = case P.statNullCount stats of
      Just _ -> PMaybeKeep   -- conservative
      Nothing -> PMaybeKeep
evalLeaf ty stats cp =
  case (statMinPV ty stats, statMaxPV ty stats) of
    (Just mn, Just mx) -> evalRange mn mx cp
    _                  -> PMaybeKeep

-- ============================================================
-- Min / max decoding
-- ============================================================

-- | Pick the modern @min_value@ if present (preferred), else
-- the legacy @min@ slot (older writers).
statMinPV :: P.ParquetType -> P.Statistics -> Maybe PValue
statMinPV ty s = preferring (P.statMinValue s) (P.statMin s)
                 >>= decodePValueLE ty

statMaxPV :: P.ParquetType -> P.Statistics -> Maybe PValue
statMaxPV ty s = preferring (P.statMaxValue s) (P.statMax s)
                 >>= decodePValueLE ty

preferring :: Maybe a -> Maybe a -> Maybe a
preferring (Just x) _ = Just x
preferring Nothing  m = m

-- | Decode a min/max byte payload according to its
-- 'P.ParquetType'. Mirrors what the writer's
-- @statisticsFor*@ helpers in "Parquet.Write" produced.
decodePValueLE :: P.ParquetType -> ByteString -> Maybe PValue
decodePValueLE ty bs = case ty of
  P.PTBoolean ->
    case BS.uncons bs of
      Just (b, _) -> Just (PVBool (b /= 0))
      Nothing     -> Nothing
  P.PTInt32 | BS.length bs >= 4 ->
    Just (PVInt32 (fromIntegral (readLE32 bs 0)))
  P.PTInt64 | BS.length bs >= 8 ->
    Just (PVInt64 (fromIntegral (readLE64 bs 0)))
  P.PTFloat | BS.length bs >= 4 ->
    Just (PVFloat  (castWord32ToFloat (readLE32 bs 0)))
  P.PTDouble | BS.length bs >= 8 ->
    Just (PVDouble (castWord64ToDouble (readLE64 bs 0)))
  P.PTByteArray ->
    -- Stats store the bytes without the PLAIN length prefix
    -- (per spec). Treat them as opaque binary; consumers can
    -- coerce to text if they know the column is UTF-8.
    Just (PVBinary bs)
  P.PTFixedLenByteArray ->
    Just (PVBinary bs)
  _ -> Nothing

readLE32 :: ByteString -> Int -> Word32
readLE32 bs o =
  let b0 = fromIntegral (BS.index bs o)        :: Word32
      b1 = fromIntegral (BS.index bs (o + 1))  :: Word32
      b2 = fromIntegral (BS.index bs (o + 2))  :: Word32
      b3 = fromIntegral (BS.index bs (o + 3))  :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

readLE64 :: ByteString -> Int -> Word64
readLE64 bs o =
  let lo = fromIntegral (readLE32 bs o)        :: Word64
      hi = fromIntegral (readLE32 bs (o + 4))  :: Word64
  in lo .|. (hi `shiftL` 32)

-- ============================================================
-- Page-index-based skipping
-- ============================================================

-- | Per-page skip decisions for one column chunk, derived from
-- the chunk's 'P.ColumnIndex'. The result vector is parallel
-- to @ciNullPages@ / @ciMinValues@ / @ciMaxValues@: an entry
-- of 'PSkip' means the corresponding 'P.PageLocation' (in the
-- chunk's matching 'P.OffsetIndex') can be skipped without
-- decoding.
--
-- Pages whose @null_pages@ slot is 'True' (the page contains
-- only nulls) are skipped for /any/ value-style predicate
-- ('PEq', 'PLt', …); 'PIsNull' keeps them.
evalPagesByColumnIndex
  :: P.ParquetType
  -> P.ColumnIndex
  -> PColPredicate
  -> V.Vector Decision
evalPagesByColumnIndex ty ci cp =
  let !nulls = P.ciNullPages ci
      !mins  = P.ciMinValues ci
      !maxs  = P.ciMaxValues ci
      !n     = V.length nulls
  in V.generate n $ \i ->
       let !isNull = V.unsafeIndex nulls i
       in if isNull
            then case cp of
              PIsNull    -> PMaybeKeep
              PIsNotNull -> PSkip
              _          -> PSkip
            else
              case ( decodePValueLE ty (V.unsafeIndex mins i)
                   , decodePValueLE ty (V.unsafeIndex maxs i)
                   ) of
                (Just mn, Just mx) -> evalRange mn mx cp
                _                  -> PMaybeKeep

-- ============================================================
-- Bloom-filter membership
-- ============================================================

-- | Probe a column chunk's bloom filter for membership of the
-- value(s) referenced by the predicate. Returns 'PSkip' when
-- the bloom proves the column /cannot/ contain any matching
-- value; 'PMaybeKeep' otherwise.
--
-- Only @PEq@ and @PIn@ are bloom-checkable; other predicates
-- always degrade to 'PMaybeKeep' here (range / nullity have
-- to use min/max stats instead).
evalBloomChunk
  :: P.ParquetType
  -> Bloom.Sbbf
  -> PColPredicate
  -> Decision
evalBloomChunk ty sbbf = \case
  PEq v -> case bloomCheckVal ty sbbf v of
    True  -> PMaybeKeep
    False -> PSkip
  PIn vs ->
    -- Skip iff every candidate is rejected by the filter.
    if all (not . bloomCheckVal ty sbbf) vs
      then PSkip
      else PMaybeKeep
  _      -> PMaybeKeep

-- | Bloom-filter check for one PValue. Encodes the value into
-- its PLAIN-form byte payload (matching what the writer
-- inserts) and probes the filter.
bloomCheckVal :: P.ParquetType -> Bloom.Sbbf -> PValue -> Bool
bloomCheckVal _ sbbf v =
  let !payload = encodePlain v
  in Bloom.sbbfCheck payload sbbf

-- | Encode a value as its PLAIN payload — matches what
-- 'Parquet.Write' inserts into the bloom filter at write time.
--
-- Uses direct 'BSI.unsafeCreate' + 'pokeByteOff' rather than
-- @BS.pack [bytes]@ so each call avoids the cons-list
-- intermediate. (Predicate evaluation isn't per-row in our
-- read path, but this gets called from the writer's
-- statistics + bloom path on every value.)
encodePlain :: PValue -> ByteString
encodePlain = \case
  PVInt32 n  -> word32LE (fromIntegral n)
  PVInt64 n  -> word64LE (fromIntegral n)
  PVFloat f  -> word32LE (castFloatToWord32 f)
  PVDouble d -> word64LE (castDoubleToWord64 d)
  PVBool  b  -> BS.singleton (if b then 1 else 0)
  PVText  t  -> TE.encodeUtf8 t
  PVBinary b -> b

{-# INLINE word32LE #-}
word32LE :: Word32 -> ByteString
word32LE !w = BSI.unsafeCreate 4 $ \p -> pokeByteOff p 0 w

{-# INLINE word64LE #-}
word64LE :: Word64 -> ByteString
word64LE !w = BSI.unsafeCreate 8 $ \p -> pokeByteOff p 0 w
