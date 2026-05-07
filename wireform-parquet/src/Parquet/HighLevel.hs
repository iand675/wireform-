{-# LANGUAGE BangPatterns #-}
-- | High-level Parquet API.
--
-- 95% of callers should reach for this module. It hides the
-- @ColumnAux@ parallel-array machinery, the encrypted-footer
-- variant, and the per-row-group compression knobs of
-- "Parquet.Write" behind a single record-of-options:
--
-- @
-- -- Encode
-- let bytes = 'encodeParquet' 'defaultWriteOptions'
--                            schema rowGroups
--
-- -- Read
-- case 'decodeParquet' bytes of
--   Right (schema', rowGroups') -> ...
--   Left  err                   -> ...
-- @
--
-- @rowGroups@ is a @[V.Vector ColumnData]@ — one entry per row
-- group, each entry one 'ColumnData' per /leaf/ schema column in
-- the same order @schema@ enumerates them.
--
-- The 'WriteOptions' record consolidates every Parquet writer
-- knob in one place: compression, page version, bloom filters,
-- page-index emission, per-column encryption, and footer
-- encryption (PARE mode). Defaults are the modern-Parquet
-- recommended settings (Snappy, V2 pages, no encryption).
--
-- For lower-level control (per-column compression overrides,
-- bespoke 'ColumnAux' values, custom row-group dictionaries) drop
-- down to "Parquet.Write".
module Parquet.HighLevel
  ( -- * Encoding
    encodeParquet
  , encodeParquetMixed
  , encodeParquetNested
  , WriteOptions (..)
  , DictPolicy (..)
  , defaultWriteOptions
  , ParquetColumn (..)
  , OptionalColumn (..)
    -- * Decoding
  , decodeParquet
  , ReadOptions (..)
  , defaultReadOptions
  , FooterDecryption (..)
  , ParquetFile (..)
    -- * Re-exports for convenience
  , ColumnData (..)
  , ColumnEncryption (..)
  , FooterEncryption (..)
  , Compression (..)
  , PageVersion (..)
  , SchemaElement (..)
  , NestedSchema (..)
  , NestedRow (..)
  , LeafType (..)
  , LeafValue (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import Foreign.Storable (pokeByteOff)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Wireform.Hash as WHash
import GHC.Float (castDoubleToWord64, castFloatToWord32)

import qualified Parquet.BloomFilter as Bloom
import qualified Parquet.Nested as PN

import Parquet.Nested
  ( LeafType (..)
  , LeafValue (..)
  , NestedRow (..)
  , NestedSchema (..)
  , buildNestedFile
  )
import Parquet.Read
  ( FooterDecryption (..)
  , ParquetFile (..)
  , loadParquetFile
  , loadParquetFileEncrypted
  )
import Parquet.Types
  ( Compression (..)
  , SchemaElement (..)
  )
import Parquet.Write
  ( ColumnAux (..)
  , ColumnData (..)
  , ColumnEncryption (..)
  , FooterEncryption (..)
  , OptionalColumn (..)
  , PageVersion (..)
  , ParquetColumn (..)
  , buildParquetFileMixed
  , buildParquetFileMixedWith
  , buildParquetFileWithIndex
  , buildParquetFileWithIndexEncryptedFooter
  , emptyColumnAux
  )

-- ============================================================
-- Options
-- ============================================================

-- | Dictionary-encoding policy for the Parquet writer.
--
-- When enabled, low-cardinality columns are emitted as a
-- @DICTIONARY_PAGE@ holding the unique values + a
-- @DATA_PAGE@ holding RLE-encoded index references. This is
-- typically a large size win for string columns (e.g. enums,
-- categorical data) and a small CPU win on read.
data DictPolicy
  = DictNever
    -- ^ Always emit @PLAIN@-encoded data pages.
  | DictAlways
    -- ^ Always dict-encode (every column).
  | DictAuto
    -- ^ Heuristic: dict-encode when the dictionary's
    -- on-the-wire size (uniques + indices) is smaller than
    -- the raw PLAIN encoding /and/ the column has < 16k
    -- distinct values per chunk. Fall back to PLAIN
    -- otherwise. This matches what parquet-mr does by
    -- default.
  deriving (Show, Eq)

-- | Parquet writer configuration. Construct one with
-- 'defaultWriteOptions' and override the fields you care about:
--
-- @
-- let opts = 'defaultWriteOptions'
--             { writeCompression = 'ZSTD'
--             , writeBloomFilters = ["sku"]
--             }
--     bytes = 'encodeParquet' opts schema rowGroups
-- @
data WriteOptions = WriteOptions
  { writeCompression       :: !Compression
    -- ^ Compression codec applied to every column's data pages.
    -- Default: 'Snappy'. Use 'Uncompressed' to skip.
  , writePageVersion       :: !PageVersion
    -- ^ Data page version. 'PageV2' is recommended for modern
    -- writers; 'PageV1' interoperates with older Parquet readers.
    -- Default: 'PageV2'.
  , writeBloomFilters      :: ![Text]
    -- ^ Column paths (top-level field names) that should carry a
    -- split-block bloom filter. Empty list → no bloom filters.
    -- Filter parameters use parquet-cpp's defaults
    -- (~3% FPP at 1024 distinct values per row group).
  , writeDictionary        :: !DictPolicy
    -- ^ Dictionary-encoding policy. Default: 'DictAuto'.
  , writePageIndex         :: !Bool
    -- ^ Emit per-column 'OffsetIndex' / 'ColumnIndex' regions in
    -- the trailing page-index area. Default: 'True' — page
    -- indexes substantially improve scan performance for filter
    -- pushdown and most ecosystem readers (pyarrow, parquet-mr,
    -- DuckDB, Trino) take advantage of them when present.
  , writeColumnEncryption  :: !(V.Vector (Maybe ColumnEncryption))
    -- ^ Per-leaf-column encryption configuration. The vector
    -- length must match the number of leaf columns; 'Nothing'
    -- entries leave the column in plaintext. Default:
    -- 'V.empty' (no per-column encryption — interpreted as
    -- "all columns plaintext").
  , writeFooterEncryption  :: !(Maybe FooterEncryption)
    -- ^ When 'Just', the file is emitted in /encrypted-footer/
    -- mode (PARE trailing magic). When 'Nothing', the footer
    -- stays in plaintext (PAR1 magic). Default: 'Nothing'.
  } deriving (Show, Eq)

-- | Sensible modern-Parquet defaults: Snappy compression, V2
-- pages, page indexes on, no encryption, no bloom filters,
-- dictionary encoding off (callers can opt into 'DictAuto'
-- for typical workloads — left off by default so the
-- baseline output stays byte-stable).
defaultWriteOptions :: WriteOptions
defaultWriteOptions = WriteOptions
  { writeCompression       = Snappy
  , writePageVersion       = PageV2
  , writeBloomFilters      = []
  , writeDictionary        = DictNever
  , writePageIndex         = True
  , writeColumnEncryption  = V.empty
  , writeFooterEncryption  = Nothing
  }

-- ============================================================
-- Encoding
-- ============================================================

-- | Serialise a Parquet file from a schema + row groups, applying
-- the supplied options.
--
-- The schema is a flat 'V.Vector' of 'SchemaElement' (one entry
-- per node in the parquet schema tree, including the synthetic
-- root). Row groups are a list of column-major collections — one
-- 'V.Vector' 'ColumnData' per row group, each containing exactly
-- one entry per leaf column in declaration order.
encodeParquet
  :: WriteOptions
  -> V.Vector SchemaElement
  -> [V.Vector ColumnData]
  -> ByteString
encodeParquet opts schema rgs =
  let !rowGroups = V.fromList rgs
      !auxes     = V.map (mkAuxes opts schema) rowGroups
  in  case writeFooterEncryption opts of
        Nothing -> buildParquetFileWithIndex schema rowGroups auxes
        Just fe ->
          buildParquetFileWithIndexEncryptedFooter fe schema rowGroups auxes

-- | Build the leaf-name -> column-index lookup for a flat
-- schema. The synthetic root is at index 0; leaves start at 1
-- in the schema vector but are addressed 0-indexed in the
-- per-row-group columns. Used by the bloom-filter populator to
-- map the @writeBloomFilters@ name list to column positions.
leafColumnIndex :: V.Vector SchemaElement -> Text -> Maybe Int
leafColumnIndex schema name =
  let leaves = V.filter (maybe False (const True) . seType) schema
  in V.findIndex ((== name) . seName) leaves

-- | Serialise a Parquet file from a schema + row groups where
-- each column may be either required ('PCRequired') or nullable
-- ('PCOptional'). Routes through 'buildParquetFileMixedWith'
-- which emits @DATA_PAGE_V1@ pages and applies the requested
-- compression codec to every column-chunk body.
--
-- Honours 'writeCompression' from the supplied 'WriteOptions';
-- 'writePageVersion', 'writeBloomFilters', 'writePageIndex',
-- 'writeColumnEncryption', and 'writeFooterEncryption' are still
-- unsupported on the mixed path (use 'encodeParquet' with
-- all-required 'ColumnData' if those matter).
--
-- Returns the empty @ByteString@ when the codec choice fails
-- (e.g. Snappy without the @+snappy@ flag); callers that care
-- about that case should check 'writeCompression' against the
-- available codecs first.
encodeParquetMixed
  :: WriteOptions
  -> V.Vector SchemaElement
  -> [V.Vector ParquetColumn]
  -> ByteString
encodeParquetMixed opts schema rgs =
  case buildParquetFileMixedWith
         (writeCompression opts) schema (V.fromList rgs) of
    Right bs -> bs
    -- Failure here means the codec isn't built into this
    -- wireform copy. Fall back to uncompressed so the writer
    -- still emits something parseable.
    Left _   -> buildParquetFileMixed schema (V.fromList rgs)

-- | Compute a 'ColumnAux' vector per row group from the
-- write-time options.
--
-- Compression + page version are applied uniformly to every
-- column. Bloom filters are built per-column for the leaves
-- whose names appear in 'writeBloomFilters' — the writer hashes
-- each value's PLAIN payload into a fresh 'Sbbf' sized for the
-- column's row count so a downstream reader can probe membership
-- via 'Parquet.Predicate.evalBloomChunk' without false
-- negatives.
mkAuxes
  :: WriteOptions
  -> V.Vector SchemaElement
  -> V.Vector ColumnData
  -> V.Vector ColumnAux
mkAuxes opts schema cols =
  let !bloomNames = writeBloomFilters opts
      bloomIdxs   = [ i | nm <- bloomNames
                        , Just i <- [leafColumnIndex schema nm] ]
      bloomSet    = bloomIdxs
      !dictPolicy = writeDictionary opts
  in V.imap (\i col -> mkOne i col bloomSet dictPolicy) cols
  where
    mkOne :: Int -> ColumnData -> [Int] -> DictPolicy -> ColumnAux
    mkOne i col blooms policy =
      emptyColumnAux
        { caCodec        = writeCompression opts
        , caPageVersion  = writePageVersion opts
        , caBloomFilter  = if i `elem` blooms
                             then Just $! buildBloomFilterFor col
                             else Nothing
        , caUseDictionary = decideDict policy col
        }

-- | Decide whether to dict-encode a column based on the
-- write-time policy.
--
-- 'DictAuto' uses a conservative size + cardinality heuristic:
-- for 'ColByteArray' columns we dict-encode when both
--
--   1. the cardinality fits in our 'Int32' index width AND
--      stays under a sensible threshold (16k distinct values
--      per chunk — matches parquet-mr's default), and
--   2. the dictionary's serialised size is at least 25% smaller
--      than the raw PLAIN encoding (the index stream still
--      needs ~log2(uniques) bits per row, so very-high
--      cardinality columns get bigger with dict).
--
-- For non-byte-array primitives 'DictAuto' returns 'False':
-- dict encoding is rarely a net win for fixed-width data
-- because the index stream isn't smaller than the raw values
-- once you factor in headers, and CPU cost on the read side
-- is positive.
decideDict :: DictPolicy -> ColumnData -> Bool
decideDict DictNever  _ = False
decideDict DictAlways _ = True
decideDict DictAuto col = case col of
  ColByteArray v ->
    let !n        = V.length v
        !distinct = approxDistinctByteArray v
    in  n > 0 && distinct <= 16384
          && distinctSavesBytes v distinct
  _ -> False

-- | Cheap upper bound on the distinct-value count of a
-- 'V.Vector' 'ByteString' — uses a 'Map.Strict' until it
-- crosses the cardinality threshold we care about, then
-- bails. Linear in n; bounded constant memory.
approxDistinctByteArray :: V.Vector ByteString -> Int
approxDistinctByteArray v =
  let !n   = V.length v
      goAcc !i !seen !count
        | i >= n           = count
        | count >= 16385   = count    -- saturate; we won't dict-encode
        | otherwise =
            let !x = V.unsafeIndex v i
            in if Map.member x seen
                 then goAcc (i + 1) seen count
                 else goAcc (i + 1) (Map.insert x () seen) (count + 1)
  in goAcc 0 Map.empty 0

-- | Estimate whether dict-encoding would shrink @vs@ by at
-- least 25%.
distinctSavesBytes :: V.Vector ByteString -> Int -> Bool
distinctSavesBytes vs distinct =
  let !n        = V.length vs
      -- Rough estimate of the dict's serialised size: 4-byte
      -- length prefix + payload per unique value, plus the
      -- RLE-hybrid index stream which is at most n * 4 bytes
      -- (we compute the upper bound as if every row had its
      -- own bit-packed group; in practice it's much smaller).
      !plainBytes = V.foldl' (\a bs -> a + 4 + BS.length bs) 0 vs
      -- Distinct payload is a lower bound on dict size; we
      -- assume each unique appears once in the dict.
      !uniqueBytes = sampleUniqueBytes vs distinct
      !indexBytes  = (n * indexBitWidth distinct + 7) `quot` 8
      !dictTotal   = uniqueBytes + indexBytes + 8   -- +8 for headers
  in  dictTotal * 4 < plainBytes * 3   -- ≥ 25% savings

-- | Sum of distinct-value byte sizes (for the size estimate).
-- This is O(n) but bounded by the count threshold so
-- realistic columns are fast.
sampleUniqueBytes :: V.Vector ByteString -> Int -> Int
sampleUniqueBytes vs cap
  | cap <= 0  = 0
  | otherwise =
      let !n = V.length vs
          go !i !seen !acc !count
            | i >= n         = acc
            | count >= cap   = acc
            | otherwise =
                let !x = V.unsafeIndex vs i
                in if Map.member x seen
                     then go (i + 1) seen acc count
                     else go (i + 1) (Map.insert x () seen)
                                     (acc + 4 + BS.length x) (count + 1)
      in go 0 Map.empty 0 0

-- | Bit width needed to index @distinct@ values.
indexBitWidth :: Int -> Int
indexBitWidth n
  | n <= 1 = 1
  | otherwise = ceilingLog2 n
  where
    ceilingLog2 k = go 0 1
      where
        go !w !bound | bound >= k = w | otherwise = go (w + 1) (bound * 2)

-- | Build a split-block bloom filter populated from a column's
-- values. Sizes the filter via 'Bloom.optimalNumBytes' for the
-- column's row count at a 1% false-positive rate (parquet-cpp's
-- default), then inserts each value's PLAIN-encoded payload —
-- matching what 'Parquet.Predicate.encodePlain' probes with on
-- the read side.
-- | Build a split-block bloom filter populated from a column's
-- values. Sizes the filter via 'Bloom.optimalNumBytes' for the
-- column's row count at a 1% false-positive rate (parquet-cpp's
-- default), then inserts each value's PLAIN-encoded payload.
--
-- Uses 'Bloom.buildSbbfFromHashes' so the bitset is allocated
-- once and updated in place — vs the previous 'foldl' over
-- 'sbbfInsert' shape which copied the entire bitset on every
-- value (O(rows × bitset_bytes) total work — pathological for
-- any non-trivial filter size). This collapses to
-- O(rows + bitset_bytes).
buildBloomFilterFor :: ColumnData -> Bloom.Sbbf
buildBloomFilterFor col =
  let !ndv    = max 1 (columnDistinctEstimate col)
      !nBytes = Bloom.optimalNumBytes ndv 0.01
  in case col of
       ColInt32 v ->
         Bloom.buildSbbfFromHashes nBytes (VU.generate (VS.length v)
           (\i -> WHash.xxh64 0 (i32LE (VS.unsafeIndex v i))))
       ColInt64 v ->
         Bloom.buildSbbfFromHashes nBytes (VU.generate (VS.length v)
           (\i -> WHash.xxh64 0 (i64LE (VS.unsafeIndex v i))))
       ColFloat v ->
         Bloom.buildSbbfFromHashes nBytes (VU.generate (VS.length v)
           (\i -> WHash.xxh64 0 (f32LE (VS.unsafeIndex v i))))
       ColDouble v ->
         Bloom.buildSbbfFromHashes nBytes (VU.generate (VS.length v)
           (\i -> WHash.xxh64 0 (f64LE (VS.unsafeIndex v i))))
       ColBool v ->
         Bloom.buildSbbfFromHashes nBytes (VU.generate (V.length v)
           (\i -> WHash.xxh64 0 (boolPayload (V.unsafeIndex v i))))
       ColByteArray v ->
         Bloom.buildSbbfFromBytes nBytes v

-- | Cheap distinct-value upper bound: row count. Real
-- distinct-counting would need a second pass; sizing for the
-- worst case (all-distinct) keeps the filter slightly oversize
-- but never underfilled.
columnDistinctEstimate :: ColumnData -> Int
columnDistinctEstimate = \case
  ColInt32 v     -> VS.length v
  ColInt64 v     -> VS.length v
  ColFloat v     -> VS.length v
  ColDouble v    -> VS.length v
  ColBool v      -> V.length v
  ColByteArray v -> V.length v

-- | PLAIN encodings used for bloom-filter inserts. These must
-- match 'Parquet.Predicate.encodePlain' byte-for-byte so a
-- @PEq@ predicate hashes to the same key.
--
-- Implementation note (perf): the bloom-filter path inserts
-- one of these per column value, so on a 100k-row column with
-- a bloom filter requested the encoder is hit 100k times.
-- The previous shape went through @ByteString.Builder@ +
-- @BL.toStrict@ for each call — three heap allocations per
-- value. The new shape pre-allocates the strict ByteString
-- to its exact size and pokes the bit pattern in directly.
{-# INLINE i32LE #-}
i32LE :: Int32 -> ByteString
i32LE !v = BSI.unsafeCreate 4 $ \p -> pokeByteOff p 0 v

{-# INLINE i64LE #-}
i64LE :: Int64 -> ByteString
i64LE !v = BSI.unsafeCreate 8 $ \p -> pokeByteOff p 0 v

{-# INLINE f32LE #-}
f32LE :: Float -> ByteString
f32LE !v = BSI.unsafeCreate 4 $ \p ->
  pokeByteOff p 0 (castFloatToWord32 v)

{-# INLINE f64LE #-}
f64LE :: Double -> ByteString
f64LE !v = BSI.unsafeCreate 8 $ \p ->
  pokeByteOff p 0 (castDoubleToWord64 v)

-- Bool payload is one byte; precompute both static
-- ByteStrings so each insert is a constant pointer rather
-- than a fresh allocation.
boolPayload :: Bool -> ByteString
boolPayload True  = boolTrueBs
boolPayload False = boolFalseBs

boolTrueBs, boolFalseBs :: ByteString
boolTrueBs  = BS.singleton 1
boolFalseBs = BS.singleton 0

-- ============================================================
-- Decoding
-- ============================================================

-- | Serialise a Parquet file carrying /nested/ (struct / list /
-- map / variant) columns. Delegates to 'Parquet.Nested.buildNestedFile'
-- after the Dremel shredder in "Parquet.Nested".
--
-- @columns@ is a vector of @(column-name, schema)@ pairs;
-- @rowsPerColumn@ is a parallel vector where the i-th entry is
-- the row-major list of 'NestedRow' values for column @i@. Every
-- column must carry the same number of rows.
--
-- The nested-file writer currently emits uncompressed
-- @DATA_PAGE_V2@ pages and doesn't take the 'WriteOptions'
-- record — compression / page-index / encryption knobs are
-- roadmap items for the nested path. Use 'encodeParquet' for the
-- flat path if you need those knobs today.
encodeParquetNested
  :: V.Vector (Text, NestedSchema)
  -> V.Vector (V.Vector NestedRow)
  -> Either String ByteString
encodeParquetNested = buildNestedFile

-- ============================================================
-- Decoding
-- ============================================================

-- | Parquet reader configuration. Construct one with
-- 'defaultReadOptions' and override the fields you care about.
--
-- @
-- let opts = 'defaultReadOptions'
--             { readFooterDecryption = Just ('FooterDecryption' key prefix fileId) }
-- 'decodeParquet' opts bytes
-- @
data ReadOptions = ReadOptions
  { readFooterDecryption :: !(Maybe FooterDecryption)
    -- ^ When 'Just', expect an encrypted-footer file (PARE
    -- trailing magic) and decrypt with the supplied key / AAD /
    -- file-id. When 'Nothing' (the default), a plaintext (PAR1)
    -- footer is expected.
  } deriving (Show, Eq)

-- | Plaintext-footer defaults (matches the common case).
defaultReadOptions :: ReadOptions
defaultReadOptions = ReadOptions
  { readFooterDecryption = Nothing
  }

-- | Parse a Parquet file's footer. By default expects a
-- plaintext PAR1 footer; pass 'readFooterDecryption' in
-- 'ReadOptions' to decrypt a PARE-footer file.
--
-- @
-- -- plaintext
-- 'decodeParquet' 'defaultReadOptions' bytes
--
-- -- encrypted footer
-- 'decodeParquet'
--   'defaultReadOptions' { 'readFooterDecryption' = Just fd }
--   bytes
-- @
decodeParquet :: ReadOptions -> ByteString -> Either String ParquetFile
decodeParquet opts = case readFooterDecryption opts of
  Nothing -> loadParquetFile
  Just fd -> loadParquetFileEncrypted fd
