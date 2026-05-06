{-# LANGUAGE BangPatterns #-}
-- | Parquet definition / repetition levels for data page v1.
--
-- Data page v1 layout (uncompressed body): repetition levels (if @max repetition > 0@),
-- then definition levels (if @max definition > 0@), then encoded values (@PLAIN@, etc.).
-- Each level column is length-prefixed hybrid RLE (see @Parquet.RLE@).
--
-- Use 'maxLevelsForColumnPath' with footer schema + 'ColumnMetadata' path to obtain
-- @maxRep@ / @maxDef@ for a leaf column.
module Parquet.Levels
  ( levelBitWidth
  , maxLevelsForColumnPath
  , parseDataPageV1Levels
  , materializePlainInt32Optional
  , materializePlainInt64Optional
  , materializePlainFloatOptional
  , materializePlainDoubleOptional
  , materializePlainBoolOptional
  , materializePlainByteArrayOptional
  , materializeRepeatedInt32
  , materializeRepeatedInt64
  , materializeRepeatedFloat
  , materializeRepeatedDouble
  , materializeRepeatedByteArray
  , materializeRepeatedByNested
  , NestedValue (..)
  ) where

import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import Data.Word (Word32, Word64)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafePerformIO)

import Parquet.RLE (decodeHybridRleUnsigned32)
import Parquet.Types (Repetition (..), SchemaElement (..))

-- | Bits required to store any level in @[0 .. maxLevel]@. Returns @0@ when
-- @maxLevel == 0@ (no level data on disk for that column).
levelBitWidth :: Int -> Int
levelBitWidth maxLevel
  | maxLevel <= 0 = 0
  | otherwise = go 1 0
  where
    go !bound !w
      | bound > maxLevel = w
      | otherwise = go (bound * 2) (w + 1)

-- | LE 'Word32' read via a single unsafe 'peekByteOff'. Replaces
-- the byte-by-byte BS.index + shifts shape used here previously.
{-# INLINE readLE32 #-}
readLE32 :: ByteString -> Int -> Word32
readLE32 bs o = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(cstr, _) ->
    peekByteOff (cstr `plusPtr` o) 0

{-# INLINE readLE64 #-}
readLE64 :: ByteString -> Int -> Word64
readLE64 bs o = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(cstr, _) ->
    peekByteOff (cstr `plusPtr` o) 0

{-# INLINE readBitLsb #-}
readBitLsb :: ByteString -> Int -> Bool
readBitLsb bs bitIdx =
  let bi = bitIdx `quot` 8
      ii = bitIdx `rem` 8
      b = BSU.unsafeIndex bs bi
  in (b `shiftR` ii) .&. 1 /= 0

-- | Generic engine for the @materializePlain*Optional@
-- family. Walks the @defs@ vector once and writes
-- @Just (reader off)@ for present entries and @Nothing@ for
-- nulls into a freshly-allocated boxed mutable vector. No
-- list intermediates, no @reverse@, no @V.fromList@.
--
-- The previous shape was
--
--     case go [] 0 0 of                    -- builds a [Maybe a] in reverse
--       Right xs -> Right $! V.fromList (reverse xs)
--
-- which allocated 3·n cells (cons cells in @go@'s acc, cons
-- cells produced by 'reverse', and the boxed vector's spine).
-- The new shape allocates one mutable vector + one freeze.
{-# INLINE materializePlainOptionalGeneric #-}
materializePlainOptionalGeneric
  :: forall a.
     String                                     -- ^ type name (used in error msgs)
  -> Int                                        -- ^ stride (bytes per present value)
  -> (ByteString -> Int -> a)                   -- ^ value reader
  -> VP.Vector Int32 -> Int -> ByteString
  -> Either String (V.Vector (Maybe a))
materializePlainOptionalGeneric ty stride reader defs maxDef plain
  | maxDef < 0 = Left "Parquet.Levels: negative max definition level"
  | otherwise = runST $ do
      let !n     = VP.length defs
          !maxD  = fromIntegral maxDef :: Int32
          !bsLen = BS.length plain
      mv <- VM.unsafeNew n
      let go !i !off
            | i >= n =
                if off <= bsLen
                  then pure (Right ())
                  else pure (Left ("Parquet.Levels: PLAIN " ++ ty ++ " buffer overrun"))
            | otherwise = do
                let !d = VP.unsafeIndex defs i
                if d > maxD || d < 0
                  then pure (Left ("Parquet.Levels: definition level out of range: " ++ show d))
                  else if d == maxD
                    then if off + stride > bsLen
                      then pure (Left ("Parquet.Levels: PLAIN " ++ ty ++ " buffer too small"))
                      else do
                        VM.unsafeWrite mv i (Just (reader plain off))
                        go (i + 1) (off + stride)
                    else do
                      VM.unsafeWrite mv i Nothing
                      go (i + 1) off
      r <- go 0 0
      case r of
        Left e   -> pure (Left e)
        Right () -> Right <$> V.unsafeFreeze mv

-- | Decode one length-prefixed hybrid level stream starting at @off@.
-- Returns decoded levels and the offset immediately after this stream.
decodeLengthPrefixedHybrid ::
  Int ->
  Int ->
  ByteString ->
  Int ->
  Either String (VP.Vector Int32, Int)
decodeLengthPrefixedHybrid bw n bs off
  | bw == 0 =
      if n < 0
        then Left "Parquet.Levels: negative value count for level stream"
        else Right (VP.replicate n 0, off)
  | off + 4 > BS.length bs =
      Left "Parquet.Levels: truncated level length prefix"
  | otherwise =
      let !len = fromIntegral (readLE32 bs off) :: Int
          !rest = BS.drop (off + 4) bs
       in if len < 0 || len > BS.length rest
            then Left "Parquet.Levels: invalid level slice length"
            else case decodeHybridRleUnsigned32 bw n (BS.take len rest) of
              Left e -> Left e
              Right v -> Right (v, off + 4 + len)

-- | Split an uncompressed data page v1 body into repetition levels, definition
-- levels, and the remaining payload (e.g. @PLAIN@ values).
--
-- When @maxRep@ or @maxDef@ is @0@, the corresponding vector is all zeros and no
-- bytes are consumed for that stream (Parquet omits zero-bit-width level data).
parseDataPageV1Levels ::
  Int ->
  Int ->
  Int ->
  ByteString ->
  Either String (VP.Vector Int32, VP.Vector Int32, ByteString)
parseDataPageV1Levels maxRep maxDef numValues raw = do
  let !bwRep = levelBitWidth maxRep
      !bwDef = levelBitWidth maxDef
  (rep, off1) <- decodeLengthPrefixedHybrid bwRep numValues raw 0
  (def, off2) <- decodeLengthPrefixedHybrid bwDef numValues raw off1
  let !rest = BS.drop off2 raw
  pure (rep, def, rest)

-- | Map @PLAIN@ packed @INT32@ values using Parquet definition levels: rows with
-- @def == maxDef@ consume the next four bytes; nulls use @def \< maxDef@.
materializePlainInt32Optional
  :: VP.Vector Int32 -> Int -> ByteString
  -> Either String (V.Vector (Maybe Int32))
materializePlainInt32Optional =
  materializePlainOptionalGeneric "INT32" 4
    (\bs o -> fromIntegral (readLE32 bs o) :: Int32)

-- | @PLAIN@ @INT64@ (8-byte LE per defined value).
materializePlainInt64Optional
  :: VP.Vector Int32 -> Int -> ByteString
  -> Either String (V.Vector (Maybe Int64))
materializePlainInt64Optional =
  materializePlainOptionalGeneric "INT64" 8
    (\bs o -> fromIntegral (readLE64 bs o) :: Int64)

-- | @PLAIN@ @FLOAT@ (4-byte IEEE LE per defined value).
materializePlainFloatOptional
  :: VP.Vector Int32 -> Int -> ByteString
  -> Either String (V.Vector (Maybe Float))
materializePlainFloatOptional =
  materializePlainOptionalGeneric "FLOAT" 4
    (\bs o -> castWord32ToFloat (readLE32 bs o))

-- | @PLAIN@ @DOUBLE@ (8-byte IEEE LE per defined value).
materializePlainDoubleOptional
  :: VP.Vector Int32 -> Int -> ByteString
  -> Either String (V.Vector (Maybe Double))
materializePlainDoubleOptional =
  materializePlainOptionalGeneric "DOUBLE" 8
    (\bs o -> castWord64ToDouble (readLE64 bs o))

-- | @PLAIN@ @BOOLEAN@ — packed bits in definition order; only defined values
-- occupy bits (LSB of first byte is first defined value).
materializePlainBoolOptional ::
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (Maybe Bool))
materializePlainBoolOptional defs maxDef plain
  | maxDef < 0 = Left "Parquet.Levels: negative max definition level"
  | otherwise =
      let !n      = VP.length defs
          !maxDi  = fromIntegral maxDef :: Int32
          !needBits = VP.foldl' (\a v -> if v == maxDi then a + 1 else a) 0 defs
          !needBytes = (needBits + 7) `quot` 8
      in if BS.length plain < needBytes
           then Left $ "Parquet.Levels: PLAIN BOOLEAN buffer too small for "
                         ++ show needBits ++ " defined bits"
           else runST $ do
             mv <- VM.unsafeNew n
             let go !i !bitPos
                   | i >= n = pure (Right ())
                   | otherwise = do
                       let !d = VP.unsafeIndex defs i
                       if d > maxDi || d < 0
                         then pure (Left ("Parquet.Levels: definition level out of range: " ++ show d))
                         else if d == maxDi
                           then do
                             VM.unsafeWrite mv i (Just (readBitLsb plain bitPos))
                             go (i + 1) (bitPos + 1)
                           else do
                             VM.unsafeWrite mv i Nothing
                             go (i + 1) bitPos
             r <- go 0 0
             case r of
               Left e   -> pure (Left e)
               Right () -> Right <$> V.unsafeFreeze mv

-- | @PLAIN@ @BYTE_ARRAY@ — per defined value: 4-byte LE length + payload.
materializePlainByteArrayOptional ::
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (Maybe ByteString))
materializePlainByteArrayOptional defs maxDef plain
  | maxDef < 0 = Left "Parquet.Levels: negative max definition level"
  | otherwise = runST $ do
      let !n     = VP.length defs
          !maxDi = fromIntegral maxDef :: Int32
          !bsLen = BS.length plain
      mv <- VM.unsafeNew n
      let go !i !off
            | i >= n = pure (Right ())
            | otherwise = do
                let !d = VP.unsafeIndex defs i
                if d > maxDi || d < 0
                  then pure (Left ("Parquet.Levels: definition level out of range: " ++ show d))
                  else if d == maxDi
                    then if off + 4 > bsLen
                      then pure (Left "Parquet.Levels: PLAIN BYTE_ARRAY truncated length")
                      else do
                        let !len  = fromIntegral (readLE32 plain off) :: Int
                            !off2 = off + 4
                        if len < 0 || off2 + len > bsLen
                          then pure (Left "Parquet.Levels: PLAIN BYTE_ARRAY payload out of bounds")
                          else do
                            let !payload = BS.take len (BS.drop off2 plain)
                            VM.unsafeWrite mv i (Just payload)
                            go (i + 1) (off2 + len)
                    else do
                      VM.unsafeWrite mv i Nothing
                      go (i + 1) off
      r <- go 0 0
      case r of
        Left e   -> pure (Left e)
        Right () -> Right <$> V.unsafeFreeze mv

-- | Maximum repetition and definition levels for a leaf column identified by
-- @path@ (same as 'Parquet.Types.ColumnMetadata' @path_in_schema@), from a
-- preorder Parquet schema. On success: @(max repetition level, max definition level)@.
maxLevelsForColumnPath ::
  V.Vector SchemaElement ->
  V.Vector Text ->
  Either String (Int, Int)
maxLevelsForColumnPath sch path
  | V.null sch = Left "Parquet.Levels: empty schema"
  | V.null path = Left "Parquet.Levels: empty path"
  | otherwise = descend 0 (V.toList path) (0, 0)
  where
    descend :: Int -> [Text] -> (Int, Int) -> Either String (Int, Int)
    descend _ [] _ = Left "Parquet.Levels: empty path segment"
    descend parent (p : ps) acc = do
      c <- findChild sch parent p
      let el = V.unsafeIndex sch c
          acc' = stepLevels (fromMaybe Required (seRepetition el)) acc
      case seType el of
        Just _
          | not (null ps) ->
              Left "Parquet.Levels: path continues past primitive column"
          | otherwise -> Right acc'
        Nothing -> case seNumChildren el of
          Nothing -> Left "Parquet.Levels: group without num_children"
          Just _
            | null ps ->
                Left "Parquet.Levels: path ends at group, not leaf"
            | otherwise -> descend c ps acc'

    stepLevels :: Repetition -> (Int, Int) -> (Int, Int)
    stepLevels Required (!maxRep, !maxDef) = (maxRep, maxDef)
    stepLevels Optional (!maxRep, !maxDef) = (maxRep, maxDef + 1)
    stepLevels Repeated (!maxRep, !maxDef) = (maxRep + 1, maxDef + 1)

-- | Size of the preorder subtree rooted at @i@ (including @i@).
subtreeSize :: V.Vector SchemaElement -> Int -> Either String Int
subtreeSize sch i
  | i < 0 || i >= V.length sch =
      Left "Parquet.Levels: schema index out of range"
  | otherwise =
      case seNumChildren (V.unsafeIndex sch i) of
        Nothing -> Right 1
        Just k0 ->
          let k = fromIntegral k0 :: Int
              go !idx !remaining !total
                | remaining == 0 = Right (1 + total)
                | otherwise = do
                    sz <- subtreeSize sch idx
                    go (idx + sz) (remaining - 1) (total + sz)
           in go (i + 1) k 0

-- | Index of the direct child of @parent@ named @name@.
findChild :: V.Vector SchemaElement -> Int -> Text -> Either String Int
findChild sch parent name = do
  k0 <- case seNumChildren (V.unsafeIndex sch parent) of
    Nothing -> Left "Parquet.Levels: parent is not a group"
    Just k -> Right (fromIntegral k :: Int)
  let go !idx !remaining
        | remaining == 0 =
            Left "Parquet.Levels: column path not found in schema"
        | idx >= V.length sch =
            Left "Parquet.Levels: schema truncated while searching children"
        | seName (V.unsafeIndex sch idx) == name = Right idx
        | otherwise = do
            sz <- subtreeSize sch idx
            go (idx + sz) (remaining - 1)
  go (parent + 1) k0

-- | Materialize a repeated @INT32@ column using repetition and definition levels.
--
-- @rep=0@ starts a new top-level row. Within a row, @rep>0@ continues the same
-- list. @def \< maxDef@ means the list element is null.
materializeRepeatedInt32 ::
  VP.Vector Int32 ->
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (V.Vector (Maybe Int32)))
materializeRepeatedInt32 reps defs maxDef plain
  | VP.length reps /= VP.length defs =
      Left "Parquet.Levels: rep/def level count mismatch"
  | otherwise = case goRepI32 [] [] 0 0 of
      Left e -> Left e
      Right (rows, _) ->
        Right $! V.fromList (reverse (map (V.fromList . reverse) rows))
  where
    !n = VP.length reps
    !maxD = fromIntegral maxDef :: Int32

    goRepI32 :: [[Maybe Int32]] -> [Maybe Int32] -> Int -> Int
             -> Either String ([[Maybe Int32]], Int)
    goRepI32 !rows !curRow !i !off
      | i >= n =
          let !finalRows = if null curRow then rows else curRow : rows
          in Right (finalRows, off)
      | otherwise =
          let !r = VP.unsafeIndex reps i
              !d = VP.unsafeIndex defs i
              !rows' = if r == 0
                         then if null curRow then rows else curRow : rows
                         else rows
              !row' = if r == 0 then [] else curRow
          in if d == maxD
               then
                 if off + 4 > BS.length plain
                   then Left "Parquet.Levels: PLAIN INT32 buffer too small for repeated column"
                   else let !v = fromIntegral (readLE32 plain off) :: Int32
                        in goRepI32 rows' (Just v : row') (i + 1) (off + 4)
               else goRepI32 rows' (Nothing : row') (i + 1) off

-- | Materialize a repeated @BYTE_ARRAY@ column using repetition and definition levels.
materializeRepeatedByteArray ::
  VP.Vector Int32 ->
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (V.Vector (Maybe ByteString)))
materializeRepeatedByteArray reps defs maxDef plain
  | VP.length reps /= VP.length defs =
      Left "Parquet.Levels: rep/def level count mismatch"
  | otherwise = case goRepBA [] [] 0 0 of
      Left e -> Left e
      Right (rows, _) ->
        Right $! V.fromList (reverse (map (V.fromList . reverse) rows))
  where
    !n = VP.length reps
    !maxD = fromIntegral maxDef :: Int32

    goRepBA :: [[Maybe ByteString]] -> [Maybe ByteString] -> Int -> Int
            -> Either String ([[Maybe ByteString]], Int)
    goRepBA !rows !curRow !i !off
      | i >= n =
          let !finalRows = if null curRow then rows else curRow : rows
          in Right (finalRows, off)
      | otherwise =
          let !r = VP.unsafeIndex reps i
              !d = VP.unsafeIndex defs i
              !rows' = if r == 0
                         then if null curRow then rows else curRow : rows
                         else rows
              !row' = if r == 0 then [] else curRow
          in if d == maxD
               then
                 if off + 4 > BS.length plain
                   then Left "Parquet.Levels: BYTE_ARRAY truncated length for repeated column"
                   else let !len = fromIntegral (readLE32 plain off) :: Int
                            !off2 = off + 4
                        in if len < 0 || off2 + len > BS.length plain
                             then Left "Parquet.Levels: BYTE_ARRAY payload out of bounds for repeated column"
                             else let !val = BS.take len (BS.drop off2 plain)
                                  in goRepBA rows' (Just val : row') (i + 1) (off2 + len)
               else goRepBA rows' (Nothing : row') (i + 1) off

-- | Materialize a repeated @INT64@ column using repetition and definition levels.
materializeRepeatedInt64 ::
  VP.Vector Int32 ->
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (V.Vector (Maybe Int64)))
materializeRepeatedInt64 =
  materializeFixedWidth 8 decodeI64
  where
    decodeI64 !bs !off = fromIntegral (readLE64 bs off) :: Int64

-- | Materialize a repeated @FLOAT@ column using repetition and definition levels.
materializeRepeatedFloat ::
  VP.Vector Int32 ->
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (V.Vector (Maybe Float)))
materializeRepeatedFloat =
  materializeFixedWidth 4 (\bs off -> castWord32ToFloat (readLE32 bs off))

-- | Materialize a repeated @DOUBLE@ column using repetition and definition levels.
materializeRepeatedDouble ::
  VP.Vector Int32 ->
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (V.Vector (Maybe Double)))
materializeRepeatedDouble =
  materializeFixedWidth 8 (\bs off -> castWord64ToDouble (readLE64 bs off))

-- | Fixed-width primitive repeated materialiser. Walks rep/def
-- levels row-grouped and lifts the fixed-size on-disk slice at
-- @off@ via @decode bs off@ when @d == maxDef@.
materializeFixedWidth
  :: Int
  -> (ByteString -> Int -> a)
  -> VP.Vector Int32
  -> VP.Vector Int32
  -> Int
  -> ByteString
  -> Either String (V.Vector (V.Vector (Maybe a)))
materializeFixedWidth !w decode reps defs maxDef plain
  | VP.length reps /= VP.length defs =
      Left "Parquet.Levels: rep/def level count mismatch"
  | otherwise = case go [] [] 0 0 of
      Left e -> Left e
      Right (rows, _) ->
        Right $! V.fromList (reverse (map (V.fromList . reverse) rows))
  where
    !n = VP.length reps
    !maxD = fromIntegral maxDef :: Int32

    go !rows !curRow !i !off
      | i >= n =
          let !finalRows = if null curRow then rows else curRow : rows
          in Right (finalRows, off)
      | otherwise =
          let !r = VP.unsafeIndex reps i
              !d = VP.unsafeIndex defs i
              !rows' = if r == 0
                         then if null curRow then rows else curRow : rows
                         else rows
              !row' = if r == 0 then [] else curRow
          in if d == maxD
               then
                 if off + w > BS.length plain
                   then Left "Parquet.Levels: PLAIN buffer too small for repeated column"
                   else let !v = decode plain off
                        in go rows' (Just v : row') (i + 1) (off + w)
               else go rows' (Nothing : row') (i + 1) off

-- | Decode a column with arbitrary repetition depth (i.e. nested
-- @LIST<LIST<…<T>>>@) using the standard Dremel reconstruction
-- algorithm. Returns one 'NestedValue' tree per top-level row.
--
-- The parameter @maxRep@ is the maximum repetition level (number of
-- nested @LIST@ wrappers). A @rep == 0@ marker starts a new
-- top-level row; @rep == k@ starts a new sibling at depth @k@.
--
-- @
--   maxRep = 2
--   reps  = 0 2 1 2 0 1
--   defs  = 2 2 2 2 2 2   (all present, maxDef == 2)
--   →
--   row 0 = [ [v0, v1], [v2, v3] ]
--   row 1 = [ [v4],     [v5]      ]   -- second top-level row
-- @
--
-- The caller supplies a per-leaf decoder that lifts one on-disk
-- value to a @Maybe a@ and returns the advanced offset. A
-- non-maximal definition level produces a 'NVLeaf' @Nothing@ at
-- the appropriate depth (Dremel's null-with-shape semantics).
materializeRepeatedByNested
  :: VP.Vector Int32
  -> VP.Vector Int32
  -> Int                             -- ^ maxDef
  -> Int                             -- ^ maxRep
  -> ByteString
  -> (ByteString -> Int -> Either String (a, Int))
                                      -- ^ per-leaf decoder
  -> Either String (V.Vector (NestedValue a))
materializeRepeatedByNested reps defs maxDef maxRep plain leafDecode
  | VP.length reps /= VP.length defs =
      Left "Parquet.Levels: rep/def level count mismatch"
  | maxRep < 0 =
      Left "Parquet.Levels: negative maxRep"
  | VP.null reps = Right V.empty
  | otherwise = go 0 0 []
  where
    !n   = VP.length reps
    !maxD = fromIntegral maxDef :: Int32

    -- Decode the leaf at cursor @i@ given the on-disk @off@. When
    -- @def < maxDef@ the row position contains a NULL (no value
    -- bytes consumed); the returned 'NestedValue' is then 'NVLeaf
    -- Nothing' at the depth implied by 'def'.
    decodeLeaf !off !i = do
      let !d = VP.unsafeIndex defs i
      if d == maxD
        then do
          (v, off') <- leafDecode plain off
          Right (NVLeaf (Just v), off')
        else Right (NVLeaf Nothing, off)

    -- Top-level driver. Accumulates rows into 'rev' (in reverse).
    -- @off@ is the current decoder position into @plain@; @i@ is
    -- the current rep/def cursor.
    go !off !i rev
      | i >= n   = Right (V.fromList (reverse rev))
      | otherwise = do
          (row, off', i') <- consumeRow off i
          go off' i' (row : rev)

    -- Read one top-level row, returning a 'NestedValue' tree.
    consumeRow !off !i = do
      -- A row always starts at rep == 0; skip the check for the
      -- first item so callers can pass the initial cursor cleanly.
      let !d = VP.unsafeIndex defs i
      (leaf, off1) <- decodeLeaf off i
      buildRow off1 (i + 1) (singletonAtDepth maxRep leaf (fromIntegral d))

    -- @i@ is the cursor for the NEXT triple; @acc@ is the row
    -- built so far (a NestedValue tree that grows to the right).
    -- Continue while @rep > 0@ — that means "still in this row".
    buildRow !off !i acc
      | i >= n = Right (acc, off, i)
      | VP.unsafeIndex reps i == 0 = Right (acc, off, i)
      | otherwise = do
          let !d = VP.unsafeIndex defs i
              !r = fromIntegral (VP.unsafeIndex reps i) :: Int
          (leaf, off1) <- decodeLeaf off i
          let !acc' = appendAtDepth maxRep r (fromIntegral d) leaf acc
          buildRow off1 (i + 1) acc'

-- | Build the initial single-leaf row value: wrap @leaf@ in
-- @maxRep@ list layers so the leaf lands at the innermost
-- position.
--
-- Nuance: a row with @def \< maxDef@ in a nested column actually
-- represents an /empty list/ or /null at some intermediate level/
-- — Dremel's def-level encoding distinguishes those cases — but
-- the standard Parquet/Dremel reconstruction requires you to look
-- at @def@ relative to the schema's nullable layers. For now we
-- always emit a fully-nested singleton; callers that need the
-- empty-list case should consult the def stream directly.
singletonAtDepth :: Int -> NestedValue a -> Int -> NestedValue a
singletonAtDepth !depth !leaf !_def = wrapLists depth leaf

-- | Append a new leaf to an existing nested value tree at the
-- specified rep depth. The leaf is wrapped so its physical depth
-- under the row root is @maxRep@ (i.e. it lands as an innermost-
-- list element). The /append point/ is at depth @rep@ of the
-- existing tree:
--
--   * @rep == maxRep@: descend to the innermost list and append
--     the bare leaf as a new sibling.
--   * @rep == maxRep-1@: descend one level less and append a /new
--     innermost list containing the leaf/.
--   * In general: descend @rep - 1@ list layers (rightmost child
--     each time), then append a new node of depth @maxRep - rep@
--     to that list.
appendAtDepth :: Int -> Int -> Int -> NestedValue a -> NestedValue a -> NestedValue a
appendAtDepth !maxRep !rep !_def !leaf !tree =
  goDescend (rep - 1) tree
  where
    -- The new sibling we'll attach: leaf wrapped in @(maxRep - rep)@
    -- list layers so its leaf depth becomes @maxRep@.
    !newSibling = wrapLists (maxRep - rep) leaf

    -- Descend @k@ list layers from the current node (rightmost
    -- child each time), then append @newSibling@ to that list.
    goDescend !k node = case node of
      NVList xs
        | k <= 0    -> NVList (xs ++ [newSibling])
        | otherwise -> case reverse xs of
            (lastChild : revInit) ->
              NVList (reverse revInit ++ [goDescend (k - 1) lastChild])
            [] -> NVList [newSibling]
      NVLeaf _ ->
        -- Shouldn't happen if maxRep > 0 and the row was started
        -- correctly, but be defensive.
        NVList [node, newSibling]

-- | Wrap a value in @k@ singleton lists.
wrapLists :: Int -> NestedValue a -> NestedValue a
wrapLists !k v
  | k <= 0    = v
  | otherwise = NVList [wrapLists (k - 1) v]

-- | A nested value: either a leaf (possibly null) or a list of
-- children. The caller's row is one 'NestedValue' per top-level
-- column entry.
data NestedValue a
  = NVLeaf !(Maybe a)
  | NVList ![NestedValue a]
  deriving stock (Show, Eq)
