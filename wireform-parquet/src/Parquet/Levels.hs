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
  , materializeRepeatedBool
  , materializeRepeatedByteArray
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

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

readLE32 :: ByteString -> Int -> Word32
readLE32 bs o =
  let b0 = fromIntegral (BS.index bs o) :: Word32
      b1 = fromIntegral (BS.index bs (o + 1)) :: Word32
      b2 = fromIntegral (BS.index bs (o + 2)) :: Word32
      b3 = fromIntegral (BS.index bs (o + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

readLE64 :: ByteString -> Int -> Word64
readLE64 bs o =
  let w0 = fromIntegral (readLE32 bs o) :: Word64
      w1 = fromIntegral (readLE32 bs (o + 4)) :: Word64
  in w0 .|. (w1 `shiftL` 32)

{-# INLINE readBitLsb #-}
readBitLsb :: ByteString -> Int -> Bool
readBitLsb bs bitIdx =
  let bi = bitIdx `quot` 8
      ii = bitIdx `rem` 8
      b = BS.index bs bi
  in (b `shiftR` ii) .&. 1 /= 0

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
materializePlainInt32Optional ::
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (Maybe Int32))
materializePlainInt32Optional defs maxDef plain
  | maxDef < 0 = Left "Parquet.Levels: negative max definition level"
  | otherwise =
      let n = VP.length defs
          needPresent =
            VP.foldl'
              ( \a v ->
                  if v == fromIntegral maxDef then a + 1 else a
              )
              0
              defs
          needBytes = needPresent * 4
          go !acc !i !off
            | i >= n =
                if off == needBytes
                  then Right acc
                  else
                    Left $
                      "Parquet.Levels: unconsumed PLAIN bytes: "
                        ++ show (needBytes - off)
            | otherwise =
                let !d = VP.unsafeIndex defs i
                    !maxD = fromIntegral maxDef :: Int32
                 in if d > maxD || d < 0
                      then
                        Left $
                          "Parquet.Levels: definition level out of range: "
                            ++ show d
                      else
                        if d == maxD
                          then
                            let !v = fromIntegral (readLE32 plain off) :: Int32
                             in go (Just v : acc) (i + 1) (off + 4)
                          else go (Nothing : acc) (i + 1) off
       in if BS.length plain < needBytes
            then
              Left $
                "Parquet.Levels: PLAIN INT32 buffer too small for "
                  ++ show needPresent
                  ++ " defined values"
            else case go [] 0 0 of
              Left e -> Left e
              Right xs -> Right $! V.fromList (reverse xs)

-- | @PLAIN@ @INT64@ (8-byte LE per defined value).
materializePlainInt64Optional ::
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (Maybe Int64))
materializePlainInt64Optional defs maxDef plain
  | maxDef < 0 = Left "Parquet.Levels: negative max definition level"
  | otherwise =
      let n = VP.length defs
          needPresent =
            VP.foldl'
              ( \a v ->
                  if v == fromIntegral maxDef then a + 1 else a
              )
              0
              defs
          needBytes = needPresent * 8
          go !acc !i !off
            | i >= n =
                if off == needBytes
                  then Right acc
                  else
                    Left $
                      "Parquet.Levels: unconsumed PLAIN bytes: "
                        ++ show (needBytes - off)
            | otherwise =
                let !d = VP.unsafeIndex defs i
                    !maxD = fromIntegral maxDef :: Int32
                 in if d > maxD || d < 0
                      then
                        Left $
                          "Parquet.Levels: definition level out of range: "
                            ++ show d
                      else
                        if d == maxD
                          then
                            let !v = fromIntegral (readLE64 plain off) :: Int64
                             in go (Just v : acc) (i + 1) (off + 8)
                          else go (Nothing : acc) (i + 1) off
       in if BS.length plain < needBytes
            then
              Left $
                "Parquet.Levels: PLAIN INT64 buffer too small for "
                  ++ show needPresent
                  ++ " defined values"
            else case go [] 0 0 of
              Left e -> Left e
              Right xs -> Right $! V.fromList (reverse xs)

-- | @PLAIN@ @FLOAT@ (4-byte IEEE LE per defined value).
materializePlainFloatOptional ::
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (Maybe Float))
materializePlainFloatOptional defs maxDef plain
  | maxDef < 0 = Left "Parquet.Levels: negative max definition level"
  | otherwise =
      let n = VP.length defs
          needPresent =
            VP.foldl'
              ( \a v ->
                  if v == fromIntegral maxDef then a + 1 else a
              )
              0
              defs
          needBytes = needPresent * 4
          go !acc !i !off
            | i >= n =
                if off == needBytes
                  then Right acc
                  else
                    Left $
                      "Parquet.Levels: unconsumed PLAIN bytes: "
                        ++ show (needBytes - off)
            | otherwise =
                let !d = VP.unsafeIndex defs i
                    !maxD = fromIntegral maxDef :: Int32
                 in if d > maxD || d < 0
                      then
                        Left $
                          "Parquet.Levels: definition level out of range: "
                            ++ show d
                      else
                        if d == maxD
                          then
                            let !w = readLE32 plain off
                                !v = castWord32ToFloat w
                             in go (Just v : acc) (i + 1) (off + 4)
                          else go (Nothing : acc) (i + 1) off
       in if BS.length plain < needBytes
            then
              Left $
                "Parquet.Levels: PLAIN FLOAT buffer too small for "
                  ++ show needPresent
                  ++ " defined values"
            else case go [] 0 0 of
              Left e -> Left e
              Right xs -> Right $! V.fromList (reverse xs)

-- | @PLAIN@ @DOUBLE@ (8-byte IEEE LE per defined value).
materializePlainDoubleOptional ::
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (Maybe Double))
materializePlainDoubleOptional defs maxDef plain
  | maxDef < 0 = Left "Parquet.Levels: negative max definition level"
  | otherwise =
      let n = VP.length defs
          needPresent =
            VP.foldl'
              ( \a v ->
                  if v == fromIntegral maxDef then a + 1 else a
              )
              0
              defs
          needBytes = needPresent * 8
          go !acc !i !off
            | i >= n =
                if off == needBytes
                  then Right acc
                  else
                    Left $
                      "Parquet.Levels: unconsumed PLAIN bytes: "
                        ++ show (needBytes - off)
            | otherwise =
                let !d = VP.unsafeIndex defs i
                    !maxD = fromIntegral maxDef :: Int32
                 in if d > maxD || d < 0
                      then
                        Left $
                          "Parquet.Levels: definition level out of range: "
                            ++ show d
                      else
                        if d == maxD
                          then
                            let !w = readLE64 plain off
                                !v = castWord64ToDouble w
                             in go (Just v : acc) (i + 1) (off + 8)
                          else go (Nothing : acc) (i + 1) off
       in if BS.length plain < needBytes
            then
              Left $
                "Parquet.Levels: PLAIN DOUBLE buffer too small for "
                  ++ show needPresent
                  ++ " defined values"
            else case go [] 0 0 of
              Left e -> Left e
              Right xs -> Right $! V.fromList (reverse xs)

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
      let n = VP.length defs
          needBits =
            VP.foldl'
              ( \a v ->
                  if v == fromIntegral maxDef then a + 1 else a
              )
              0
              defs
          needBytes = (needBits + 7) `quot` 8
          go !acc !i !bitPos
            | i >= n =
                if bitPos == needBits
                  then Right acc
                  else
                    Left $
                      "Parquet.Levels: BOOLEAN bit stream incomplete: "
                        ++ show (needBits - bitPos)
            | otherwise =
                let !d = VP.unsafeIndex defs i
                    !maxD = fromIntegral maxDef :: Int32
                 in if d > maxD || d < 0
                      then
                        Left $
                          "Parquet.Levels: definition level out of range: "
                            ++ show d
                      else
                        if d == maxD
                          then
                            let !b = readBitLsb plain bitPos
                             in go (Just b : acc) (i + 1) (bitPos + 1)
                          else go (Nothing : acc) (i + 1) bitPos
       in if BS.length plain < needBytes
            then
              Left $
                "Parquet.Levels: PLAIN BOOLEAN buffer too small for "
                  ++ show needBits
                  ++ " defined bits"
            else case go [] 0 0 of
              Left e -> Left e
              Right xs -> Right $! V.fromList (reverse xs)

-- | @PLAIN@ @BYTE_ARRAY@ — per defined value: 4-byte LE length + payload.
materializePlainByteArrayOptional ::
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (Maybe ByteString))
materializePlainByteArrayOptional defs maxDef plain
  | maxDef < 0 = Left "Parquet.Levels: negative max definition level"
  | otherwise =
      let n = VP.length defs
          go !acc !i !off
            | i >= n =
                if off == BS.length plain
                  then Right acc
                  else
                    Left $
                      "Parquet.Levels: trailing PLAIN BYTE_ARRAY bytes: "
                        ++ show (BS.length plain - off)
            | otherwise =
                let !d = VP.unsafeIndex defs i
                    !maxD = fromIntegral maxDef :: Int32
                 in if d > maxD || d < 0
                      then
                        Left $
                          "Parquet.Levels: definition level out of range: "
                            ++ show d
                      else
                        if d == maxD
                          then
                            if off + 4 > BS.length plain
                              then Left "Parquet.Levels: PLAIN BYTE_ARRAY truncated length"
                              else
                                let !len = fromIntegral (readLE32 plain off) :: Int
                                    !off2 = off + 4
                                 in if len < 0 || off2 + len > BS.length plain
                                      then
                                        Left "Parquet.Levels: PLAIN BYTE_ARRAY payload out of bounds"
                                      else
                                        let !payload = BS.take len (BS.drop off2 plain)
                                         in go (Just payload : acc) (i + 1) (off2 + len)
                          else go (Nothing : acc) (i + 1) off
       in case go [] 0 0 of
        Left e -> Left e
        Right xs -> Right $! V.fromList (reverse xs)

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

-- | Materialize a repeated @INT64@ column using repetition and definition levels.
materializeRepeatedInt64 ::
  VP.Vector Int32 ->
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (V.Vector (Maybe Int64)))
materializeRepeatedInt64 = materializeRepeatedFixed 8 readLE64I64
  where
    readLE64I64 plain o = fromIntegral (readLE64 plain o) :: Int64

-- | Materialize a repeated @FLOAT@ column using repetition and definition levels.
materializeRepeatedFloat ::
  VP.Vector Int32 ->
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (V.Vector (Maybe Float)))
materializeRepeatedFloat = materializeRepeatedFixed 4 (\plain o -> castWord32ToFloat (readLE32 plain o))

-- | Materialize a repeated @DOUBLE@ column using repetition and definition levels.
materializeRepeatedDouble ::
  VP.Vector Int32 ->
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (V.Vector (Maybe Double)))
materializeRepeatedDouble = materializeRepeatedFixed 8 (\plain o -> castWord64ToDouble (readLE64 plain o))

-- | Generic backing helper for repeated /fixed-width/ primitive columns.
materializeRepeatedFixed
  :: Int                              -- ^ bytes per value
  -> (ByteString -> Int -> a)          -- ^ reader
  -> VP.Vector Int32                   -- ^ rep levels
  -> VP.Vector Int32                   -- ^ def levels
  -> Int                               -- ^ max def
  -> ByteString                        -- ^ PLAIN payload
  -> Either String (V.Vector (V.Vector (Maybe a)))
materializeRepeatedFixed !bytesPer reader reps defs maxDef plain
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
                 if off + bytesPer > BS.length plain
                   then Left "Parquet.Levels: PLAIN buffer too small for repeated column"
                   else let !v = reader plain off
                        in go rows' (Just v : row') (i + 1) (off + bytesPer)
               else go rows' (Nothing : row') (i + 1) off

-- | Materialize a repeated @BOOLEAN@ column using repetition and
-- definition levels.  Booleans are bit-packed (LSB-first) in the
-- payload; only /defined/ rows consume bits.
materializeRepeatedBool ::
  VP.Vector Int32 ->
  VP.Vector Int32 ->
  Int ->
  ByteString ->
  Either String (V.Vector (V.Vector (Maybe Bool)))
materializeRepeatedBool reps defs maxDef plain
  | VP.length reps /= VP.length defs =
      Left "Parquet.Levels: rep/def level count mismatch"
  | otherwise = case goRepBool [] [] 0 0 of
      Left e -> Left e
      Right (rows, _) ->
        Right $! V.fromList (reverse (map (V.fromList . reverse) rows))
  where
    !n = VP.length reps
    !maxD = fromIntegral maxDef :: Int32

    goRepBool :: [[Maybe Bool]] -> [Maybe Bool] -> Int -> Int
              -> Either String ([[Maybe Bool]], Int)
    goRepBool !rows !curRow !i !bitPos
      | i >= n =
          let !finalRows = if null curRow then rows else curRow : rows
          in Right (finalRows, bitPos)
      | otherwise =
          let !r = VP.unsafeIndex reps i
              !d = VP.unsafeIndex defs i
              !rows' = if r == 0
                         then if null curRow then rows else curRow : rows
                         else rows
              !row' = if r == 0 then [] else curRow
          in if d == maxD
               then
                 let !bytePos = bitPos `quot` 8
                 in if bytePos >= BS.length plain
                      then Left "Parquet.Levels: PLAIN BOOLEAN buffer too small for repeated column"
                      else let !b = readBitLsb plain bitPos
                           in goRepBool rows' (Just b : row') (i + 1) (bitPos + 1)
               else goRepBool rows' (Nothing : row') (i + 1) bitPos

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
