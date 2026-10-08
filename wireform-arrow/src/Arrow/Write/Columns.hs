{-# LANGUAGE BangPatterns #-}

{- | Arrow IPC column encoders: lay a 'ColumnArray' out as Arrow field
nodes and body buffers (depth-first pre-order, validity first). The
message framing lives in "Arrow.FlatBufferIPC"; the high-level stream
and file writers are in "Arrow.Stream" and "Arrow.Write".
-}
module Arrow.Write.Columns (
  encodePlainInt32Column,
  encodePlainInt64Column,
  encodePlainFloat,
  encodePlainDouble,
  encodePlainBool,
  encodePlainUtf8,
  encodeNullBitmap,

  -- * Column-tree encoding
  validateColumns,
  encodeColumns,
  emptyBuildAcc,
  BuildAcc (..),
) where

import Arrow.Column (ColumnArray (..), columnLength, isNullableColumn)
import Arrow.Types
import Data.Bits (complement, shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Word (Word16, Word32, Word64, Word8)
import Wireform.Builder qualified as B


-- * Plain column encoders


encodePlainInt32Column :: VP.Vector Int32 -> ByteString
encodePlainInt32Column vec =
  BL.toStrict $
    B.toLazyByteString $
      VP.foldl' (\acc v -> acc <> B.int32LE v) mempty vec


encodePlainInt64Column :: VP.Vector Int64 -> ByteString
encodePlainInt64Column vec =
  BL.toStrict $
    B.toLazyByteString $
      VP.foldl' (\acc v -> acc <> B.int64LE v) mempty vec


encodePlainFloat :: VP.Vector Float -> ByteString
encodePlainFloat vec =
  BL.toStrict $
    B.toLazyByteString $
      VP.foldl' (\acc v -> acc <> B.floatLE v) mempty vec


encodePlainDouble :: VP.Vector Double -> ByteString
encodePlainDouble vec =
  BL.toStrict $
    B.toLazyByteString $
      VP.foldl' (\acc v -> acc <> B.doubleLE v) mempty vec


encodePlainBool :: V.Vector Bool -> ByteString
encodePlainBool vec =
  let !n = V.length vec
      !nBytes = (n + 7) `quot` 8
      packByte !byteIdx =
        let !base = byteIdx * 8
            goBit !acc !bit
              | bit >= 8 = acc
              | base + bit >= n = acc
              | V.unsafeIndex vec (base + bit) = goBit (acc .|. (1 `shiftL` bit)) (bit + 1)
              | otherwise = goBit acc (bit + 1)
        in goBit (0 :: Word8) 0
      go !i
        | i >= nBytes = mempty
        | otherwise = B.word8 (packByte i) <> go (i + 1)
  in BL.toStrict (B.toLazyByteString (go 0))


encodePlainUtf8 :: V.Vector Text -> (ByteString, ByteString)
encodePlainUtf8 vec =
  let !n = V.length vec
      go !i !off !offB !datB
        | i >= n =
            ( BL.toStrict (B.toLazyByteString (offB <> B.int32LE off))
            , BL.toStrict (B.toLazyByteString datB)
            )
        | otherwise =
            let !bs = TE.encodeUtf8 (V.unsafeIndex vec i)
                !len = fromIntegral (BS.length bs) :: Int32
            in go (i + 1) (off + len) (offB <> B.int32LE off) (datB <> B.byteString bs)
  in go 0 0 mempty mempty


encodeNullBitmap :: V.Vector Bool -> ByteString
encodeNullBitmap = encodePlainBool


-- * Internal column encoders


encodePlainBinary :: V.Vector ByteString -> (ByteString, ByteString)
encodePlainBinary vec =
  let !n = V.length vec
      go !i !off !offB !datB
        | i >= n =
            ( BL.toStrict (B.toLazyByteString (offB <> B.int32LE off))
            , BL.toStrict (B.toLazyByteString datB)
            )
        | otherwise =
            let !bs = V.unsafeIndex vec i
                !len = fromIntegral (BS.length bs) :: Int32
            in go (i + 1) (off + len) (offB <> B.int32LE off) (datB <> B.byteString bs)
  in go 0 0 mempty mempty


encodeInt8s :: VP.Vector Int8 -> ByteString
encodeInt8s vec =
  BL.toStrict $
    B.toLazyByteString $
      VP.foldl' (\acc v -> acc <> B.int8 v) mempty vec


encodeInt16s :: VP.Vector Int16 -> ByteString
encodeInt16s vec =
  BL.toStrict $
    B.toLazyByteString $
      VP.foldl' (\acc v -> acc <> B.int16LE v) mempty vec


-- Unsigned integer encoders.
encodeUInt8s :: VP.Vector Word8 -> ByteString
encodeUInt8s vec =
  BL.toStrict $
    B.toLazyByteString $
      VP.foldl' (\acc v -> acc <> B.word8 v) mempty vec


encodeUInt16s :: VP.Vector Word16 -> ByteString
encodeUInt16s vec =
  BL.toStrict $
    B.toLazyByteString $
      VP.foldl' (\acc v -> acc <> B.word16LE v) mempty vec


encodeUInt32s :: VP.Vector Word32 -> ByteString
encodeUInt32s vec =
  BL.toStrict $
    B.toLazyByteString $
      VP.foldl' (\acc v -> acc <> B.word32LE v) mempty vec


encodeUInt64s :: VP.Vector Word64 -> ByteString
encodeUInt64s vec =
  BL.toStrict $
    B.toLazyByteString $
      VP.foldl' (\acc v -> acc <> B.word64LE v) mempty vec


-- Half-precision floats are just raw 16-bit words.
encodeFloat16s :: VP.Vector Word16 -> ByteString
encodeFloat16s = encodeUInt16s


-- Int64 encoder for LargeList / LargeBinary / LargeUtf8 offsets.
encodePlainInt64Offsets :: VP.Vector Int64 -> ByteString
encodePlainInt64Offsets vec =
  BL.toStrict $
    B.toLazyByteString $
      VP.foldl' (\acc v -> acc <> B.int64LE v) mempty vec


-- Int32 array encoder that accepts the same shape as 'encodeInt16s'.
-- (Left as an alias for discoverability from the encodeCol site.)
encodeInt32s :: VP.Vector Int32 -> ByteString
encodeInt32s = encodePlainInt32Column


encodeInt64s :: VP.Vector Int64 -> ByteString
encodeInt64s = encodePlainInt64Column


-- Large variable-length (Int64 offsets) encoders.
encodePlainLargeUtf8 :: V.Vector Text -> (ByteString, ByteString)
encodePlainLargeUtf8 vec =
  let !n = V.length vec
      go !i !off !offB !datB
        | i >= n =
            ( BL.toStrict (B.toLazyByteString (offB <> B.int64LE off))
            , BL.toStrict (B.toLazyByteString datB)
            )
        | otherwise =
            let !bs = TE.encodeUtf8 (V.unsafeIndex vec i)
                !len = fromIntegral (BS.length bs) :: Int64
            in go (i + 1) (off + len) (offB <> B.int64LE off) (datB <> B.byteString bs)
  in go 0 0 mempty mempty


encodePlainLargeBinary :: V.Vector ByteString -> (ByteString, ByteString)
encodePlainLargeBinary vec =
  let !n = V.length vec
      go !i !off !offB !datB
        | i >= n =
            ( BL.toStrict (B.toLazyByteString (offB <> B.int64LE off))
            , BL.toStrict (B.toLazyByteString datB)
            )
        | otherwise =
            let !bs = V.unsafeIndex vec i
                !len = fromIntegral (BS.length bs) :: Int64
            in go (i + 1) (off + len) (offB <> B.int64LE off) (datB <> B.byteString bs)
  in go 0 0 mempty mempty


-- Fixed-size binary: just concatenate the fixed-width payloads. The
-- caller is expected to have enforced the width (we do a best-effort
-- pad / truncate here so ragged inputs don't corrupt the downstream
-- offsets).
encodePlainFixedSizeBinary :: Int -> V.Vector ByteString -> ByteString
encodePlainFixedSizeBinary !w vec =
  BL.toStrict $
    B.toLazyByteString $
      V.foldl'
        ( \acc bs ->
            let !raw = BS.length bs
            in if raw == w
                 then acc <> B.byteString bs
                 else
                   if raw > w
                     then acc <> B.byteString (BS.take w bs)
                     else
                       acc
                         <> B.byteString bs
                         <> B.byteString (BS.replicate (w - raw) 0)
        )
        mempty
        vec


-- Interval encoders (YearMonth / DayTime / MonthDayNano).
encodeIntervalYearMonth :: VP.Vector Int32 -> ByteString
encodeIntervalYearMonth = encodePlainInt32Column


encodeIntervalDayTime :: VP.Vector Int32 -> VP.Vector Int32 -> ByteString
encodeIntervalDayTime days millis =
  BL.toStrict $
    B.toLazyByteString $
      let !n = min (VP.length days) (VP.length millis)
          go !i
            | i >= n = mempty
            | otherwise =
                B.int32LE (VP.unsafeIndex days i)
                  <> B.int32LE (VP.unsafeIndex millis i)
                  <> go (i + 1)
      in go 0


encodeIntervalMonthDayNano
  :: VP.Vector Int32 -> VP.Vector Int32 -> VP.Vector Int64 -> ByteString
encodeIntervalMonthDayNano months days nanos =
  BL.toStrict $
    B.toLazyByteString $
      let !n = min (VP.length months) (min (VP.length days) (VP.length nanos))
          go !i
            | i >= n = mempty
            | otherwise =
                B.int32LE (VP.unsafeIndex months i)
                  <> B.int32LE (VP.unsafeIndex days i)
                  <> B.int64LE (VP.unsafeIndex nanos i)
                  <> go (i + 1)
      in go 0


alignUp8 :: Int -> Int
alignUp8 n = (n + 7) .&. complement 7


-- * Record batch builder accumulator


data BuildAcc = BuildAcc
  { baOffset :: !Int64
  , baNodes :: ![FieldNode]
  , baBufs :: ![Buffer]
  , baBody :: !B.Builder
  , baVariadic :: ![Int64]
  {- ^ For each Utf8View / BinaryView field encountered (in
  pre-order DFS), the number of variadic data buffers emitted.
  The Arrow @RecordBatch.variadicBufferCounts@ vector is the
  reverse of this list at the end of encoding.
  -}
  }


emptyBuildAcc :: BuildAcc
emptyBuildAcc = BuildAcc 0 [] [] mempty []


addBufData :: ByteString -> BuildAcc -> BuildAcc
addBufData bs (BuildAcc off ns bufs body var) =
  let !rawLen = BS.length bs
      !padded = alignUp8 rawLen
      !pad = padded - rawLen
  in BuildAcc
       (off + fromIntegral padded)
       ns
       (Buffer off (fromIntegral rawLen) : bufs)
       (body <> B.byteString bs <> B.byteString (BS.replicate pad 0))
       var


addFieldNode :: Int64 -> Int64 -> BuildAcc -> BuildAcc
addFieldNode len nc (BuildAcc off ns bufs body var) =
  BuildAcc off (FieldNode len nc : ns) bufs body var


{- | Record one entry in the variadic-buffer-counts vector for the
/current/ view column. Called exactly once per ColUtf8View /
ColBinaryView encoded.
-}
addVariadicCount :: Int64 -> BuildAcc -> BuildAcc
addVariadicCount c (BuildAcc off ns bufs body var) =
  BuildAcc off ns bufs body (c : var)


countNulls :: V.Vector (Maybe a) -> Int
countNulls = V.foldl' (\c x -> case x of Nothing -> c + 1; Just _ -> c) 0


-- * Column encoding (DFS preorder, matching Arrow spec)


{- | Lay the columns out for the fields. The columns must agree with the
fields (see 'validateColumns'); this function does not check.
-}
encodeColumns :: V.Vector Field -> V.Vector ColumnArray -> BuildAcc -> BuildAcc
encodeColumns fields cols acc =
  V.ifoldl' (\a i f -> encodeCol f (V.unsafeIndex cols i) a) acc fields


{- | Check that a batch agrees with the schema fields before it is
encoded: one column per field, every column with the same row count,
and every column (recursively) of its field's shape:

* the constructor matches the field type (integer width and
  signedness, float precision, date / time / interval unit class,
  decimal precision and scale, fixed widths, list size), and a
  dictionary-encoded field holds a 'ColDictionary' /
  'ColDictionaryMaybe' with the field's dictionary id whose values
  match the value type;
* a nullable (@*Maybe@) column sits under a nullable field (a
  non-nullable column under a nullable field is fine: every row is
  valid);
* nested columns have one child per child field, struct children have
  exactly the struct's row count, a fixed-size list child has exactly
  @rows * size@ elements, list and map offsets lie inside the child,
  validity vectors, offsets and sizes have the row count, and sparse
  union children cover every row.

Only lengths and constructors are inspected; no per-row work.
-}
validateColumns :: V.Vector Field -> V.Vector ColumnArray -> Either String ()
validateColumns fields cols
  | V.length fields /= V.length cols =
      Left
        ( "Arrow.Write: batch has "
            ++ show (V.length cols)
            ++ " columns, the schema has "
            ++ show (V.length fields)
            ++ " fields"
        )
  | otherwise = do
      let !n = if V.null cols then 0 else columnLength (V.head cols)
      case V.findIndex (\c -> columnLength c /= n) cols of
        Just i ->
          Left
            ( "Arrow.Write: column "
                ++ show (fieldName (V.unsafeIndex fields i))
                ++ " has "
                ++ show (columnLength (V.unsafeIndex cols i))
                ++ " rows, the first column has "
                ++ show n
            )
        Nothing -> V.zipWithM_ (checkField []) fields cols


-- | One column against its field; @path0@ names the enclosing fields (innermost first).
checkField :: [Text] -> Field -> ColumnArray -> Either String ()
checkField path0 f col = case fieldDictionary f of
  Just de -> case col of
    ColDictionary did _ vals -> dict de did vals
    ColDictionaryMaybe did _ vals
      | not (fieldNullable f) -> bad "nullable dictionary column under a non-nullable field"
      | otherwise -> dict de did vals
    _ -> mismatch
  Nothing
    | isNullableColumn col && not (fieldNullable f) && hasValidity ->
        bad ("nullable column " ++ tagOf col ++ " under a non-nullable field")
    | otherwise -> shape
  where
    path = fieldName f : path0
    bad msg = Left ("Arrow.Write: column " ++ show (T.intercalate "." (reverse path)) ++ ": " ++ msg)
    mismatch = bad ("field type " ++ show (fieldType f) ++ " cannot hold a " ++ tagOf col ++ " column")
    ok = Right ()
    expect cond msg = if cond then ok else bad msg
    hasValidity = case fieldType f of
      ANull -> False
      AUnion _ _ -> False
      ARunEndEncoded -> False
      _ -> True
    kids = fieldChildren f
    dict de did vals
      | did /= deId de = bad ("dictionary id " ++ show did ++ ", the field declares " ++ show (deId de))
      | otherwise = checkField path0 f {fieldDictionary = Nothing, fieldNullable = isNullableColumn vals} vals
    one k = case V.toList kids of
      [c] -> k c
      _ -> bad (show (fieldType f) ++ " field must have exactly one child")
    rows what n m = expect (n == m) (what ++ " has " ++ show m ++ " entries for " ++ show n ++ " rows")
    offsets :: (Integral o, VP.Prim o) => Maybe Int -> VP.Vector o -> Int -> Either String ()
    offsets mrows offs childLen
      | VP.null offs = bad "offsets are empty (a list needs rows + 1 offsets)"
      | Just r <- mrows, VP.length offs /= r + 1 = bad ("has " ++ show (VP.length offs) ++ " offsets for " ++ show r ++ " rows")
      | VP.head offs < 0 = bad "offsets start below zero"
      | toInteger (VP.last offs) > toInteger childLen =
          bad ("offsets end at " ++ show (toInteger (VP.last offs)) ++ ", the child has " ++ show childLen ++ " rows")
      | otherwise = ok
    listOf mrows offs c = one $ \cf -> offsets mrows offs (columnLength c) >> checkField path cf c
    viewOf mrows offs sizes c = one $ \cf -> do
      rows "sizes" (VP.length offs) (VP.length sizes)
      maybe ok (\r -> rows "offsets" r (VP.length offs)) mrows
      checkField path cf c
    struct n cs
      | V.length cs /= V.length kids =
          bad ("struct column has " ++ show (V.length cs) ++ " children, the field has " ++ show (V.length kids))
      | otherwise =
          V.zipWithM_
            ( \cf (_, c) -> do
                expect
                  (columnLength c == n)
                  ("struct child " ++ show (fieldName cf) ++ " has " ++ show (columnLength c) ++ " rows, the struct has " ++ show n)
                checkField path cf c
            )
            kids
            cs
    fixed w w' n c
      | w /= w' = mismatch
      | otherwise = one $ \cf -> do
          expect
            (columnLength c == n * w)
            ("fixed-size list child has " ++ show (columnLength c) ++ " elements, " ++ show n ++ " rows of size " ++ show w ++ " need " ++ show (n * w))
          checkField path cf c
    mapOf mrows offs k v = case V.toList kids of
      [entries] | [kf, vf] <- V.toList (fieldChildren entries) -> do
        expect (columnLength k == columnLength v) ("map keys have " ++ show (columnLength k) ++ " rows, values " ++ show (columnLength v))
        offsets mrows offs (columnLength k)
        checkField (fieldName entries : path) kf k
        checkField (fieldName entries : path) vf v
      _ -> bad "map field must have one entries struct child with key and value"
    union cs extra
      | V.length cs /= V.length kids =
          bad ("union column has " ++ show (V.length cs) ++ " children, the field has " ++ show (V.length kids))
      | otherwise = V.zipWithM_ (\cf c -> extra cf c >> checkField path cf c) kids cs
    shape = case (fieldType f, col) of
      (ANull, ColNull _) -> ok
      (AInt 8 True, ColInt8 _) -> ok
      (AInt 8 True, ColInt8Maybe _) -> ok
      (AInt 16 True, ColInt16 _) -> ok
      (AInt 16 True, ColInt16Maybe _) -> ok
      (AInt 32 True, ColInt32 _) -> ok
      (AInt 32 True, ColInt32Maybe _) -> ok
      (AInt 64 True, ColInt64 _) -> ok
      (AInt 64 True, ColInt64Maybe _) -> ok
      (AInt 8 False, ColUInt8 _) -> ok
      (AInt 8 False, ColUInt8Maybe _) -> ok
      (AInt 16 False, ColUInt16 _) -> ok
      (AInt 16 False, ColUInt16Maybe _) -> ok
      (AInt 32 False, ColUInt32 _) -> ok
      (AInt 32 False, ColUInt32Maybe _) -> ok
      (AInt 64 False, ColUInt64 _) -> ok
      (AInt 64 False, ColUInt64Maybe _) -> ok
      (AFloatingPoint Half, ColFloat16 _) -> ok
      (AFloatingPoint Half, ColFloat16Maybe _) -> ok
      (AFloatingPoint Single, ColFloat _) -> ok
      (AFloatingPoint Single, ColFloatMaybe _) -> ok
      (AFloatingPoint DoublePrecision, ColDouble _) -> ok
      (AFloatingPoint DoublePrecision, ColDoubleMaybe _) -> ok
      (ABool, ColBool _) -> ok
      (ABool, ColBoolMaybe _) -> ok
      (AUtf8, ColUtf8 _) -> ok
      (AUtf8, ColUtf8Maybe _) -> ok
      (ABinary, ColBinary _) -> ok
      (ABinary, ColBinaryMaybe _) -> ok
      (ALargeUtf8, ColLargeUtf8 _) -> ok
      (ALargeUtf8, ColLargeUtf8Maybe _) -> ok
      (ALargeBinary, ColLargeBinary _) -> ok
      (ALargeBinary, ColLargeBinaryMaybe _) -> ok
      (AUtf8View, ColUtf8View _) -> ok
      (AUtf8View, ColUtf8ViewMaybe _) -> ok
      (ABinaryView, ColBinaryView _) -> ok
      (ABinaryView, ColBinaryViewMaybe _) -> ok
      (AFixedSizeBinary w, ColFixedSizeBinary w' _) -> expect (w == w') ("fixed-size binary width " ++ show w' ++ ", the field declares " ++ show w)
      (AFixedSizeBinary w, ColFixedSizeBinaryMaybe w' _) -> expect (w == w') ("fixed-size binary width " ++ show w' ++ ", the field declares " ++ show w)
      (ADecimal p s, ColDecimal128 p' s' _) -> decimal p s p' s'
      (ADecimal p s, ColDecimal128Maybe p' s' _) -> decimal p s p' s'
      (ADecimal256 p s, ColDecimal256 p' s' _) -> decimal p s p' s'
      (ADecimal256 p s, ColDecimal256Maybe p' s' _) -> decimal p s p' s'
      (ADate DateDay, ColDate32 _) -> ok
      (ADate DateDay, ColDate32Maybe _) -> ok
      (ADate DateMillisecond, ColDate64 _) -> ok
      (ADate DateMillisecond, ColDate64Maybe _) -> ok
      (ATime u _, ColTime32 _) -> expect (time32 u) "time32 column for a 64-bit time unit"
      (ATime u _, ColTime32Maybe _) -> expect (time32 u) "time32 column for a 64-bit time unit"
      (ATime u _, ColTime64 _) -> expect (not (time32 u)) "time64 column for a 32-bit time unit"
      (ATime u _, ColTime64Maybe _) -> expect (not (time32 u)) "time64 column for a 32-bit time unit"
      (ATimestamp _ _, ColTimestamp _) -> ok
      (ATimestamp _ _, ColTimestampMaybe _) -> ok
      (ADuration _, ColDuration _) -> ok
      (ADuration _, ColDurationMaybe _) -> ok
      (AInterval YearMonth, ColIntervalYearMonth _) -> ok
      (AInterval YearMonth, ColIntervalYearMonthMaybe _) -> ok
      (AInterval DayTime, ColIntervalDayTime d m) -> rows "interval milliseconds" (VP.length d) (VP.length m)
      (AInterval DayTime, ColIntervalDayTimeMaybe _) -> ok
      (AInterval MonthDayNano, ColIntervalMonthDayNano m d ns) ->
        rows "interval days" (VP.length m) (VP.length d) >> rows "interval nanoseconds" (VP.length m) (VP.length ns)
      (AInterval MonthDayNano, ColIntervalMonthDayNanoMaybe _) -> ok
      (AStruct, ColStruct n cs) -> struct n cs
      (AStruct, ColStructMaybe v cs) -> struct (V.length v) cs
      (AList, ColList o c) -> listOf Nothing o c
      (AList, ColListMaybe v o c) -> listOf (Just (V.length v)) o c
      (ALargeList, ColLargeList o c) -> listOf Nothing o c
      (ALargeList, ColLargeListMaybe v o c) -> listOf (Just (V.length v)) o c
      (AFixedSizeList w, ColFixedSizeList w' n c) -> fixed w w' n c
      (AFixedSizeList w, ColFixedSizeListMaybe w' v c) -> fixed w w' (V.length v) c
      (AMap _, ColMap o k v) -> mapOf Nothing o k v
      (AMap _, ColMapMaybe valid o k v) -> mapOf (Just (V.length valid)) o k v
      (AListView, ColListView o s c) -> viewOf Nothing o s c
      (AListView, ColListViewMaybe v o s c) -> viewOf (Just (V.length v)) o s c
      (ALargeListView, ColLargeListView o s c) -> viewOf Nothing o s c
      (ALargeListView, ColLargeListViewMaybe v o s c) -> viewOf (Just (V.length v)) o s c
      (AUnion Dense _, ColDenseUnion ts offs cs) -> do
        rows "dense union offsets" (VP.length ts) (VP.length offs)
        union cs (\_ _ -> ok)
      (AUnion Sparse _, ColSparseUnion ts cs) ->
        union cs $ \cf c ->
          expect
            (columnLength c >= VP.length ts)
            ("sparse union child " ++ show (fieldName cf) ++ " has " ++ show (columnLength c) ++ " rows, the union has " ++ show (VP.length ts))
      (ARunEndEncoded, ColRunEndEncoded re vals) -> case V.toList kids of
        [rf, vf] -> do
          expect
            (columnLength vals >= runCount re)
            ("run-end-encoded column has " ++ show (runCount re) ++ " runs but " ++ show (columnLength vals) ++ " values")
          checkField path rf re
          checkField path vf vals
        _ -> bad "run-end-encoded field must have two children (run_ends, values)"
      _ -> mismatch
    decimal p s p' s' =
      expect (p == p' && s == s') ("decimal(" ++ show p' ++ ", " ++ show s' ++ "), the field declares decimal(" ++ show p ++ ", " ++ show s ++ ")")
    time32 u = u == Second || u == Millisecond
    runCount = \case
      ColInt16 v -> VP.length v
      ColInt32 v -> VP.length v
      ColInt64 v -> VP.length v
      _ -> 0


-- | Constructor name of a column, for error messages.
tagOf :: ColumnArray -> String
tagOf = takeWhile (/= ' ') . show


{- | Encode one column (depth-first preorder per the Arrow IPC spec).
Every 'ColumnArray' constructor has a handler; non-null primitive
columns emit a single data buffer, nullable primitives prepend a
validity bitmap, variable-length columns emit (offsets, data),
large variants use 64-bit offsets, and nested columns recurse into
their children.
-}
encodeCol :: Field -> ColumnArray -> BuildAcc -> BuildAcc
encodeCol f col acc = case col of
  -- ============================================================
  -- Non-null primitives
  -- ============================================================
  ColInt8 v -> primFlat (encodeInt8s v) (VP.length v) acc
  ColInt16 v -> primFlat (encodeInt16s v) (VP.length v) acc
  ColInt32 v -> primFlat (encodeInt32s v) (VP.length v) acc
  ColInt64 v -> primFlat (encodeInt64s v) (VP.length v) acc
  ColUInt8 v -> primFlat (encodeUInt8s v) (VP.length v) acc
  ColUInt16 v -> primFlat (encodeUInt16s v) (VP.length v) acc
  ColUInt32 v -> primFlat (encodeUInt32s v) (VP.length v) acc
  ColUInt64 v -> primFlat (encodeUInt64s v) (VP.length v) acc
  ColFloat16 v -> primFlat (encodeFloat16s v) (VP.length v) acc
  ColFloat v -> primFlat (encodePlainFloat v) (VP.length v) acc
  ColDouble v -> primFlat (encodePlainDouble v) (VP.length v) acc
  ColBool v -> primFlat (encodePlainBool v) (V.length v) acc
  -- Date / time / timestamp / duration are all fixed-width integer
  -- payloads under the hood.
  ColDate32 v -> primFlat (encodeInt32s v) (VP.length v) acc
  ColDate64 v -> primFlat (encodeInt64s v) (VP.length v) acc
  ColTime32 v -> primFlat (encodeInt32s v) (VP.length v) acc
  ColTime64 v -> primFlat (encodeInt64s v) (VP.length v) acc
  ColTimestamp v -> primFlat (encodeInt64s v) (VP.length v) acc
  ColDuration v -> primFlat (encodeInt64s v) (VP.length v) acc
  -- Interval / decimal / fixed-size binary: fixed-width payloads
  -- with unit-specific strides.
  ColIntervalYearMonth v ->
    primFlat (encodeIntervalYearMonth v) (VP.length v) acc
  ColIntervalDayTime days millis ->
    primFlat (encodeIntervalDayTime days millis) (VP.length days) acc
  ColIntervalMonthDayNano months days nanos ->
    primFlat (encodeIntervalMonthDayNano months days nanos) (VP.length months) acc
  ColDecimal128 _ _ v ->
    primFlat (encodePlainFixedSizeBinary 16 v) (V.length v) acc
  ColDecimal256 _ _ v ->
    primFlat (encodePlainFixedSizeBinary 32 v) (V.length v) acc
  ColFixedSizeBinary w v ->
    primFlat (encodePlainFixedSizeBinary w v) (V.length v) acc
  -- ============================================================
  -- Non-null variable-length columns
  -- ============================================================
  ColUtf8 v ->
    let (offBs, datBs) = encodePlainUtf8 v
    in varFlat offBs datBs (V.length v) acc
  ColBinary v ->
    let (offBs, datBs) = encodePlainBinary v
    in varFlat offBs datBs (V.length v) acc
  ColLargeUtf8 v ->
    let (offBs, datBs) = encodePlainLargeUtf8 v
    in varFlat offBs datBs (V.length v) acc
  ColLargeBinary v ->
    let (offBs, datBs) = encodePlainLargeBinary v
    in varFlat offBs datBs (V.length v) acc
  -- ============================================================
  -- Nullable primitives (one validity bitmap + one data buffer)
  -- ============================================================
  ColInt8Maybe v -> primNullable encodeInt8s (0 :: Int8) v acc
  ColInt16Maybe v -> primNullable encodeInt16s (0 :: Int16) v acc
  ColInt32Maybe v -> primNullable encodeInt32s (0 :: Int32) v acc
  ColInt64Maybe v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColUInt8Maybe v -> primNullable encodeUInt8s (0 :: Word8) v acc
  ColUInt16Maybe v -> primNullable encodeUInt16s (0 :: Word16) v acc
  ColUInt32Maybe v -> primNullable encodeUInt32s (0 :: Word32) v acc
  ColUInt64Maybe v -> primNullable encodeUInt64s (0 :: Word64) v acc
  ColFloat16Maybe v -> primNullable encodeFloat16s (0 :: Word16) v acc
  ColFloatMaybe v -> primNullable encodePlainFloat (0 :: Float) v acc
  ColDoubleMaybe v -> primNullable encodePlainDouble (0 :: Double) v acc
  ColBoolMaybe v -> primNullableBoxed encodePlainBool False v acc
  ColDate32Maybe v -> primNullable encodeInt32s (0 :: Int32) v acc
  ColDate64Maybe v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColTime32Maybe v -> primNullable encodeInt32s (0 :: Int32) v acc
  ColTime64Maybe v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColTimestampMaybe v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColDurationMaybe v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColDecimal128Maybe _ _ v ->
    primNullableBoxed (encodePlainFixedSizeBinary 16) (BS.replicate 16 0) v acc
  ColDecimal256Maybe _ _ v ->
    primNullableBoxed (encodePlainFixedSizeBinary 32) (BS.replicate 32 0) v acc
  ColIntervalYearMonthMaybe v -> primNullable encodeIntervalYearMonth (0 :: Int32) v acc
  ColIntervalDayTimeMaybe v ->
    primNullableBoxed
      (\xs -> encodeIntervalDayTime (VP.convert (V.map fst xs)) (VP.convert (V.map snd xs)))
      (0, 0)
      v
      acc
  ColIntervalMonthDayNanoMaybe v ->
    primNullableBoxed
      ( \xs ->
          encodeIntervalMonthDayNano
            (VP.convert (V.map (\(m, _, _) -> m) xs))
            (VP.convert (V.map (\(_, d, _) -> d) xs))
            (VP.convert (V.map (\(_, _, ns) -> ns) xs))
      )
      (0, 0, 0)
      v
      acc
  -- Nullable variable-length + fixed-size binary columns.
  ColUtf8Maybe v ->
    varNullableBoxed encodePlainUtf8 T.empty v acc
  ColBinaryMaybe v ->
    varNullableBoxed encodePlainBinary BS.empty v acc
  ColLargeUtf8Maybe v ->
    varNullableBoxed encodePlainLargeUtf8 T.empty v acc
  ColLargeBinaryMaybe v ->
    varNullableBoxed encodePlainLargeBinary BS.empty v acc
  ColFixedSizeBinaryMaybe w v ->
    primNullableBoxed (encodePlainFixedSizeBinary w) (BS.replicate w 0) v acc
  -- ============================================================
  -- Nested columns
  -- ============================================================
  ColStruct n children ->
    let acc1 = addFieldNode (fromIntegral n) 0 acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i (_, cc) -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children
  ColStructMaybe validity children ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i (_, cc) -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children
  ColList offsets child ->
    let !n = fromIntegral (max 0 (VP.length offsets - 1)) :: Int64
        acc1 = addBufData (encodePlainInt32Column offsets) $ addFieldNode n 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  ColListMaybe validity offsets child ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 =
          addBufData (encodePlainInt32Column offsets) $
            addBufData (encodeNullBitmap validity) $
              addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  ColLargeList offsets child ->
    let !n = fromIntegral (max 0 (VP.length offsets - 1)) :: Int64
        acc1 = addBufData (encodePlainInt64Offsets offsets) $ addFieldNode n 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  ColLargeListMaybe validity offsets child ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 =
          addBufData (encodePlainInt64Offsets offsets) $
            addBufData (encodeNullBitmap validity) $
              addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  -- FixedSizeList has no offsets buffer: the length is implicit in
  -- the schema's FixedSizeList type, and the child array is exactly
  -- @parentLen * size@ long. We emit a single FieldNode for the
  -- parent then recurse into the child.
  ColFixedSizeList _ n child ->
    let acc1 = addFieldNode (fromIntegral n) 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  ColFixedSizeListMaybe _ validity child ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  ColMap offsets keyChild valChild ->
    let !n = fromIntegral (max 0 (VP.length offsets - 1)) :: Int64
        acc1 = addBufData (encodePlainInt32Column offsets) $ addFieldNode n 0 acc
        -- Map child is a single struct field; the struct itself has
        -- one FieldNode + (keys, values) sub-fields.
        structField = childFieldAt f 0
        keyField = childFieldAt structField 0
        valField = childFieldAt structField 1
        !structLen = fromIntegral (columnLength keyChild) :: Int64
        acc2 = addFieldNode structLen 0 acc1
        acc3 = encodeCol keyField keyChild acc2
    in encodeCol valField valChild acc3
  ColMapMaybe validity offsets keyChild valChild ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 =
          addBufData (encodePlainInt32Column offsets) $
            addBufData (encodeNullBitmap validity) $
              addFieldNode n nc acc
        structField = childFieldAt f 0
        keyField = childFieldAt structField 0
        valField = childFieldAt structField 1
        !structLen = fromIntegral (columnLength keyChild) :: Int64
        acc2 = addFieldNode structLen 0 acc1
        acc3 = encodeCol keyField keyChild acc2
    in encodeCol valField valChild acc3
  -- Union columns don't carry a top-level validity bitmap (nulls are
  -- represented via the child arrays + the type_ids buffer). The
  -- column holds child indices; the wire carries the field's type ids.
  ColDenseUnion typeIds offsets children ->
    let !n = fromIntegral (VP.length typeIds) :: Int64
        acc1 =
          addBufData (encodePlainInt32Column offsets) $
            addBufData (encodeInt8s (unionWireTypeIds f typeIds)) $
              addFieldNode n 0 acc
    in V.ifoldl' (\a i cc -> encodeCol (childFieldAt f i) cc a) acc1 children
  ColSparseUnion typeIds children ->
    let !n = fromIntegral (VP.length typeIds) :: Int64
        acc1 = addBufData (encodeInt8s (unionWireTypeIds f typeIds)) $ addFieldNode n 0 acc
    in V.ifoldl' (\a i cc -> encodeCol (childFieldAt f i) cc a) acc1 children
  -- Dictionary-encoded columns emit the indices at the field's index
  -- width; the dictionary itself lives in a separate DictionaryBatch
  -- message written by the stream / file writer.
  ColDictionary _dictId indices _dictValues ->
    primFlat (encodeDictIndices f indices) (VP.length indices) acc
  ColDictionaryMaybe _dictId indices _dictValues ->
    let !n = fromIntegral (V.length indices) :: Int64
        !nc = fromIntegral (countNulls indices) :: Int64
        dense = VP.generate (V.length indices) (fromMaybe 0 . V.unsafeIndex indices)
    in addBufData (encodeDictIndices f dense) $
         addBufData (encodeNullBitmap (V.map isJust indices)) $
           addFieldNode n nc acc
  -- RunEndEncoded: parent emits ZERO buffers (no validity, no data)
  -- and one FieldNode for itself; the run_ends and values children
  -- carry their own buffers and field nodes via recursive encodeCol.
  ColRunEndEncoded runEnds values ->
    let !logicalLen = fromIntegral (columnLength (ColRunEndEncoded runEnds values)) :: Int64
        acc1 = addFieldNode logicalLen 0 acc
        runEndsField = childFieldAt f 0
        valuesField = childFieldAt f 1
        acc2 = encodeCol runEndsField runEnds acc1
    in encodeCol valuesField values acc2
  -- ListView: validity (when nullable), offsets (i32), sizes (i32),
  -- then the child column.
  ColListView offsets sizes child ->
    let !n = fromIntegral (VP.length offsets) :: Int64
        acc1 =
          addBufData (encodePlainInt32Column sizes) $
            addBufData (encodePlainInt32Column offsets) $
              addFieldNode n 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  ColListViewMaybe validity offsets sizes child ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 =
          addBufData (encodePlainInt32Column sizes) $
            addBufData (encodePlainInt32Column offsets) $
              addBufData (encodeNullBitmap validity) $
                addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  ColLargeListView offsets sizes child ->
    let !n = fromIntegral (VP.length offsets) :: Int64
        acc1 =
          addBufData (encodePlainInt64Offsets sizes) $
            addBufData (encodePlainInt64Offsets offsets) $
              addFieldNode n 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  ColLargeListViewMaybe validity offsets sizes child ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 =
          addBufData (encodePlainInt64Offsets sizes) $
            addBufData (encodePlainInt64Offsets offsets) $
              addBufData (encodeNullBitmap validity) $
                addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  -- Utf8View / BinaryView: 2 buffers (validity + view) plus zero
  -- variadic data buffers. We INLINE every payload — if any value
  -- exceeds 12 bytes we fall back to using one variadic buffer
  -- holding all out-of-line payloads concatenated.
  ColUtf8View vs ->
    encodeViewColumn (V.map (Just . TE.encodeUtf8) vs) False acc
  ColUtf8ViewMaybe vs ->
    encodeViewColumn (V.map (fmap TE.encodeUtf8) vs) True acc
  ColBinaryView vs ->
    encodeViewColumn (V.map Just vs) False acc
  ColBinaryViewMaybe vs ->
    encodeViewColumn vs True acc
  -- ANull column: just emits a field-node with the row count;
  -- no buffers, no validity bitmap (per spec).
  ColNull n -> addFieldNode (fromIntegral n) (fromIntegral n) acc


-- ============================================================
-- Helpers
-- ============================================================

{- | Pick the i-th child field, defaulting to the parent on an out-of-
range index (which should never happen for a well-formed schema,
but we'd rather produce a degenerate record batch than crash).
-}
childFieldAt :: Field -> Int -> Field
childFieldAt f i =
  let !cs = fieldChildren f
  in if i < V.length cs then V.unsafeIndex cs i else f


{- | Map per-row child indices to the union field's wire type ids
(@AUnion _ ids@; an empty id list means the type id is the child
index).
-}
unionWireTypeIds :: Field -> VP.Vector Int8 -> VP.Vector Int8
unionWireTypeIds f childIdx = case fieldType f of
  AUnion _ ids
    | not (V.null ids) ->
        VP.map (\k -> maybe k fromIntegral (ids V.!? fromIntegral k)) childIdx
  _ -> childIdx


{- | Dictionary indices at the width of the field's index type
(@deIndexType@, default signed 32-bit). Indices are non-negative, so
signed and unsigned types share the encoding.
-}
encodeDictIndices :: Field -> VP.Vector Int32 -> ByteString
encodeDictIndices f ix = case deIndexType <$> fieldDictionary f of
  Just (AInt 8 _) -> encodeInt8s (VP.map fromIntegral ix)
  Just (AInt 16 _) -> encodeInt16s (VP.map fromIntegral ix)
  Just (AInt 64 _) -> encodeInt64s (VP.map fromIntegral ix)
  _ -> encodeInt32s ix


primFlat :: ByteString -> Int -> BuildAcc -> BuildAcc
primFlat bs n acc =
  addBufData bs (addFieldNode (fromIntegral n) 0 acc)


{- | Encode one Utf8View / BinaryView column. Each row is laid out
as a 16-byte view struct; payloads <= 12 bytes are stored
inline, longer payloads are pooled into a single variadic data
buffer.

Buffers emitted (in order, append-to-acc semantics): variadic
data buffer (only when needed), view buffer, validity bitmap
(when @nullable@). 'addBufData' prepends to the buffer list, so
we add validity LAST to match the spec order
@[validity, view, ...variadic]@.
-}
encodeViewColumn :: V.Vector (Maybe ByteString) -> Bool -> BuildAcc -> BuildAcc
encodeViewColumn vs nullable acc =
  let !n = V.length vs
      (variadicLen, viewBytes) = buildViews vs
      hasVariadic = variadicLen > 0
      validity = V.map (maybe False (const True)) vs
      !nc =
        if nullable
          then fromIntegral (V.foldl' (\c m -> if isJust m then c else c + 1) (0 :: Int) vs) :: Int64
          else 0
      -- Emission order = final buffer-list order. Final order:
      --   [validity (if nullable), view, variadic (if needed)].
      acc1 = addFieldNode (fromIntegral n) nc acc
      acc2 =
        if nullable
          then addBufData (encodeNullBitmap validity) acc1
          else acc1
      acc3 = addBufData viewBytes acc2
      acc4 =
        if hasVariadic
          then addBufData (variadicPayload vs) acc3
          else acc3
  in addVariadicCount (fromIntegral (if hasVariadic then 1 else 0 :: Int)) acc4


{- | Pack each row into its 16-byte view struct + collect the
variadic-buffer payload.  Returns @(variadicTotalLen, viewBytes)@
where @viewBytes@ has length @n*16@.
-}
buildViews :: V.Vector (Maybe ByteString) -> (Int, ByteString)
buildViews vs =
  let go (!off, accB) (Just bs)
        | BS.length bs <= 12 =
            let !len = BS.length bs
                !padded = bs <> BS.replicate (12 - len) 0
                !rec =
                  BL.toStrict
                    ( B.toLazyByteString $
                        B.int32LE (fromIntegral len) <> B.byteString padded
                    )
            in (off, accB <> B.byteString rec)
        | otherwise =
            let !len = BS.length bs
                !prefix = BS.take 4 bs
                !padPrefix = prefix <> BS.replicate (4 - BS.length prefix) 0
                !rec =
                  BL.toStrict
                    ( B.toLazyByteString $
                        B.int32LE (fromIntegral len)
                          <> B.byteString padPrefix
                          <> B.int32LE 0 -- buffer index
                          <> B.int32LE (fromIntegral off)
                    )
            in (off + len, accB <> B.byteString rec)
      go (!off, accB) Nothing =
        -- Null view: 16 zero bytes.
        (off, accB <> B.byteString (BS.replicate 16 0))
      (variadicLen, builder) = V.foldl' go (0 :: Int, mempty) vs
  in ( variadicLen
     , BL.toStrict (B.toLazyByteString builder)
     )


{- | Concatenated variadic-buffer payload for the rows that need
out-of-line storage.
-}
variadicPayload :: V.Vector (Maybe ByteString) -> ByteString
variadicPayload vs =
  BL.toStrict $
    B.toLazyByteString $
      V.foldl'
        ( \acc m -> case m of
            Just bs | BS.length bs > 12 -> acc <> B.byteString bs
            _ -> acc
        )
        mempty
        vs


varFlat :: ByteString -> ByteString -> Int -> BuildAcc -> BuildAcc
varFlat offBs datBs n acc =
  addBufData datBs $
    addBufData offBs $
      addFieldNode (fromIntegral n) 0 acc


{- | Encode a @V.Vector (Maybe a)@ for an @Unboxed/Primitive a@ value
payload. Builds a validity bitmap + a dense payload (nulls filled
with the caller-supplied zero).
-}
primNullable
  :: VP.Prim a
  => (VP.Vector a -> ByteString)
  -> a
  -> V.Vector (Maybe a)
  -> BuildAcc
  -> BuildAcc
primNullable enc zero vec acc =
  let !n = fromIntegral (V.length vec) :: Int64
      !nc = fromIntegral (countNulls vec) :: Int64
      validity = V.map isJust vec
      vals = VP.generate (V.length vec) $ \i ->
        fromMaybe zero (V.unsafeIndex vec i)
  in addBufData (enc vals) $
       addBufData (encodeNullBitmap validity) $
         addFieldNode n nc acc


{- | Encode a @V.Vector (Maybe a)@ backed by 'V.Vector' (i.e. not
primitive — 'Bool' / 'ByteString' / 'Text'). The 'V.Vector'-side
variant can't reuse 'primNullable' because the payload lives in a
boxed vector.
-}
primNullableBoxed
  :: (V.Vector a -> ByteString)
  -> a
  -> V.Vector (Maybe a)
  -> BuildAcc
  -> BuildAcc
primNullableBoxed enc zero vec acc =
  let !n = fromIntegral (V.length vec) :: Int64
      !nc = fromIntegral (countNulls vec) :: Int64
      validity = V.map isJust vec
      vals = V.map (fromMaybe zero) vec
  in addBufData (enc vals) $
       addBufData (encodeNullBitmap validity) $
         addFieldNode n nc acc


{- | Encode a @V.Vector (Maybe a)@ that serialises as an
(offsets, data) pair (Utf8 / Binary / LargeUtf8 / LargeBinary).
-}
varNullableBoxed
  :: (V.Vector a -> (ByteString, ByteString))
  -> a
  -> V.Vector (Maybe a)
  -> BuildAcc
  -> BuildAcc
varNullableBoxed enc zero vec acc =
  let !n = fromIntegral (V.length vec) :: Int64
      !nc = fromIntegral (countNulls vec) :: Int64
      validity = V.map isJust vec
      vals = V.map (fromMaybe zero) vec
      (offBs, datBs) = enc vals
  in addBufData datBs $
       addBufData offBs $
         addBufData (encodeNullBitmap validity) $
           addFieldNode n nc acc


-- | Count @False@ entries in a validity bitmap.
validityNullCount :: V.Vector Bool -> Int64
validityNullCount = V.foldl' (\c v -> if v then c else c + 1) 0

