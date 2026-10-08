{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{- | Arrow IPC record batch layout: the plan pass that turns a batch of
'ColumnArray's into Arrow field nodes and body pieces (spec layout,
depth-first pre-order, a validity slot for every array that has one).

Pieces alias the column buffers. The only copies made here normalise
sliced columns, once per buffer and only when needed: offsets that do
not start at 0 are rebased, and bitmaps whose bit offset is not a
multiple of 8 are re-aligned. Var-length data and list children are
trimmed to the range the offsets reference, struct and sparse union
children to the parent's rows.

The message framing lives in "Arrow.FlatBufferIPC.Write"; the
high-level stream and file writers are in "Arrow.Stream" and
"Arrow.Write".
-}
module Arrow.Write.Columns (
  -- * Schema agreement
  validateColumns,

  -- * Layout
  BatchPlan (..),
  planBatch,
) where

import Arrow.Column (
  columnLength,
  columnTag,
  nullCount,
  rebaseRunEnds,
  sliceColumnArray,
 )
import Arrow.Column.Internal (
  Bitmap (..),
  ChildRange (..),
  ColumnArray,
  IntegralPrim (..),
  Offset,
  PrimType,
  SomePrimType (..),
  Validity (..),
  copyBitmap,
  integralPrim,
  primTypeFor,
  primWidth,
  rebaseToZero,
  samePrimType,
  storableToBytes,
  withPrim,
 )
import Arrow.Column.Internal qualified as I
import Arrow.Types
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS


-- ============================================================
-- Plan
-- ============================================================

{- | One record batch laid out for the wire: field nodes, one body
piece per 'Buffer' slot (unpadded; the framing pads each to 8 bytes)
and the variadic buffer counts of view columns, all in schema
pre-order.
-}
data BatchPlan = BatchPlan
  { bpLength :: !Int64
  , bpNodes :: !(VS.Vector FieldNode)
  , bpPieces :: !(V.Vector ByteString)
  , bpVariadic :: !(V.Vector Int64)
  }


-- | Reversed field nodes, pieces and variadic counts, with the first two lengths.
data Acc = Acc ![FieldNode] {-# UNPACK #-} !Int ![ByteString] {-# UNPACK #-} !Int ![Int64]


{- | Lay out a batch whose columns agree with the fields (see
'validateColumns'; this function only checks what it needs to stay
total). O(columns + buffers) apart from the slice normalisation
described in the module header.
-}
planBatch :: V.Vector Field -> V.Vector ColumnArray -> Either String BatchPlan
planBatch fields cols
  | V.length fields /= V.length cols = Left "Arrow.Write: column count differs from the schema's field count"
  | otherwise = do
      Acc ns nn ps np vs <- V.ifoldM' (\acc i f -> planCol f (V.unsafeIndex cols i) acc) (Acc [] 0 [] 0 []) fields
      Right
        BatchPlan
          { bpLength = if V.null cols then 0 else fromIntegral (columnLength (V.unsafeHead cols))
          , bpNodes = VS.fromListN nn (reverse ns)
          , bpPieces = V.fromListN np (reverse ps)
          , bpVariadic = V.fromList (reverse vs)
          }


node :: Int -> Int -> Acc -> Acc
node len nc (Acc ns nn ps np vs) = Acc (FieldNode (fromIntegral len) (fromIntegral nc) : ns) (nn + 1) ps np vs
{-# INLINE node #-}


piece :: ByteString -> Acc -> Acc
piece p (Acc ns nn ps np vs) = Acc ns nn (p : ps) (np + 1) vs
{-# INLINE piece #-}


variadic :: Int -> Acc -> Acc
variadic c (Acc ns nn ps np vs) = Acc ns nn ps np (fromIntegral c : vs)


-- | The node of an array that has a validity slot, followed by its validity piece.
withValidity :: ColumnArray -> Maybe Validity -> Acc -> Acc
withValidity col v acc = validityPiece v (node (columnLength col) (nullCount col) acc)


-- | The validity piece; a validity without nulls is written like 'Nothing' (an empty slot).
validityPiece :: Maybe Validity -> Acc -> Acc
validityPiece = \case
  Just (Validity bm nc) | nc > 0 -> piece (bitmapPiece bm)
  _ -> piece BS.empty


{- | The bytes of a bitmap starting at bit 0: an alias when the bit
offset is byte aligned, else one bit-copy.
-}
bitmapPiece :: Bitmap -> ByteString
bitmapPiece bm@(Bitmap bs off len)
  | off .&. 7 == 0 = BS.take nbytes (BS.drop (off `shiftR` 3) bs)
  | otherwise = I.bitmapBytes (copyBitmap bm)
  where
    !nbytes = (len + 7) `shiftR` 3


-- | Offsets starting at 0 (aliased when they already do) and the child range they cover.
offsetsPiece :: Offset o => VS.Vector o -> (ByteString, ChildRange)
offsetsPiece o
  | not (VS.null o) && VS.unsafeHead o == 0 = (storableToBytes o, ChildRange 0 (fromIntegral (VS.unsafeLast o)))
  | otherwise = let !(o', r) = rebaseToZero o in (storableToBytes o', r)


sliceRange :: ChildRange -> ColumnArray -> ColumnArray
sliceRange (ChildRange s l) = sliceColumnArray s l


bytesRange :: ChildRange -> ByteString -> ByteString
bytesRange (ChildRange s l) = BS.take l . BS.drop s


childField :: Field -> Int -> Either String Field
childField f i = case fieldChildren f V.!? i of
  Just c -> Right c
  Nothing -> Left ("Arrow.Write: field " ++ show (fieldName f) ++ " has no child " ++ show i)


planCol :: Field -> ColumnArray -> Acc -> Either String Acc
planCol f col acc = case col of
  I.ColNull n -> Right (node n n acc)
  I.ColPrim t v xs -> Right (piece (withPrim t (storableToBytes xs)) (withValidity col v acc))
  I.ColBool v bm -> Right (piece (bitmapPiece bm) (withValidity col v acc))
  I.ColUtf8 v o d -> Right (var v o d)
  I.ColBinary v o d -> Right (var v o d)
  I.ColLargeUtf8 v o d -> Right (var v o d)
  I.ColLargeBinary v o d -> Right (var v o d)
  I.ColFixedSizeBinary w n v d -> Right (piece (BS.take (w * n) d) (withValidity col v acc))
  I.ColUtf8View v views bufs -> Right (view v views bufs)
  I.ColBinaryView v views bufs -> Right (view v views bufs)
  I.ColStruct n v cs ->
    V.ifoldM'
      (\a i (_, c) -> childField f i >>= \cf -> planCol cf (sliceColumnArray 0 n c) a)
      (withValidity col v acc)
      cs
  I.ColList v o c -> list v o c
  I.ColLargeList v o c -> list v o c
  I.ColListView v o s c -> do
    cf <- childField f 0
    planCol cf c (piece (storableToBytes s) (piece (storableToBytes o) (withValidity col v acc)))
  I.ColLargeListView v o s c -> do
    cf <- childField f 0
    planCol cf c (piece (storableToBytes s) (piece (storableToBytes o) (withValidity col v acc)))
  I.ColFixedSizeList w n v c -> do
    cf <- childField f 0
    planCol cf (sliceColumnArray 0 (n * w) c) (withValidity col v acc)
  I.ColMap v o k x -> do
    entries <- childField f 0
    kf <- childField entries 0
    xf <- childField entries 1
    let !(offs, r) = offsetsPiece o
        !acc1 = piece BS.empty (node (childLength r) 0 (piece offs (withValidity col v acc)))
    planCol kf (sliceRange r k) acc1 >>= planCol xf (sliceRange r x)
  I.ColDenseUnion t o cs ->
    let !acc1 = piece (storableToBytes o) (piece (unionTypes f t) (node (VS.length t) 0 acc))
    in V.ifoldM' (\a i c -> childField f i >>= \cf -> planCol cf c a) acc1 cs
  I.ColSparseUnion t cs ->
    let !n = VS.length t
        !acc1 = piece (unionTypes f t) (node n 0 acc)
    in V.ifoldM' (\a i c -> childField f i >>= \cf -> planCol cf (sliceColumnArray 0 n c) a) acc1 cs
  I.ColDictionary _ keys _ -> case keys of
    I.ColPrim kt v ks -> do
      bytes <- keysPiece f kt ks
      Right (piece bytes (withValidity col v acc))
    _ -> Left ("Arrow.Write: dictionary " ++ show (fieldName f) ++ " has non-integer keys " ++ columnTag keys)
  I.ColRunEndEncoded {} ->
    rebaseRunEnds col >>= \case
      I.ColRunEndEncoded _ n re vals -> do
        rf <- childField f 0
        vf <- childField f 1
        planCol rf re (node n 0 acc) >>= planCol vf vals
      other -> Left ("Arrow.Write: rebaseRunEnds returned " ++ columnTag other)
  where
    var :: Offset o => Maybe Validity -> VS.Vector o -> ByteString -> Acc
    var v o d =
      let !(offs, r) = offsetsPiece o
      in piece (bytesRange r d) (piece offs (withValidity col v acc))
    list :: Offset o => Maybe Validity -> VS.Vector o -> ColumnArray -> Either String Acc
    list v o c = do
      cf <- childField f 0
      let !(offs, r) = offsetsPiece o
      planCol cf (sliceRange r c) (piece offs (withValidity col v acc))
    view v views bufs =
      let !acc1 = piece (BS.take (16 * columnLength col) views) (withValidity col v acc)
      in variadic (V.length bufs) (V.foldl' (flip piece) acc1 bufs)


{- | Union type ids on the wire: the column holds child positions, the
field may declare other type ids (@AUnion _ ids@). Aliased when the ids
are positional (or absent).
-}
unionTypes :: Field -> VS.Vector Int8 -> ByteString
unionTypes f t = case fieldType f of
  AUnion _ ids
    | not (V.and (V.imap (\i x -> fromIntegral i == x) ids)) ->
        storableToBytes (VS.map (\k -> maybe k fromIntegral (ids V.!? fromIntegral k)) t)
  _ -> storableToBytes t


{- | Dictionary keys at the width of the field's index type (signed
32-bit when it declares none). Keys already at that width are aliased;
others are converted in one pass. Valid keys are below the dictionary
length, which the stream writer checks against the index type, so the
conversion never truncates a valid key.
-}
keysPiece :: Field -> PrimType a -> VS.Vector a -> Either String ByteString
keysPiece f kt ks = case integralPrim kt of
  Nothing -> Left ("Arrow.Write: dictionary " ++ show (fieldName f) ++ " keys are not integers")
  Just IntegralPrim
    | width == primWidth kt -> Right (storableToBytes ks)
    | otherwise -> Right $ case width of
        1 -> storableToBytes (VS.map fromIntegral ks :: VS.Vector Int8)
        2 -> storableToBytes (VS.map fromIntegral ks :: VS.Vector Int16)
        8 -> storableToBytes (VS.map fromIntegral ks :: VS.Vector Int64)
        _ -> storableToBytes (VS.map fromIntegral ks :: VS.Vector Int32)
  where
    !width = case deIndexType <$> fieldDictionary f of
      Just (AInt bits _) | bits == 8 || bits == 16 || bits == 64 -> bits `quot` 8
      _ -> 4


-- ============================================================
-- Schema agreement
-- ============================================================

{- | Check that a batch agrees with the schema fields before it is
encoded: one column per field, every column with the same row count,
and every column (recursively) of its field's shape:

* the column type matches the field type (integer width and
  signedness, float precision, date / time / interval unit class,
  decimal precision and scale, fixed widths, list size), and a
  dictionary-encoded field holds a 'ColDictionary' with the field's
  dictionary id, integer keys, and values that match the value type;
* a column with nulls sits under a nullable field (dictionary rows
  count through their keys; nulls in rows the parent does not
  reference are ignored);
* nested columns have one child per child field, struct children have
  at least the struct's row count, a fixed-size list child has at
  least @rows * size@ elements, list and map offsets lie inside the
  child, list view offsets and sizes have the row count, and sparse
  union children cover every row.

Only shapes and lengths are inspected: O(columns), no per-row work.
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
    I.ColDictionary did keys vals
      | did /= deId de -> bad ("dictionary id " ++ show did ++ ", the field declares " ++ show (deId de))
      | not (integerKeys keys) -> bad ("dictionary keys must be an integer column, not " ++ columnTag keys)
      | nullCount keys > 0 && not (fieldNullable f) -> bad "dictionary column with null rows under a non-nullable field"
      | otherwise -> checkField path0 f {fieldDictionary = Nothing, fieldNullable = True} vals
    _ -> mismatch
  Nothing
    | hasValidity && not (fieldNullable f) && nullCount col > 0 ->
        bad (columnTag col ++ " column with " ++ show (nullCount col) ++ " nulls under a non-nullable field")
    | otherwise -> shape
  where
    path = fieldName f : path0
    bad msg = Left ("Arrow.Write: column " ++ show (T.intercalate "." (reverse path)) ++ ": " ++ msg)
    mismatch = bad ("field type " ++ show (fieldType f) ++ " cannot hold a " ++ columnTag col ++ " column")
    ok = Right ()
    expect cond msg = if cond then ok else bad msg
    hasValidity = case fieldType f of
      ANull -> False
      AUnion _ _ -> False
      ARunEndEncoded -> False
      _ -> True
    kids = fieldChildren f
    rows = columnLength col
    integerKeys = \case
      I.ColPrim kt _ _ -> isJust (integralPrim kt)
      _ -> False
    one k = case V.toList kids of
      [c] -> k c
      _ -> bad (show (fieldType f) ++ " field must have exactly one child")
    sizeIs what n m = expect (n == m) (what ++ " has " ++ show m ++ " entries for " ++ show n ++ " rows")
    -- Offsets inside the child; the referenced child range.
    offsets :: Offset o => VS.Vector o -> Int -> Either String ChildRange
    offsets offs childLen
      | VS.null offs = Left "offsets are empty (a list needs rows + 1 offsets)"
      | VS.head offs < 0 = Left "offsets start below zero"
      | VS.last offs < VS.head offs = Left "offsets end before they start"
      | toInteger (VS.last offs) > toInteger childLen =
          Left ("offsets end at " ++ show (toInteger (VS.last offs)) ++ ", the child has " ++ show childLen ++ " rows")
      | otherwise = Right (ChildRange (fromIntegral (VS.head offs)) (fromIntegral (VS.last offs - VS.head offs)))
    withOffsets :: Offset o => VS.Vector o -> Int -> (ChildRange -> Either String ()) -> Either String ()
    withOffsets offs childLen k = either bad k (offsets offs childLen)
    listOf :: Offset o => VS.Vector o -> ColumnArray -> Either String ()
    listOf offs c = one $ \cf -> withOffsets offs (columnLength c) $ \r -> checkField path cf (sliceRange r c)
    varOf :: Offset o => VS.Vector o -> ByteString -> Either String ()
    varOf offs d = withOffsets offs (BS.length d) (const ok)
    viewOf :: Offset o => VS.Vector o -> VS.Vector o -> ColumnArray -> Either String ()
    viewOf offs sizes c = one $ \cf -> do
      sizeIs "list view offsets" rows (VS.length offs)
      sizeIs "list view sizes" rows (VS.length sizes)
      checkField path cf c
    struct n cs
      | V.length cs /= V.length kids =
          bad ("struct column has " ++ show (V.length cs) ++ " children, the field has " ++ show (V.length kids))
      | otherwise =
          V.zipWithM_
            ( \cf (_, c) -> do
                expect
                  (columnLength c >= n)
                  ("struct child " ++ show (fieldName cf) ++ " has " ++ show (columnLength c) ++ " rows, the struct has " ++ show n)
                checkField path cf (sliceColumnArray 0 n c)
            )
            kids
            cs
    fixed w w' n c
      | w /= w' = mismatch
      | otherwise = one $ \cf -> do
          expect
            (columnLength c >= n * w)
            ("fixed-size list child has " ++ show (columnLength c) ++ " elements, " ++ show n ++ " rows of size " ++ show w ++ " need " ++ show (n * w))
          checkField path cf (sliceColumnArray 0 (n * w) c)
    mapOf offs k v = case V.toList kids of
      [entries] | [kf, vf] <- V.toList (fieldChildren entries) -> do
        expect (columnLength k == columnLength v) ("map keys have " ++ show (columnLength k) ++ " rows, values " ++ show (columnLength v))
        withOffsets offs (columnLength k) $ \r -> do
          checkField (fieldName entries : path) kf (sliceRange r k)
          checkField (fieldName entries : path) vf (sliceRange r v)
      _ -> bad "map field must have one entries struct child with key and value"
    union cs check
      | V.length cs /= V.length kids =
          bad ("union column has " ++ show (V.length cs) ++ " children, the field has " ++ show (V.length kids))
      | otherwise = V.zipWithM_ check kids cs
    prim :: PrimType a -> Either String ()
    prim t = case primTypeFor (fieldType f) of
      Just (SomePrimType et)
        | samePrimType t et -> ok
        | otherwise -> mismatch
      Nothing -> mismatch
    shape = case (fieldType f, col) of
      (ANull, I.ColNull _) -> ok
      (_, I.ColPrim t _ _) -> prim t
      (ABool, I.ColBool {}) -> ok
      (AUtf8, I.ColUtf8 _ o d) -> varOf o d
      (ABinary, I.ColBinary _ o d) -> varOf o d
      (ALargeUtf8, I.ColLargeUtf8 _ o d) -> varOf o d
      (ALargeBinary, I.ColLargeBinary _ o d) -> varOf o d
      (AUtf8View, I.ColUtf8View _ vs _) -> expect (BS.length vs >= 16 * rows) "views buffer shorter than 16 bytes per row"
      (ABinaryView, I.ColBinaryView _ vs _) -> expect (BS.length vs >= 16 * rows) "views buffer shorter than 16 bytes per row"
      (AFixedSizeBinary w, I.ColFixedSizeBinary w' n _ d) -> do
        expect (w == w') ("fixed-size binary width " ++ show w' ++ ", the field declares " ++ show w)
        expect (BS.length d >= w * n) ("fixed-size binary data has " ++ show (BS.length d) ++ " bytes for " ++ show n ++ " rows")
      (AStruct, I.ColStruct n _ cs) -> struct n cs
      (AList, I.ColList _ o c) -> listOf o c
      (ALargeList, I.ColLargeList _ o c) -> listOf o c
      (AFixedSizeList w, I.ColFixedSizeList w' n _ c) -> fixed w w' n c
      (AMap _, I.ColMap _ o k v) -> mapOf o k v
      (AListView, I.ColListView _ o s c) -> viewOf o s c
      (ALargeListView, I.ColLargeListView _ o s c) -> viewOf o s c
      (AUnion Dense _, I.ColDenseUnion ts offs cs) -> do
        sizeIs "dense union offsets" (VS.length ts) (VS.length offs)
        union cs (checkField path)
      (AUnion Sparse _, I.ColSparseUnion ts cs) ->
        union cs $ \cf c -> do
          expect
            (columnLength c >= VS.length ts)
            ("sparse union child " ++ show (fieldName cf) ++ " has " ++ show (columnLength c) ++ " rows, the union has " ++ show (VS.length ts))
          checkField path cf (sliceColumnArray 0 (VS.length ts) c)
      (ARunEndEncoded, I.ColRunEndEncoded _ _ re vals) -> case V.toList kids of
        [rf, vf] -> do
          expect
            (columnLength vals >= columnLength re)
            ("run-end-encoded column has " ++ show (columnLength re) ++ " runs but " ++ show (columnLength vals) ++ " values")
          checkField path rf re
          checkField path vf vals
        _ -> bad "run-end-encoded field must have two children (run_ends, values)"
      _ -> mismatch
