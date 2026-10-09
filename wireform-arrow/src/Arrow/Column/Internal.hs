{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

{- | Raw representation of Arrow columns.

The data constructors here establish nothing: every invariant listed
on 'ColumnArray' must hold for each value built with them, because
the accessors in "Arrow.Column" read buffers without re-checking
(after an O(1) shape check and an index check). Only the IPC reader,
the writer, "Arrow.Column.Builder", the smart constructors in
"Arrow.Column" and malformed-input tests should import this module.
Everyone else uses the matching-only pattern synonyms, the @mk*@
constructors and the builders that "Arrow.Column" exports.

The low-level buffer types ('Bitmap', 'Validity' and the element
types) are re-exported with their constructors.
-}
module Arrow.Column.Internal (
  -- * Columns
  ColumnArray (..),
  columnBuffers,

  -- * Fixed-width element tags
  PrimType (..),
  withPrim,
  primWidth,
  primTypeName,
  samePrimTag,
  samePrimType,
  SomePrimType (..),
  primTypeFor,
  IntegralPrim (..),
  integralPrim,

  -- * Structural validators (shared by the reader and the smart constructors)
  Offset (..),
  validateOffsets,
  validateUtf8,
  validateKeys,
  validateRunEnds,
  validateListView,
  validateDenseUnion,
  validateSparseUnionTypes,
  validateViews,
  withValidityPtr,

  -- * Offsets
  ChildRange (..),
  rebaseToZero,

  -- * Buffers
  module Arrow.Column.Buffer,
) where

import Arrow.Column.Buffer
import Arrow.Types (ArrowType (..), DateUnit (..), IntervalUnit (..), Precision (..), TimeUnit (..))
import Arrow.Vector.Internal (FixedWidth)
import Columnar.SIMD qualified as K
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Type.Equality ((:~:) (..))
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import System.IO.Unsafe (unsafeDupablePerformIO)


{- | Element tag of a fixed-width column. Several tags share a Haskell
element type (dates, times, timestamps and durations are 'Int32' or
'Int64'), so the tag, not the type, says what the column is.
Decimal tags carry precision and scale.
-}
data PrimType a where
  PInt8 :: PrimType Int8
  PInt16 :: PrimType Int16
  PInt32 :: PrimType Int32
  PInt64 :: PrimType Int64
  PUInt8 :: PrimType Word8
  PUInt16 :: PrimType Word16
  PUInt32 :: PrimType Word32
  PUInt64 :: PrimType Word64
  PFloat16 :: PrimType Float16
  PFloat :: PrimType Float
  PDouble :: PrimType Double
  PDate32 :: PrimType Int32
  PDate64 :: PrimType Int64
  PTime32 :: PrimType Int32
  PTime64 :: PrimType Int64
  PTimestamp :: PrimType Int64
  PDuration :: PrimType Int64
  PIntervalYearMonth :: PrimType Int32
  PIntervalDayTime :: PrimType IntervalDayTime
  PIntervalMonthDayNano :: PrimType IntervalMonthDayNano
  PDecimal128 :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> PrimType Decimal128
  PDecimal256 :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> PrimType Decimal256


deriving stock instance Show (PrimType a)
deriving stock instance Eq (PrimType a)


{- | Bring the element type's instances into scope from the tag
('FixedWidth' includes 'Storable').
-}
withPrim :: PrimType a -> ((FixedWidth a, Eq a, Show a) => r) -> r
withPrim t k = case t of
  PInt8 -> k
  PInt16 -> k
  PInt32 -> k
  PInt64 -> k
  PUInt8 -> k
  PUInt16 -> k
  PUInt32 -> k
  PUInt64 -> k
  PFloat16 -> k
  PFloat -> k
  PDouble -> k
  PDate32 -> k
  PDate64 -> k
  PTime32 -> k
  PTime64 -> k
  PTimestamp -> k
  PDuration -> k
  PIntervalYearMonth -> k
  PIntervalDayTime -> k
  PIntervalMonthDayNano -> k
  PDecimal128 _ _ -> k
  PDecimal256 _ _ -> k
{-# INLINE withPrim #-}


-- | Element width in bytes.
primWidth :: PrimType a -> Int
primWidth = \case
  PInt8 -> 1
  PInt16 -> 2
  PInt32 -> 4
  PInt64 -> 8
  PUInt8 -> 1
  PUInt16 -> 2
  PUInt32 -> 4
  PUInt64 -> 8
  PFloat16 -> 2
  PFloat -> 4
  PDouble -> 8
  PDate32 -> 4
  PDate64 -> 8
  PTime32 -> 4
  PTime64 -> 8
  PTimestamp -> 8
  PDuration -> 8
  PIntervalYearMonth -> 4
  PIntervalDayTime -> 8
  PIntervalMonthDayNano -> 16
  PDecimal128 _ _ -> 16
  PDecimal256 _ _ -> 32
{-# INLINE primWidth #-}


-- | The tag's name without the @P@ (the column constructor is @Col@ ++ this).
primTypeName :: PrimType a -> String
primTypeName = \case
  PInt8 -> "Int8"
  PInt16 -> "Int16"
  PInt32 -> "Int32"
  PInt64 -> "Int64"
  PUInt8 -> "UInt8"
  PUInt16 -> "UInt16"
  PUInt32 -> "UInt32"
  PUInt64 -> "UInt64"
  PFloat16 -> "Float16"
  PFloat -> "Float"
  PDouble -> "Double"
  PDate32 -> "Date32"
  PDate64 -> "Date64"
  PTime32 -> "Time32"
  PTime64 -> "Time64"
  PTimestamp -> "Timestamp"
  PDuration -> "Duration"
  PIntervalYearMonth -> "IntervalYearMonth"
  PIntervalDayTime -> "IntervalDayTime"
  PIntervalMonthDayNano -> "IntervalMonthDayNano"
  PDecimal128 _ _ -> "Decimal128"
  PDecimal256 _ _ -> "Decimal256"


{- | Same tag (decimal precision and scale ignored), with the element
type equality as evidence.
-}
samePrimTag :: PrimType a -> PrimType b -> Maybe (a :~: b)
samePrimTag a b = case (a, b) of
  (PInt8, PInt8) -> Just Refl
  (PInt16, PInt16) -> Just Refl
  (PInt32, PInt32) -> Just Refl
  (PInt64, PInt64) -> Just Refl
  (PUInt8, PUInt8) -> Just Refl
  (PUInt16, PUInt16) -> Just Refl
  (PUInt32, PUInt32) -> Just Refl
  (PUInt64, PUInt64) -> Just Refl
  (PFloat16, PFloat16) -> Just Refl
  (PFloat, PFloat) -> Just Refl
  (PDouble, PDouble) -> Just Refl
  (PDate32, PDate32) -> Just Refl
  (PDate64, PDate64) -> Just Refl
  (PTime32, PTime32) -> Just Refl
  (PTime64, PTime64) -> Just Refl
  (PTimestamp, PTimestamp) -> Just Refl
  (PDuration, PDuration) -> Just Refl
  (PIntervalYearMonth, PIntervalYearMonth) -> Just Refl
  (PIntervalDayTime, PIntervalDayTime) -> Just Refl
  (PIntervalMonthDayNano, PIntervalMonthDayNano) -> Just Refl
  (PDecimal128 _ _, PDecimal128 _ _) -> Just Refl
  (PDecimal256 _ _, PDecimal256 _ _) -> Just Refl
  _ -> Nothing


-- | Same tag including decimal precision and scale.
samePrimType :: PrimType a -> PrimType b -> Bool
samePrimType a b = case (a, b) of
  (PDecimal128 p s, PDecimal128 p' s') -> p == p' && s == s'
  (PDecimal256 p s, PDecimal256 p' s') -> p == p' && s == s'
  _ -> case samePrimTag a b of
    Just Refl -> True
    Nothing -> False


data SomePrimType = forall a. SomePrimType !(PrimType a)


instance Show SomePrimType where
  showsPrec d (SomePrimType t) = showParen (d > 10) $ showString "SomePrimType " . showsPrec 11 t


{- | The fixed-width tag of an Arrow type, if it has one. Integer
widths other than 8, 16, 32 and 64 have none.
-}
primTypeFor :: ArrowType -> Maybe SomePrimType
primTypeFor = \case
  AInt 8 True -> some PInt8
  AInt 16 True -> some PInt16
  AInt 32 True -> some PInt32
  AInt 64 True -> some PInt64
  AInt 8 False -> some PUInt8
  AInt 16 False -> some PUInt16
  AInt 32 False -> some PUInt32
  AInt 64 False -> some PUInt64
  AFloatingPoint Half -> some PFloat16
  AFloatingPoint Single -> some PFloat
  AFloatingPoint DoublePrecision -> some PDouble
  ADate DateDay -> some PDate32
  ADate DateMillisecond -> some PDate64
  ATime u _
    | u == Second || u == Millisecond -> some PTime32
    | otherwise -> some PTime64
  ATimestamp _ _ -> some PTimestamp
  ADuration _ -> some PDuration
  AInterval YearMonth -> some PIntervalYearMonth
  AInterval DayTime -> some PIntervalDayTime
  AInterval MonthDayNano -> some PIntervalMonthDayNano
  ADecimal p s -> some (PDecimal128 p s)
  ADecimal256 p s -> some (PDecimal256 p s)
  _ -> Nothing
  where
    some :: PrimType a -> Maybe SomePrimType
    some = Just . SomePrimType


-- | Evidence that a tag is a plain integer (the dictionary key and run-end types).
data IntegralPrim a where
  IntegralPrim :: (Integral a, Bounded a, Storable a, Show a) => IntegralPrim a


-- | 'Just' for the eight integer tags only (not dates, times or intervals).
integralPrim :: PrimType a -> Maybe (IntegralPrim a)
integralPrim = \case
  PInt8 -> Just IntegralPrim
  PInt16 -> Just IntegralPrim
  PInt32 -> Just IntegralPrim
  PInt64 -> Just IntegralPrim
  PUInt8 -> Just IntegralPrim
  PUInt16 -> Just IntegralPrim
  PUInt32 -> Just IntegralPrim
  PUInt64 -> Just IntegralPrim
  _ -> Nothing
{-# INLINE integralPrim #-}


{- | One Arrow array. Buffers are Arrow-native and usually alias the
IPC input (see "Arrow.Column" for retention and 'Arrow.Column.copyColumn').

Invariants (not checked by these constructors):

* Validity, when present, has exactly as many bits as the array has
  rows and a null count > 0 that matches its bits ('Validity').
* 'ColPrim': rows = vector length.
* 'ColBool': rows = bitmap length.
* Var-length ('ColUtf8', 'ColBinary', 'ColLargeUtf8',
  'ColLargeBinary'): offsets length = rows + 1 (at least 1),
  @offs[0] >= 0@, monotonic, @offs[rows] <= length data@. Utf8 data in
  @[offs[0], offs[rows])@ is valid UTF-8 and every offset falls on a
  character boundary. Offsets need not start at 0.
* 'ColFixedSizeBinary' width rows: width >= 0, data length >= width * rows.
* Views: views length = 16 * rows; every valid view references
  in-range bytes of its data buffer with a matching prefix; utf8
  views hold valid UTF-8.
* 'ColStruct' rows: every child has at least rows rows (row @i@ of
  the struct is row @i@ of each child).
* Lists and maps: offsets as for var-length, against the child
  length (both keys and values for maps).
* List views: offsets and sizes have rows elements; every valid row
  has @offset >= 0@, @size >= 0@, @offset + size <= child length@.
* 'ColFixedSizeList' size rows: size >= 0, child length >= rows * size.
* Unions: one child index per row (an index into the children, not the
  schema type id), each below the number of children. Dense offsets
  index into the selected child. Sparse children have at least rows rows.
* 'ColDictionary' id indices values: indices is an integer 'ColPrim'
  (8 integer tags) at its wire width carrying the row validity; every
  valid key is in @[0, length values)@ once resolved (the reader holds
  unresolved dictionaries with an empty placeholder until
  'Arrow.Column.resolveDictionaryColumn').
* 'ColRunEndEncoded' offset length runEnds values: runEnds is a
  non-null 'ColPrim' of tag 'PInt16', 'PInt32' or 'PInt64', strictly
  increasing and positive; logical row @i@ is the value of the first
  run whose end is greater than @offset + i@; the last run end is at
  least @offset + length@; values has at least as many rows as runs.
-}
data ColumnArray
  = ColNull {-# UNPACK #-} !Int
  | forall a. ColPrim !(PrimType a) !(Maybe Validity) !(VS.Vector a)
  | ColBool !(Maybe Validity) !Bitmap
  | ColUtf8 !(Maybe Validity) !(VS.Vector Int32) !ByteString
  | ColBinary !(Maybe Validity) !(VS.Vector Int32) !ByteString
  | ColLargeUtf8 !(Maybe Validity) !(VS.Vector Int64) !ByteString
  | ColLargeBinary !(Maybe Validity) !(VS.Vector Int64) !ByteString
  | -- | Width, rows, validity, data.
    ColFixedSizeBinary {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(Maybe Validity) !ByteString
  | -- | Validity, the 16-byte views, the variadic data buffers.
    ColUtf8View !(Maybe Validity) !ByteString !(V.Vector ByteString)
  | ColBinaryView !(Maybe Validity) !ByteString !(V.Vector ByteString)
  | -- | Rows, validity, named children.
    ColStruct {-# UNPACK #-} !Int !(Maybe Validity) !(V.Vector (Text, ColumnArray))
  | ColList !(Maybe Validity) !(VS.Vector Int32) !ColumnArray
  | ColLargeList !(Maybe Validity) !(VS.Vector Int64) !ColumnArray
  | -- | Validity, offsets, sizes, child.
    ColListView !(Maybe Validity) !(VS.Vector Int32) !(VS.Vector Int32) !ColumnArray
  | ColLargeListView !(Maybe Validity) !(VS.Vector Int64) !(VS.Vector Int64) !ColumnArray
  | -- | List size, rows, validity, child.
    ColFixedSizeList {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(Maybe Validity) !ColumnArray
  | -- | Validity, offsets, keys, values.
    ColMap !(Maybe Validity) !(VS.Vector Int32) !ColumnArray !ColumnArray
  | -- | Child index per row, offset per row, children.
    ColDenseUnion !(VS.Vector Int8) !(VS.Vector Int32) !(V.Vector ColumnArray)
  | -- | Child index per row, children.
    ColSparseUnion !(VS.Vector Int8) !(V.Vector ColumnArray)
  | -- | Dictionary id, indices (integer 'ColPrim' with the row validity), values.
    ColDictionary {-# UNPACK #-} !Int64 !ColumnArray !ColumnArray
  | -- | Logical offset, logical length, run ends, values.
    ColRunEndEncoded {-# UNPACK #-} !Int {-# UNPACK #-} !Int !ColumnArray !ColumnArray


{- | Every byte region a column references (buffers, children,
dictionary values), for retention checks and tests. Regions are the
whole underlying buffers, not trimmed to the logical range.
-}
columnBuffers :: ColumnArray -> [ByteString]
columnBuffers = \case
  ColNull _ -> []
  ColPrim t v xs -> withPrim t (vb v ++ [storableToBytes xs])
  ColBool v b -> vb v ++ [bitmapBytes b]
  ColUtf8 v o d -> vb v ++ [storableToBytes o, d]
  ColBinary v o d -> vb v ++ [storableToBytes o, d]
  ColLargeUtf8 v o d -> vb v ++ [storableToBytes o, d]
  ColLargeBinary v o d -> vb v ++ [storableToBytes o, d]
  ColFixedSizeBinary _ _ v d -> vb v ++ [d]
  ColUtf8View v views bufs -> vb v ++ views : V.toList bufs
  ColBinaryView v views bufs -> vb v ++ views : V.toList bufs
  ColStruct _ v cs -> vb v ++ concatMap (columnBuffers . snd) (V.toList cs)
  ColList v o c -> vb v ++ storableToBytes o : columnBuffers c
  ColLargeList v o c -> vb v ++ storableToBytes o : columnBuffers c
  ColListView v o s c -> vb v ++ storableToBytes o : storableToBytes s : columnBuffers c
  ColLargeListView v o s c -> vb v ++ storableToBytes o : storableToBytes s : columnBuffers c
  ColFixedSizeList _ _ v c -> vb v ++ columnBuffers c
  ColMap v o k x -> vb v ++ storableToBytes o : columnBuffers k ++ columnBuffers x
  ColDenseUnion t o cs -> storableToBytes t : storableToBytes o : concatMap columnBuffers (V.toList cs)
  ColSparseUnion t cs -> storableToBytes t : concatMap columnBuffers (V.toList cs)
  ColDictionary _ ix vals -> columnBuffers ix ++ columnBuffers vals
  ColRunEndEncoded _ _ re vals -> columnBuffers re ++ columnBuffers vals
  where
    vb = maybe [] (\x -> [bitmapBytes (validityBits x)])


-- ============================================================
-- Structural validators
-- ============================================================

-- | Offset widths of var-length and list arrays, with their C kernels.
class (Storable o, Integral o, Bounded o, Show o) => Offset o where
  cOffsetsCheck :: Ptr o -> Int -> Int64 -> IO Int
  cUtf8Boundaries :: Ptr o -> Int -> Ptr Word8 -> Int -> IO Int
  cRebaseOffsets :: Ptr o -> Ptr o -> Int -> Int64 -> IO ()
  cTakeOffsets :: Ptr o -> Ptr o -> Ptr Int -> Int -> IO Int
  cTakeBytes :: Ptr Word8 -> Ptr o -> Ptr Word8 -> Ptr Int -> Int -> IO ()
  cListViewCheck :: Ptr o -> Ptr o -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int


instance Offset Int32 where
  cOffsetsCheck = K.offsetsCheckI32
  cUtf8Boundaries = K.utf8BoundariesI32
  cRebaseOffsets = K.rebaseOffsetsI32
  cTakeOffsets = K.takeOffsetsI32
  cTakeBytes = K.takeBytesI32
  cListViewCheck = K.listViewCheckI32
  {-# INLINE cOffsetsCheck #-}
  {-# INLINE cUtf8Boundaries #-}
  {-# INLINE cRebaseOffsets #-}
  {-# INLINE cTakeOffsets #-}
  {-# INLINE cTakeBytes #-}
  {-# INLINE cListViewCheck #-}


instance Offset Int64 where
  cOffsetsCheck = K.offsetsCheckI64
  cUtf8Boundaries = K.utf8BoundariesI64
  cRebaseOffsets = K.rebaseOffsetsI64
  cTakeOffsets = K.takeOffsetsI64
  cTakeBytes = K.takeBytesI64
  cListViewCheck = K.listViewCheckI64
  {-# INLINE cOffsetsCheck #-}
  {-# INLINE cUtf8Boundaries #-}
  {-# INLINE cRebaseOffsets #-}
  {-# INLINE cTakeOffsets #-}
  {-# INLINE cTakeBytes #-}
  {-# INLINE cListViewCheck #-}


kernelResult :: String -> String -> Int -> Either String ()
kernelResult what msg r
  | r < 0 = Right ()
  | otherwise = Left (what ++ ": " ++ msg ++ " at index " ++ show r)
{-# INLINE kernelResult #-}


{- | Offsets of a var-length or list array with @limit@ addressable
elements: at least one offset, @offs[0] >= 0@, monotonic,
@offs[last] <= limit@. One C pass.
-}
validateOffsets :: Offset o => String -> Int -> VS.Vector o -> Either String ()
validateOffsets what limit offs
  | VS.null offs = Left (what ++ ": offsets buffer is empty (an array of n rows needs n + 1 offsets)")
  | otherwise =
      kernelResult what "invalid offset" $
        unsafeDupablePerformIO $
          VS.unsafeWith offs $ \p -> cOffsetsCheck p (VS.length offs) (fromIntegral limit)


{- | UTF-8 data of a utf8 array whose offsets already passed
'validateOffsets' against the data length: the referenced range is
valid UTF-8 and every offset is on a character boundary.
-}
validateUtf8 :: Offset o => String -> VS.Vector o -> ByteString -> Either String ()
validateUtf8 what offs dat
  | VS.null offs = Right ()
  | not (BS.isValidUtf8 (BSU.unsafeTake (e - s) (BSU.unsafeDrop s dat))) = Left (what ++ ": data is not valid UTF-8")
  | otherwise =
      kernelResult what "offset inside a UTF-8 character" $
        unsafeDupablePerformIO $
          VS.unsafeWith offs $ \po -> withBytesPtr dat $ \pd ->
            cUtf8Boundaries po (VS.length offs) pd (BS.length dat)
  where
    !s = fromIntegral (VS.unsafeHead offs)
    !e = fromIntegral (VS.unsafeLast offs)


-- | Run an action on a validity's byte pointer and bit offset (null pointer when all valid).
withValidityPtr :: Maybe Validity -> (Ptr Word8 -> Int -> IO r) -> IO r
withValidityPtr Nothing k = k nullPtr 0
withValidityPtr (Just v) k =
  let !b = validityBits v
  in withBytesPtr (bitmapBytes b) $ \p -> k p (bitmapOffset b)
{-# INLINE withValidityPtr #-}


{- | Dictionary keys: an integer 'ColPrim' whose valid keys are all in
@[0, dictLength)@. Null keys are not checked.
-}
validateKeys :: String -> ColumnArray -> Int -> Either String ()
validateKeys what col dictLen = case col of
  ColPrim t v xs -> case t of
    PInt8 -> go K.keysInRangeI8 v xs
    PInt16 -> go K.keysInRangeI16 v xs
    PInt32 -> go K.keysInRangeI32 v xs
    PInt64 -> go K.keysInRangeI64 v xs
    PUInt8 -> go K.keysInRangeU8 v xs
    PUInt16 -> go K.keysInRangeU16 v xs
    PUInt32 -> go K.keysInRangeU32 v xs
    PUInt64 -> go K.keysInRangeU64 v xs
    _ -> notInt
  _ -> notInt
  where
    notInt = Left (what ++ ": dictionary indices must be an integer column")
    go :: Storable a => (Ptr a -> Int -> Ptr Word8 -> Int -> Int64 -> IO Int) -> Maybe Validity -> VS.Vector a -> Either String ()
    go k v xs =
      kernelResult what ("dictionary key outside a dictionary of " ++ show dictLen ++ " values") $
        unsafeDupablePerformIO $
          VS.unsafeWith xs $ \p -> withValidityPtr v $ \pv vo -> k p (VS.length xs) pv vo (fromIntegral dictLen)


{- | Run ends: a non-null 'PInt16' / 'PInt32' / 'PInt64' column,
positive and strictly increasing, the last at least @minEnd@.
-}
validateRunEnds :: String -> ColumnArray -> Int -> Either String ()
validateRunEnds what col minEnd = case col of
  ColPrim t Nothing xs -> case t of
    PInt16 -> go K.runEndsCheckI16 xs
    PInt32 -> go K.runEndsCheckI32 xs
    PInt64 -> go K.runEndsCheckI64 xs
    _ -> bad
  ColPrim _ (Just _) _ -> Left (what ++ ": run ends must not contain nulls")
  _ -> bad
  where
    bad = Left (what ++ ": run ends must be an int16, int32 or int64 column")
    go :: Storable a => (Ptr a -> Int -> Int64 -> IO Int) -> VS.Vector a -> Either String ()
    go k xs =
      kernelResult what ("run ends must be positive, strictly increasing and reach " ++ show minEnd) $
        unsafeDupablePerformIO $
          VS.unsafeWith xs $ \p -> k p (VS.length xs) (fromIntegral minEnd)


-- | List-view offsets and sizes: per valid row, in range of the child.
validateListView :: Offset o => String -> Maybe Validity -> VS.Vector o -> VS.Vector o -> Int -> Either String ()
validateListView what v offs sizes childLen
  | VS.length offs /= VS.length sizes = Left (what ++ ": offsets and sizes differ in length")
  | otherwise =
      kernelResult what "list view outside its child" $
        unsafeDupablePerformIO $
          VS.unsafeWith offs $ \po -> VS.unsafeWith sizes $ \ps -> withValidityPtr v $ \pv vo ->
            cListViewCheck po ps (VS.length offs) pv vo (fromIntegral childLen)


-- | Dense union child indices and offsets against the child lengths.
validateDenseUnion :: String -> VS.Vector Int8 -> VS.Vector Int32 -> VS.Vector Int64 -> Either String ()
validateDenseUnion what types offs childLens
  | VS.length types /= VS.length offs = Left (what ++ ": type ids and offsets differ in length")
  | otherwise =
      kernelResult what "union child index or offset out of range" $
        unsafeDupablePerformIO $
          VS.unsafeWith types $ \pt -> VS.unsafeWith offs $ \po -> VS.unsafeWith childLens $ \pl ->
            K.denseUnionCheck pt po (VS.length types) pl (VS.length childLens)


-- | Sparse union child indices below the number of children.
validateSparseUnionTypes :: String -> VS.Vector Int8 -> Int -> Either String ()
validateSparseUnionTypes what types nChildren =
  kernelResult what "union child index out of range" $
    unsafeDupablePerformIO $
      VS.unsafeWith types $ \pt -> K.keysInRangeI8 pt (VS.length types) nullPtr 0 (fromIntegral nChildren)


{- | Binary or utf8 views: 16 bytes per row, every valid view
references in-range bytes with a matching prefix; utf8 views hold
valid UTF-8.
-}
validateViews :: String -> Bool -> Int -> Maybe Validity -> ByteString -> V.Vector ByteString -> Either String ()
validateViews what utf8 rows v views bufs
  | BS.length views < rows * 16 = Left (what ++ ": views buffer too small for " ++ show rows ++ " rows")
  | otherwise =
      kernelResult what "invalid view" $
        unsafeDupablePerformIO $
          withBytesPtr views $ \pviews -> withValidityPtr v $ \pv vo ->
            withAll (V.toList bufs) [] $ \ptrs ->
              allocaArray nb $ \pptrs -> allocaArray nb $ \plens -> do
                pokeArray pptrs (reverse ptrs)
                pokeArray plens (map (fromIntegral . BS.length) (V.toList bufs) :: [Int64])
                K.viewRefsCheck pviews rows pv vo pptrs plens nb utf8
  where
    !nb = V.length bufs
    withAll :: [ByteString] -> [Ptr Word8] -> ([Ptr Word8] -> IO r) -> IO r
    withAll [] acc k = k acc
    withAll (b : rest) acc k = withBytesPtr b $ \p -> withAll rest (castPtr p : acc) k


-- ============================================================
-- Offset helpers
-- ============================================================

-- | A row's range in a list-like column's child: start and length.
data ChildRange = ChildRange
  { childStart :: {-# UNPACK #-} !Int
  , childLength :: {-# UNPACK #-} !Int
  }
  deriving stock (Show, Eq)


-- | Offsets shifted to start at 0 (fresh memory), and the child range they covered.
rebaseToZero :: forall o. Offset o => VS.Vector o -> (VS.Vector o, ChildRange)
rebaseToZero o
  | VS.null o = (VS.singleton 0, ChildRange 0 0)
  | otherwise =
      let !o0 = VS.unsafeHead o
          !n = VS.length o
          bytes = createAligned (n * sizeOf o0) $ \dst ->
            VS.unsafeWith o $ \src -> cRebaseOffsets (castPtr dst) src n (negate (fromIntegral o0))
      in (unsafeBytesToStorable bytes, ChildRange (fromIntegral o0) (fromIntegral (VS.unsafeLast o - o0)))


