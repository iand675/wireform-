{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Properties of "Arrow.Vector" against the boxed "Data.Vector" model.

For every element type the generic API ('VG.fromList', 'VG.generate',
slices at arbitrary offsets, 'VG.map', 'VG.filter', 'VG.concat',
'VG.thaw' / 'VG.freeze' / 'VG.modify', growth, 'VG.force', 'Eq', 'Ord')
must agree with the same operation on a list. Mutable operations run on
a slice of a larger vector so bitmaps sit at bit offsets that are not a
multiple of 8 and var-length rows share their store with neighbours.

The column conversions are checked against the logical row model of
"Test.Arrow.Gen" on generated columns, on the same columns after an IPC
round trip (decoded columns alias the input; one input lives on the
GHC heap and one in malloc memory, which takes the text copy path), and
on slices of both.
-}
module Test.Arrow.Vector (tests) where

import Arrow.Column
import Arrow.Stream (decodeArrowStream, defaultWriteOptions, encodeArrowStream)
import Arrow.Types (ArrowType (..), DictionaryEncoding (..), Field (..), IntervalUnit (..), Precision (..), defaultField, defaultLeafField, defaultSchema)
import Arrow.Vector (Element, Vector, maybeValues)
import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Foldable (foldl')
import Data.Either (isLeft)
import Data.Int (Int32, Int64)
import Data.List (uncons)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as GM
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Arrow.Gen (Value (..), columnValues, genColumnFor)


tests :: IO Bool
tests =
  checkParallel $
    Group
      "Arrow.Vector"
      [ ("Int8", laws (Gen.int8 Range.linearBounded))
      , ("Int16", laws (Gen.int16 Range.linearBounded))
      , ("Int32", laws i32)
      , ("Int64", laws i64)
      , ("Int", laws (Gen.int Range.linearBounded))
      , ("Word8", laws (Gen.word8 Range.linearBounded))
      , ("Word16", laws (Gen.word16 Range.linearBounded))
      , ("Word32", laws (Gen.word32 Range.linearBounded))
      , ("Word64", laws (Gen.word64 Range.linearBounded))
      , ("Word", laws (Gen.word Range.linearBounded))
      , ("Float", laws (Gen.float (Range.linearFrac (-1e6) 1e6)))
      , ("Double", laws (Gen.double (Range.linearFrac (-1e12) 1e12)))
      , ("Float16", laws (Float16 <$> Gen.word16 Range.linearBounded))
      , ("IntervalDayTime", laws (IntervalDayTime <$> i32 <*> i32))
      , ("IntervalMonthDayNano", laws (IntervalMonthDayNano <$> i32 <*> i32 <*> i64))
      , ("Decimal128", laws genDecimal128)
      , ("Decimal256", laws (decimal256FromInteger <$> Gen.integral (Range.linearFrom 0 (negate (2 ^ (255 :: Int))) (2 ^ (255 :: Int) - 1))))
      , ("Bool", laws Gen.bool)
      , ("Text", laws genText)
      , ("ByteString", laws genBytes)
      , ("Maybe Int64", laws (maybeOf i64))
      , ("Maybe Bool", laws (maybeOf Gen.bool))
      , ("Maybe Decimal128", laws (maybeOf genDecimal128))
      , ("Maybe Text", laws (maybeOf genText))
      , ("Maybe ByteString", laws (maybeOf genBytes))
      , ("Maybe (Maybe Int32)", laws (maybeOf (maybeOf i32)))
      , ("Vector (Maybe Int32)", laws (genVec (maybeOf i32)))
      , ("Maybe (Vector (Maybe Int32))", laws (maybeOf (genVec (maybeOf i32))))
      , ("Vector Text", laws (genVec genText))
      , ("Maybe (Vector (Maybe Text))", laws (maybeOf (genVec (maybeOf genText))))
      , ("Vector (Vector Int64)", laws (genVec (genVec i64)))
      , ("new initialises: Nothing, False, empty rows", prop_newInitialises)
      , ("unsafeThaw of a converted text vector keeps rows apart", prop_unsafeThawConverted)
      , ("conversions agree with the row model on generated, decoded and sliced columns", prop_conversions)
      ]


-- ============================================================
-- Generators
-- ============================================================

i32 :: Gen Int32
i32 = Gen.int32 Range.linearBounded


i64 :: Gen Int64
i64 = Gen.int64 Range.linearBounded


genDecimal128 :: Gen Decimal128
genDecimal128 = decimal128FromInteger <$> Gen.integral (Range.linearFrom 0 (negate (2 ^ (127 :: Int))) (2 ^ (127 :: Int) - 1))


maybeOf :: Gen a -> Gen (Maybe a)
maybeOf g = Gen.frequency [(1, pure Nothing), (3, Just <$> g)]


-- | Often a suffix of a longer text, so rows start inside their array.
genText :: Gen Text
genText = do
  t <- Gen.text (Range.linear 0 10) Gen.unicode
  k <- Gen.int (Range.linear 0 (T.length t))
  pure (T.drop k t)


genBytes :: Gen ByteString
genBytes = do
  b <- Gen.bytes (Range.linear 0 10)
  k <- Gen.int (Range.linear 0 (BS.length b))
  pure (BS.drop k b)


-- | Often a slice of a longer vector.
genVec :: Element a => Gen a -> Gen (Vector a)
genVec g = do
  xs <- Gen.list (Range.linear 0 6) g
  k <- Gen.int (Range.linear 0 (length xs))
  pure (VG.drop k (VG.fromList xs))


-- ============================================================
-- Generic API against the list model
-- ============================================================

data Op a
  = Write Int a
  | Set Int Int a
  | -- | destination, source, length (the ranges may overlap)
    Move Int Int Int
  | -- | copy from a separate mutable vector
    CopyIn Int [a]
  | -- | copy from an immutable vector
    CopyFrom Int [a]
  deriving stock (Show)


genOps :: Int -> Gen a -> Gen [Op a]
genOps n g
  | n <= 0 = pure []
  | otherwise = Gen.list (Range.linear 0 8) (Gen.choice [write, set, move, copy CopyIn, copy CopyFrom])
  where
    write = Write <$> Gen.int (Range.constant 0 (n - 1)) <*> g
    set = do
      s <- Gen.int (Range.constant 0 n)
      l <- Gen.int (Range.constant 0 (n - s))
      Set s l <$> g
    move = do
      l <- Gen.int (Range.constant 0 n)
      Move <$> Gen.int (Range.constant 0 (n - l)) <*> Gen.int (Range.constant 0 (n - l)) <*> pure l
    copy k = do
      l <- Gen.int (Range.constant 0 n)
      k <$> Gen.int (Range.constant 0 (n - l)) <*> Gen.list (Range.singleton l) g


applyOp :: Element a => VG.Mutable Vector s a -> Op a -> ST s ()
applyOp mv = \case
  Write i x -> GM.write mv i x
  Set s l x -> GM.set (GM.slice s l mv) x
  Move d s l -> GM.move (GM.slice d l mv) (GM.slice s l mv)
  CopyIn d xs -> do
    src <- VG.thaw (VG.fromList xs)
    GM.copy (GM.slice d (length xs) mv) src
  CopyFrom d xs -> VG.copy (GM.slice d (length xs) mv) (VG.fromList xs)


modelOp :: [a] -> Op a -> [a]
modelOp xs = \case
  Write i x -> splice i 1 [x]
  Set s l x -> splice s l (replicate l x)
  Move d s l -> splice d l (take l (drop s xs))
  CopyIn d ys -> splice d (length ys) ys
  CopyFrom d ys -> splice d (length ys) ys
  where
    splice at l ys = take at xs ++ ys ++ drop (at + l) xs


laws :: (Element a, Ord a, Show a) => Gen a -> Property
laws gen = withTests 150 . property $ do
  xs <- forAll (Gen.list (Range.linear 0 70) gen)
  ys <- forAll (Gen.list (Range.linear 0 20) gen)
  y <- forAll gen
  let n = length xs
      v = VG.fromList xs
      w = VG.fromList ys
      bv = V.fromList xs
  -- construction, indexing, conversion to and from boxed vectors
  VG.toList v === xs
  VG.length v === n
  map (v VG.!) [0 .. n - 1] === xs
  VG.convert v === bv
  VG.convert bv === v
  VG.generate n (bv V.!) === v
  VG.toList (VG.unfoldr uncons xs `asTypeOf` v) === xs
  VG.toList (VG.replicate 11 y `asTypeOf` v) === replicate 11 y
  VG.toList (VG.snoc (VG.cons y v) y) === [y] ++ xs ++ [y]
  -- slices at any offset, and slices of slices
  i <- forAll (Gen.int (Range.linear 0 n))
  k <- forAll (Gen.int (Range.linear 0 (n - i)))
  let s = VG.slice i k v
      sm = take k (drop i xs)
  VG.toList s === sm
  j <- forAll (Gen.int (Range.linear 0 k))
  l <- forAll (Gen.int (Range.linear 0 (k - j)))
  VG.toList (VG.slice j l s) === take l (drop j sm)
  -- map, filter and concat over a slice
  keep <- V.fromList <$> forAll (Gen.list (Range.singleton k) Gen.bool)
  VG.toList (VG.imap (\ix x -> if keep V.! ix then x else y) s) === zipWith (\b x -> if b then x else y) (V.toList keep) sm
  VG.toList (VG.ifilter (\ix _ -> keep V.! ix) s) === map snd (filter fst (zip (V.toList keep) sm))
  VG.toList (VG.concat [s, w, v, s]) === sm ++ ys ++ xs ++ sm
  VG.toList (v <> w) === xs ++ ys
  -- force copies only the referenced rows and keeps them
  VG.force s === s
  VG.toList (VG.force s) === sm
  -- mutable operations on a window of a larger vector
  ops <- forAll (genOps k gen)
  let modified = VG.modify (\mv -> forM_ ops (applyOp (GM.slice i k mv))) v
  VG.toList modified === take i xs ++ foldl' modelOp sm ops ++ drop (i + k) xs
  let thawed = runST $ do
        mv <- VG.thaw s
        forM_ ops (applyOp mv)
        VG.freeze mv
  VG.toList thawed === foldl' modelOp sm ops
  VG.toList s === sm
  -- growing keeps the rows and appends writable slots
  extra <- forAll (Gen.int (Range.linear 0 20))
  let grown = runST $ do
        mv <- VG.thaw s
        mv' <- GM.grow mv extra
        forM_ [k .. k + extra - 1] $ \ix -> GM.write mv' ix y
        VG.freeze mv'
  VG.toList grown === sm ++ replicate extra y
  -- Eq and Ord are the list's
  (v == w) === (xs == ys)
  compare v w === compare xs ys
  compare s w === compare sm ys
  (VG.fromList sm == s) === True


prop_newInitialises :: Property
prop_newInitialises = withTests 50 . property $ do
  k <- forAll (Gen.int (Range.linear 0 70))
  let fresh :: Element a => Vector a
      fresh = runST (GM.new k >>= VG.freeze)
  assert (VG.all isNothing (fresh :: Vector (Maybe Int64)))
  assert (VG.all isNothing (fresh :: Vector (Maybe Text)))
  assert (VG.all isNothing (fresh :: Vector (Maybe (Vector Int32))))
  assert (VG.all not (fresh :: Vector Bool))
  assert (VG.all T.null (fresh :: Vector Text))
  assert (VG.all BS.null (fresh :: Vector ByteString))
  assert (VG.all VG.null (fresh :: Vector (Vector Int64)))


{- | A utf8 column converts with starts and ends sharing one offsets
buffer; mutating the unsafely thawed vector must still treat rows as
independent.
-}
prop_unsafeThawConverted :: Property
prop_unsafeThawConverted = withTests 100 . property $ do
  rows <- forAll (Gen.list (Range.linear 1 20) genText)
  x <- forAll genText
  i <- forAll (Gen.int (Range.linear 0 (length rows - 1)))
  converted <- evalEither (toTextVector (fromTexts (V.fromList rows)))
  let values = maybeValues converted
      written = runST $ do
        mv <- VG.unsafeThaw values
        GM.write mv i x
        VG.unsafeFreeze mv
  VG.toList written === take i rows ++ [x] ++ drop (i + 1) rows


-- ============================================================
-- Conversions against the row model
-- ============================================================

-- | Fields whose columns some conversion accepts; lists hold int32.
genConvField :: Gen Field
genConvField = do
  nullable <- Gen.bool
  let leaf = defaultLeafField "c" nullable
      list ty = defaultField "c" nullable ty (V.singleton (defaultLeafField "item" True (AInt 32 True)))
  Gen.choice
    [ leaf <$> Gen.element [AInt 8 True, AInt 16 False, AInt 32 True, AInt 64 True, AInt 64 False, AFloatingPoint Half, AFloatingPoint Single, AFloatingPoint DoublePrecision, ADecimal 38 4, ADecimal256 76 10, AInterval DayTime, AInterval MonthDayNano]
    , leaf <$> Gen.element [ABool, AUtf8, ALargeUtf8, ABinary, ALargeBinary, AUtf8View, ABinaryView]
    , leaf . AFixedSizeBinary <$> Gen.int (Range.linear 0 4)
    , list <$> Gen.element [AList, ALargeList, AListView, ALargeListView]
    , list . AFixedSizeList <$> Gen.int (Range.linear 0 3)
    , do
        ty <- Gen.element [AUtf8, ABinary, ALargeUtf8]
        idx <- AInt <$> Gen.element [8, 16, 32, 64] <*> Gen.bool
        pure (leaf ty) {fieldDictionary = Just (DictionaryEncoding 0 idx False)}
    ]


prop_conversions :: Property
prop_conversions = withTests 400 . property $ do
  f <- forAll genConvField
  n <- forAll (Gen.int (Range.linear 0 40))
  c <- forAll (genColumnFor f n)
  bytes <- evalEither (encodeArrowStream defaultWriteOptions (defaultSchema (V.singleton f)) [V.singleton c])
  heap <- decodedFrom bytes
  malloced <- evalIO (mallocCopy bytes) >>= decodedFrom
  forM_ [c, heap, malloced] $ \col -> do
    checkConversions f col
    i <- forAll (Gen.int (Range.linear 0 n))
    k <- forAll (Gen.int (Range.linear 0 (n - i)))
    checkConversions f (sliceColumnArray i k col)
  where
    decodedFrom bs = do
      (_, batches) <- evalEither (decodeArrowStream bs)
      case batches of
        [batch] | V.length batch == 1 -> pure (V.head batch)
        _ -> annotate "expected one batch of one column" >> failure


-- | A copy of the bytes in malloc memory (not the GHC heap).
mallocCopy :: ByteString -> IO ByteString
mallocCopy bs = do
  let len = BS.length bs
  p <- mallocBytes (max 1 len)
  BS.useAsCStringLen bs $ \(src, _) -> copyBytes p (castPtr src) len
  fp <- newForeignPtr finalizerFree p
  pure (BSI.BS fp len)


checkConversions :: Field -> ColumnArray -> PropertyT IO ()
checkConversions f c = do
  model <- evalEither (columnValues c)
  let rows = V.toList model
      ty = fieldType f
      textKind = ty `elem` [AUtf8, ALargeUtf8, AUtf8View]
      bytesKind = textKind || ty `elem` [ABinary, ALargeBinary, ABinaryView] || isFixedBinary ty
      listKind = ty `elem` [AList, ALargeList, AListView, ALargeListView] || isFixedList ty
  case c of
    ColPrim t _ _ -> withPrim t $ do
      arr <- maybe (annotate "asPrim" >> failure) pure (asPrim t c)
      let v = toMaybeVector arr
      VG.length v === length rows
      -- every row through the generic index path, rebuilt as a column
      evalEither (columnValues (fromMaybes t (VG.convert v))) >>= (=== model)
      evalEither (columnValues (fromMaybeVector t v)) >>= (=== model)
      evalEither (columnValues (fromMaybeVector t (VG.force v))) >>= (=== model)
    ColBool {} -> do
      v <- evalEither (toBoolVector c)
      map (maybe VNull VBool) (VG.toList v) === rows
      evalEither (columnValues (fromBoolVector (VG.force v))) >>= (=== model)
    _ -> success
  if textKind
    then do
      v <- evalEither (toTextVector c)
      map (maybe VNull VText) (VG.toList v) === rows
      VG.force v === v
    else assert (isLeft (toTextVector c))
  if bytesKind
    then do
      v <- evalEither (toBytesVector c)
      map (maybe VNull VBytes) (VG.toList v) === map asBytes rows
      VG.force v === v
    else success
  if listKind
    then do
      v <- evalEither (toListVector int32s c)
      map (maybe VNull (VList . map (maybe VNull (VInt . toInteger)) . VG.toList)) (VG.toList v) === rows
      VG.force v === v
    else success
  where
    int32s ch = maybe (Left ("not an int32 child: " ++ columnTag ch)) (Right . toMaybeVector) (asPrim PInt32 ch)
    asBytes = \case
      VText t -> VBytes (TE.encodeUtf8 t)
      other -> other
    isFixedBinary = \case
      AFixedSizeBinary _ -> True
      _ -> False
    isFixedList = \case
      AFixedSizeList _ -> True
      _ -> False

