{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Properties of the column core ("Arrow.Column", its buffers and
builders) against a boxed row model.

Columns are generated together with their model rows through the
public construction API (the @from*@ conversions, the builders and the
@mk*@ constructors), usually as a slice of a longer column so bitmaps
start at non-zero bit offsets and offsets at non-zero bases. Every
row operation is then checked against the same operation on the model.
-}
module Test.Arrow.Core (tests) where

import Arrow.Column
import Arrow.Column.Internal (Bitmap (..), Validity (..), checkValidity, columnBuffers, concatBitmaps, copyBitmap, integralPrim, IntegralPrim (..), sliceBitmap, takeBitmap)
import Control.Monad (forM, replicateM)
import Control.Monad.ST (runST)
import Data.Bits (setBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Either (isLeft, isRight)
import Data.Foldable (foldl')
import Data.Int (Int32, Int64)
import Data.List (sortOn)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range


tests :: IO Bool
tests =
  checkParallel $
    Group
      "Arrow.Core"
      [ ("bitmaps at arbitrary bit offsets match a bool-list model", prop_bitmaps)
      , ("validity is normalised: no nulls means Nothing", prop_validityNormalised)
      , ("generated columns observe as their model", prop_observeModel)
      , ("typed accessors agree with the model and reject out-of-range rows", prop_accessors)
      , ("boxed conversions agree with the model on every kind they accept", prop_conversions)
      , ("builders build what the from* conversions build, and reset on freeze", prop_builders)
      , ("Eq is reflexive and agrees with the model", prop_eq)
      , ("slice keeps the clamped window", prop_slice)
      , ("slice of slice composes", prop_sliceOfSlice)
      , ("concat of adjacent slices is the column", prop_concatAdjacent)
      , ("concat appends rows", prop_concatAppends)
      , ("take gathers rows and rejects bad indices", prop_take)
      , ("copyColumn is equal and shares no memory", prop_copyColumn)
      , ("expandDictionary keeps rows", prop_expandDictionary)
      , ("maskValidity nulls masked rows", prop_maskValidity)
      , ("map key order agrees with the model", prop_mapKeys)
      , ("mk* constructors reject invalid buffers", prop_mkRejects)
      ]


-- ============================================================
-- Model
-- ============================================================

data Value
  = VNull
  | VInt Integer
  | VBits Integer
  | VBool Bool
  | VBytes ByteString
  | VText Text
  | VPair Int32 Int32
  | VTriple Int32 Int32 Int64
  | VList [Value]
  | VStruct [Value]
  | VMap [(Value, Value)]
  | VUnion Int Value
  deriving stock (Eq, Show)


data Ty
  = TPrim SomePrimType
  | TBool
  | TUtf8
  | TBinary
  | TLargeUtf8
  | TLargeBinary
  | TFixed Int
  | TUtf8View
  | TBinaryView
  | TNull
  | TStruct [Ty]
  | TList Ty
  | TLargeList Ty
  | -- | Large (64-bit) offsets when True.
    TListView Bool Ty
  | TFixedList Int Ty
  | TMap Ty
  | TDense [Ty]
  | TSparse [Ty]
  | TDict SomePrimType Ty
  | TRee SomePrimType Ty
  deriving stock (Show)


primTypes :: [SomePrimType]
primTypes =
  [ SomePrimType PInt8
  , SomePrimType PInt16
  , SomePrimType PInt32
  , SomePrimType PInt64
  , SomePrimType PUInt8
  , SomePrimType PUInt16
  , SomePrimType PUInt32
  , SomePrimType PUInt64
  , SomePrimType PFloat16
  , SomePrimType PFloat
  , SomePrimType PDouble
  , SomePrimType PDate32
  , SomePrimType PDate64
  , SomePrimType PTime32
  , SomePrimType PTime64
  , SomePrimType PTimestamp
  , SomePrimType PDuration
  , SomePrimType PIntervalYearMonth
  , SomePrimType PIntervalDayTime
  , SomePrimType PIntervalMonthDayNano
  , SomePrimType (PDecimal128 38 4)
  , SomePrimType (PDecimal256 76 10)
  ]


keyTypes :: [SomePrimType]
keyTypes = [SomePrimType PInt8, SomePrimType PInt16, SomePrimType PInt32, SomePrimType PInt64, SomePrimType PUInt8, SomePrimType PUInt16, SomePrimType PUInt32, SomePrimType PUInt64]


genTy :: Int -> Gen Ty
genTy d =
  Gen.frequency $
    [ (6, TPrim <$> Gen.element primTypes)
    , (2, pure TBool)
    , (2, pure TUtf8)
    , (1, pure TBinary)
    , (1, pure TLargeUtf8)
    , (1, pure TLargeBinary)
    , (1, TFixed <$> Gen.int (Range.linear 0 5))
    , (1, pure TUtf8View)
    , (1, pure TBinaryView)
    , (1, pure TNull)
    ]
      ++ if d <= 0
        then []
        else
          [ (1, TStruct <$> Gen.list (Range.linear 0 3) sub)
          , (1, TList <$> sub)
          , (1, TLargeList <$> sub)
          , (1, TListView <$> Gen.bool <*> sub)
          , (1, TFixedList <$> Gen.int (Range.linear 0 3) <*> sub)
          , (1, TMap <$> sub)
          , (1, TDense <$> Gen.list (Range.linear 1 3) sub)
          , (1, TSparse <$> Gen.list (Range.linear 1 3) sub)
          , (1, TDict <$> Gen.element keyTypes <*> genTy 0)
          , (1, TRee <$> Gen.element [SomePrimType PInt16, SomePrimType PInt32, SomePrimType PInt64] <*> genTy 0)
          ]
  where
    sub = genTy (d - 1)


genPrim :: PrimType a -> Gen (a, Value)
genPrim = \case
  PInt8 -> int (Gen.int8 Range.linearBounded)
  PInt16 -> int (Gen.int16 Range.linearBounded)
  PInt32 -> int (Gen.int32 Range.linearBounded)
  PInt64 -> int (Gen.int64 Range.linearBounded)
  PUInt8 -> int (Gen.word8 Range.linearBounded)
  PUInt16 -> int (Gen.word16 Range.linearBounded)
  PUInt32 -> int (Gen.word32 Range.linearBounded)
  PUInt64 -> int (Gen.word64 Range.linearBounded)
  PFloat16 -> (\w -> (Float16 w, VBits (toInteger w))) <$> Gen.word16 Range.linearBounded
  PFloat -> (\w -> (castWord32ToFloat w, VBits (toInteger w))) <$> Gen.word32 Range.linearBounded
  PDouble -> (\w -> (castWord64ToDouble w, VBits (toInteger w))) <$> Gen.word64 Range.linearBounded
  PDate32 -> int (Gen.int32 Range.linearBounded)
  PDate64 -> int (Gen.int64 Range.linearBounded)
  PTime32 -> int (Gen.int32 Range.linearBounded)
  PTime64 -> int (Gen.int64 Range.linearBounded)
  PTimestamp -> int (Gen.int64 Range.linearBounded)
  PDuration -> int (Gen.int64 Range.linearBounded)
  PIntervalYearMonth -> int (Gen.int32 Range.linearBounded)
  PIntervalDayTime -> (\a b -> (IntervalDayTime a b, VPair a b)) <$> Gen.int32 Range.linearBounded <*> Gen.int32 Range.linearBounded
  PIntervalMonthDayNano ->
    (\a b c -> (IntervalMonthDayNano a b c, VTriple a b c))
      <$> Gen.int32 Range.linearBounded
      <*> Gen.int32 Range.linearBounded
      <*> Gen.int64 Range.linearBounded
  PDecimal128 _ _ -> (\x -> (decimal128FromInteger x, VInt x)) <$> Gen.integral (Range.linearFrom 0 (negate (2 ^ (127 :: Int))) (2 ^ (127 :: Int) - 1))
  PDecimal256 _ _ -> (\x -> (decimal256FromInteger x, VInt x)) <$> Gen.integral (Range.linearFrom 0 (negate (2 ^ (255 :: Int))) (2 ^ (255 :: Int) - 1))
  where
    int :: Integral b => Gen b -> Gen (b, Value)
    int = fmap (\x -> (x, VInt (toInteger x)))


primValue :: PrimType a -> a -> Value
primValue t x = case t of
  PInt8 -> VInt (toInteger x)
  PInt16 -> VInt (toInteger x)
  PInt32 -> VInt (toInteger x)
  PInt64 -> VInt (toInteger x)
  PUInt8 -> VInt (toInteger x)
  PUInt16 -> VInt (toInteger x)
  PUInt32 -> VInt (toInteger x)
  PUInt64 -> VInt (toInteger x)
  PFloat16 -> let Float16 w = x in VBits (toInteger w)
  PFloat -> VBits (toInteger (castFloatToWord32 x))
  PDouble -> VBits (toInteger (castDoubleToWord64 x))
  PDate32 -> VInt (toInteger x)
  PDate64 -> VInt (toInteger x)
  PTime32 -> VInt (toInteger x)
  PTime64 -> VInt (toInteger x)
  PTimestamp -> VInt (toInteger x)
  PDuration -> VInt (toInteger x)
  PIntervalYearMonth -> VInt (toInteger x)
  PIntervalDayTime -> let IntervalDayTime a b = x in VPair a b
  PIntervalMonthDayNano -> let IntervalMonthDayNano a b c = x in VTriple a b c
  PDecimal128 _ _ -> VInt (decimal128ToInteger x)
  PDecimal256 _ _ -> VInt (decimal256ToInteger x)


maybeOf :: Gen a -> Gen (Maybe a)
maybeOf g = Gen.frequency [(1, pure Nothing), (4, Just <$> g)]


genValid :: Int -> Gen [Bool]
genValid n = Gen.frequency [(1, pure (replicate n True)), (3, replicateM n (Gen.frequency [(1, pure False), (3, pure True)]))]


validityOf :: [Bool] -> Maybe Validity
validityOf = validityFromBools . V.fromList


genText :: Gen Text
genText = Gen.text (Range.linear 0 20) Gen.unicode


genBytes :: Gen ByteString
genBytes = Gen.bytes (Range.linear 0 20)


right :: Either String a -> a
right = either error id


-- | A column of @n@ rows and its model, usually a slice of a longer column.
genColumnOf :: Ty -> Int -> Gen ([Value], ColumnArray)
genColumnOf ty n = do
  pre <- Gen.frequency [(1, pure 0), (3, Gen.int (Range.linear 0 11))]
  post <- Gen.int (Range.linear 0 3)
  (m, c) <- genRaw ty (pre + n + post)
  pure (take n (drop pre m), sliceColumnArray pre n c)


genRaw :: Ty -> Int -> Gen ([Value], ColumnArray)
genRaw ty n = case ty of
  TPrim (SomePrimType t) -> withPrim t $ do
    rows <- replicateM n (maybeOf (genPrim t))
    let xs = map (fmap fst) rows
    viaBuilder <- Gen.bool
    let c =
          if viaBuilder
            then runST $ do
              b <- newPrimBuilder t 0
              mapM_ (appendPrimMaybe b) xs
              freezeBuilder b
            else fromMaybes t (V.fromList xs)
    pure (map (maybe VNull snd) rows, c)
  TBool -> do
    rows <- replicateM n (maybeOf Gen.bool)
    viaBuilder <- Gen.bool
    let c =
          if viaBuilder
            then runST $ do
              b <- newBoolBuilder 0
              mapM_ (appendBoolMaybe b) rows
              freezeBuilder b
            else fromMaybeBools (V.fromList rows)
    pure (map (maybe VNull VBool) rows, c)
  TUtf8 -> do
    rows <- replicateM n (maybeOf genText)
    how <- Gen.int (Range.constant 0 2)
    c <- case how of
      0 -> pure (fromMaybeTexts (V.fromList rows))
      1 -> pure $ runST $ do
        b <- newUtf8Builder 0
        mapM_ (appendTextMaybe b) rows
        freezeBuilder b
      _ -> do
        prefix <- genText
        let base = BS.length (TE.encodeUtf8 prefix)
            lens = map (maybe 0 (BS.length . TE.encodeUtf8)) rows
            dat = TE.encodeUtf8 (prefix <> T.concat (map (maybe "" id) rows))
            offs = VS.fromList (map fromIntegral (scanl (+) base lens)) :: VS.Vector Int32
        pure (right (mkUtf8 (validityOf (map isJust rows)) offs dat))
    pure (map (maybe VNull VText) rows, c)
  TLargeUtf8 -> do
    rows <- replicateM n (maybeOf genText)
    viaBuilder <- Gen.bool
    let c =
          if viaBuilder
            then runST $ do
              b <- newLargeUtf8Builder 0
              mapM_ (appendTextMaybe b) rows
              freezeBuilder b
            else fromMaybeLargeTexts (V.fromList rows)
    pure (map (maybe VNull VText) rows, c)
  TBinary -> do
    rows <- replicateM n (maybeOf genBytes)
    viaBuilder <- Gen.bool
    let c =
          if viaBuilder
            then runST $ do
              b <- newBinaryBuilder 0
              mapM_ (appendBytesMaybe b) rows
              freezeBuilder b
            else fromMaybeByteStrings (V.fromList rows)
    pure (map (maybe VNull VBytes) rows, c)
  TLargeBinary -> do
    rows <- replicateM n (maybeOf genBytes)
    viaBuilder <- Gen.bool
    let c =
          if viaBuilder
            then runST $ do
              b <- newLargeBinaryBuilder 0
              mapM_ (appendBytesMaybe b) rows
              freezeBuilder b
            else fromMaybeLargeByteStrings (V.fromList rows)
    pure (map (maybe VNull VBytes) rows, c)
  TFixed w -> do
    rows <- replicateM n (maybeOf (Gen.bytes (Range.singleton w)))
    pure (map (maybe VNull VBytes) rows, right (fromMaybeFixedSizeBinary w (V.fromList rows)))
  TUtf8View -> do
    rows <- replicateM n (maybeOf (Gen.text (Range.linear 0 30) Gen.unicode))
    pure (map (maybe VNull VText) rows, fromMaybeUtf8View (V.fromList rows))
  TBinaryView -> do
    rows <- replicateM n (maybeOf (Gen.bytes (Range.linear 0 30)))
    pure (map (maybe VNull VBytes) rows, fromMaybeBinaryView (V.fromList rows))
  TNull -> pure (replicate n VNull, ColNull n)
  TStruct tys -> do
    valid <- genValid n
    children <- forM tys $ \t -> do
      extra <- Gen.int (Range.linear 0 2)
      genColumnOf t (n + extra)
    let names = map (\i -> T.pack ("f" ++ show i)) [0 .. length tys - 1]
        c = right (mkStruct n (validityOf valid) (V.fromList (zip names (map snd children))))
        row i ok = if ok then VStruct (map (\(cm, _) -> cm !! i) children) else VNull
    pure (zipWith row [0 ..] valid, c)
  TList ct -> do
    (m, offs, child) <- genListParts ct n
    valid <- genValid n
    pure (listModel valid m offs, right (mkList (validityOf valid) (VS.fromList (map fromIntegral offs)) child))
  TLargeList ct -> do
    (m, offs, child) <- genListParts ct n
    valid <- genValid n
    pure (listModel valid m offs, right (mkLargeList (validityOf valid) (VS.fromList (map fromIntegral offs)) child))
  TListView large ct -> do
    len <- Gen.int (Range.linear 0 8)
    (cm, child) <- genColumnOf ct len
    views <- replicateM n $ do
      off <- Gen.int (Range.linear 0 len)
      size <- Gen.int (Range.linear 0 (len - off))
      pure (off, size)
    valid <- genValid n
    let row (off, size) ok = if ok then VList (take size (drop off cm)) else VNull
        offs = VS.fromList (map (fromIntegral . fst) views) :: VS.Vector Int32
        sizes = VS.fromList (map (fromIntegral . snd) views) :: VS.Vector Int32
    let c =
          if large
            then right (mkLargeListView (validityOf valid) (VS.map fromIntegral offs) (VS.map fromIntegral sizes) child)
            else right (mkListView (validityOf valid) offs sizes child)
    pure (zipWith row views valid, c)
  TFixedList w ct -> do
    extra <- Gen.int (Range.linear 0 2)
    (cm, child) <- genColumnOf ct (n * w + extra)
    valid <- genValid n
    let row i ok = if ok then VList (take w (drop (i * w) cm)) else VNull
    pure (zipWith row [0 ..] valid, right (mkFixedSizeList w n (validityOf valid) child))
  TMap vt -> do
    lens <- replicateM n (Gen.int (Range.linear 0 3))
    base <- Gen.int (Range.linear 0 3)
    let total = base + sum lens
    (km, keys) <- genColumnOf (TPrim (SomePrimType PInt32)) total
    (vm, vals) <- genColumnOf vt total
    valid <- genValid n
    let offs = scanl (+) base lens
        row (s, e) ok = if ok then VMap (take (e - s) (drop s (zip km vm))) else VNull
    pure (zipWith row (zip offs (drop 1 offs)) valid, right (mkMap (validityOf valid) (VS.fromList (map fromIntegral offs)) keys vals))
  TDense tys -> do
    children <- forM tys $ \t -> do
      len <- Gen.int (Range.linear 1 5)
      genColumnOf t len
    picks <- replicateM n $ do
      ci <- Gen.int (Range.linear 0 (length tys - 1))
      off <- Gen.int (Range.linear 0 (length (fst (children !! ci)) - 1))
      pure (ci, off)
    let c =
          right
            ( mkDenseUnion
                (VS.fromList (map (fromIntegral . fst) picks))
                (VS.fromList (map (fromIntegral . snd) picks))
                (V.fromList (map snd children))
            )
    pure (map (\(ci, off) -> VUnion ci (fst (children !! ci) !! off)) picks, c)
  TSparse tys -> do
    children <- forM tys $ \t -> genColumnOf t n
    picks <- replicateM n (Gen.int (Range.linear 0 (length tys - 1)))
    let c = right (mkSparseUnion (VS.fromList (map fromIntegral picks)) (V.fromList (map snd children)))
    pure (zipWith (\i ci -> VUnion ci (fst (children !! ci) !! i)) [0 ..] picks, c)
  TDict (SomePrimType kt) vt -> case integralPrim kt of
    Nothing -> error "key type"
    Just IntegralPrim -> do
      k <- Gen.int (Range.linear 0 6)
      (vm, vals) <- genColumnOf vt k
      ks <- replicateM n (if k == 0 then pure Nothing else maybeOf (Gen.int (Range.linear 0 (k - 1))))
      let keys = fromMaybes kt (V.fromList (map (fmap fromIntegral) ks))
      pure (map (maybe VNull (vm !!)) ks, right (mkDictionary 7 keys vals))
  TRee (SomePrimType rt) vt -> case integralPrim rt of
    Nothing -> error "run end type"
    Just IntegralPrim -> do
      lens <- genParts n
      (vm, vals) <- genColumnOf vt (length lens)
      let re = primColumn rt (VS.fromList (map fromIntegral (scanl1 (+) lens)))
      pure (concat (zipWith replicate lens vm), right (mkRunEndEncoded re vals))


genParts :: Int -> Gen [Int]
genParts n
  | n <= 0 = pure []
  | otherwise = do
      k <- Gen.int (Range.linear 1 (min n 4))
      (k :) <$> genParts (n - k)


-- | Row lengths, a non-zero base, and a child long enough to cover them.
genListParts :: Ty -> Int -> Gen ([Value], [Int], ColumnArray)
genListParts ct n = do
  lens <- replicateM n (Gen.int (Range.linear 0 3))
  base <- Gen.int (Range.linear 0 3)
  extra <- Gen.int (Range.linear 0 2)
  (cm, child) <- genColumnOf ct (base + sum lens + extra)
  pure (cm, scanl (+) base lens, child)


listModel :: [Bool] -> [Value] -> [Int] -> [Value]
listModel valid cm offs = zipWith (\(s, e) ok -> if ok then VList (take (e - s) (drop s cm)) else VNull) (zip offs (drop 1 offs)) valid


-- ============================================================
-- Observation through the public accessors
-- ============================================================

observe :: ColumnArray -> [Value]
observe c = map (rowValue c) [0 .. columnLength c - 1]


rowValue :: ColumnArray -> Int -> Value
rowValue c i
  | not (isValidAt (validity c) i) = VNull
  | otherwise = case c of
      ColNull _ -> VNull
      ColPrim t _ _ -> case asPrim t c of
        Just arr -> withPrim t (maybe VNull (primValue t) (primAt arr i))
        Nothing -> error "asPrim"
      ColBool _ _ -> maybe VNull VBool (boolAt c i)
      ColUtf8 {} -> maybe VNull VText (anyTextAt c i)
      ColLargeUtf8 {} -> maybe VNull VText (anyTextAt c i)
      ColUtf8View {} -> maybe VNull VText (anyTextAt c i)
      ColBinary {} -> maybe VNull VBytes (anyBytesAt c i)
      ColLargeBinary {} -> maybe VNull VBytes (anyBytesAt c i)
      ColBinaryView {} -> maybe VNull VBytes (anyBytesAt c i)
      ColFixedSizeBinary {} -> maybe VNull VBytes (anyBytesAt c i)
      ColStruct _ _ cs -> VStruct (map (\(_, x) -> rowValue x i) (V.toList cs))
      ColList _ _ x -> listOf x
      ColLargeList _ _ x -> listOf x
      ColListView _ _ _ x -> listOf x
      ColLargeListView _ _ _ x -> listOf x
      ColFixedSizeList _ _ _ x -> listOf x
      ColMap _ _ k x -> case listRange c i of
        Just (ChildRange s l) -> VMap (map (\q -> (rowValue k (s + q), rowValue x (s + q))) [0 .. l - 1])
        Nothing -> VNull
      ColDenseUnion t o cs ->
        let ci = fromIntegral (t VS.! i)
        in VUnion ci (rowValue (cs V.! ci) (fromIntegral (o VS.! i)))
      ColSparseUnion t cs ->
        let ci = fromIntegral (t VS.! i)
        in VUnion ci (rowValue (cs V.! ci) i)
      ColDictionary _ _ vals -> maybe VNull (rowValue vals) (dictKeyAt c i)
      ColRunEndEncoded off _ re vals ->
        let ends = runEnds re
        in rowValue vals (length (takeWhile (<= off + i) ends))
  where
    listOf x = case listRange c i of
      Just (ChildRange s l) -> VList (map (rowValue x) [s .. s + l - 1])
      Nothing -> VNull


runEnds :: ColumnArray -> [Int]
runEnds = \case
  ColInt16 _ xs -> map fromIntegral (VS.toList xs)
  ColInt32 _ xs -> map fromIntegral (VS.toList xs)
  ColInt64 _ xs -> map fromIntegral (VS.toList xs)
  _ -> []


-- ============================================================
-- Properties
-- ============================================================

genColumn :: Gen (Ty, [Value], ColumnArray)
genColumn = do
  ty <- genTy 2
  n <- Gen.int (Range.linear 0 12)
  (m, c) <- genColumnOf ty n
  pure (ty, m, c)


-- | Bytes holding @bools@ at bit offset @k@, with random bits around them.
bitmapAt :: Int -> [Bool] -> [Bool] -> [Bool] -> Bitmap
bitmapAt k pre bools post =
  let bits = take k (pre ++ repeat False) ++ bools ++ post
      nbytes = (length bits + 7) `div` 8
      byte j = foldl' (\acc q -> if bitIx (j * 8 + q) then setBit acc q else acc) (0 :: Word8) [0 .. 7]
      bitIx p = p < length bits && bits !! p
  in Bitmap (BS.pack (map byte [0 .. nbytes - 1])) k (length bools)


prop_bitmaps :: Property
prop_bitmaps = withTests 300 . property $ do
  k <- forAll (Gen.int (Range.linear 0 70))
  pre <- forAll (Gen.list (Range.singleton k) Gen.bool)
  bools <- forAll (Gen.list (Range.linear 0 200) Gen.bool)
  post <- forAll (Gen.list (Range.linear 0 9) Gen.bool)
  let b = bitmapAt k pre bools post
      n = length bools
  bitmapToBools b === bools
  map (bitAt b) [0 .. n - 1] === bools
  bitmapSetCount b === length (filter id bools)
  s <- forAll (Gen.int (Range.linear 0 n))
  l <- forAll (Gen.int (Range.linear 0 (n - s)))
  bitmapToBools (sliceBitmap s l b) === take l (drop s bools)
  bitmapSetCount (sliceBitmap s l b) === length (filter id (take l (drop s bools)))
  let cp = copyBitmap b
  bitmapOffset cp === 0
  bitmapToBools cp === bools
  bools2 <- forAll (Gen.list (Range.linear 0 70) Gen.bool)
  k2 <- forAll (Gen.int (Range.linear 0 13))
  let b2 = bitmapAt k2 (replicate k2 True) bools2 []
  bitmapToBools (concatBitmaps [sliceBitmap s l b, b2, b]) === take l (drop s bools) ++ bools2 ++ bools
  ix <- forAll (if n == 0 then pure [] else Gen.list (Range.linear 0 40) (Gen.int (Range.linear 0 (n - 1))))
  let (tb, set) = takeBitmap (VS.fromList ix) b
  bitmapToBools tb === map (bools !!) ix
  set === length (filter (bools !!) ix)
  bitmapToBools (bitmapGenerate n (bools !!)) === bools


prop_validityNormalised :: Property
prop_validityNormalised = withTests 300 . property $ do
  t <- forAll (Gen.element primTypes)
  case t of
    SomePrimType pt -> withPrim pt $ do
      rows <- forAll (Gen.list (Range.linear 0 40) (Gen.frequency [(1, pure Nothing), (6, pure (Just ()))]))
      xs <- forAll (traverse (traverse (const (fst <$> genPrim pt))) rows)
      let c = fromMaybes pt (V.fromList xs)
          nulls = length (filter isNothing xs)
      nullCount c === nulls
      assert (isNothing (validity c) == (nulls == 0))
      -- A window without nulls loses its validity.
      let firstNull = length (takeWhile isJust xs)
      assert (isNothing (validity (sliceColumnArray 0 firstNull c)))
      case validity c of
        Nothing -> success
        Just v -> do
          validityNullCount v === nulls
          assert (isLeft (checkValidity "t" (length xs) (Just (Validity (validityBits v) (nulls + 1)))))
          assert (isLeft (checkValidity "t" (length xs + 1) (Just v)))
          checkValidity "t" (length xs) (Just v) === Right (Just v)
  bools <- forAll (Gen.list (Range.linear 0 50) Gen.bool)
  let b = bitmapFromBools (V.fromList bools)
  fmap validityNullCount (mkValidity b) === (if and bools then Nothing else Just (length (filter not bools)))


prop_observeModel :: Property
prop_observeModel = withTests 500 . property $ do
  (_, m, c) <- forAll genColumn
  columnLength c === length m
  observe c === m


prop_accessors :: Property
prop_accessors = withTests 300 . property $ do
  t <- forAll (Gen.element primTypes)
  n <- forAll (Gen.int (Range.linear 0 30))
  case t of
    SomePrimType pt -> withPrim pt $ do
      (m, c) <- forAll (genColumnOf (TPrim t) n)
      arr <- maybe (failure) pure (asPrim pt c)
      map (fmap (primValue pt) . primAt arr) [0 .. n - 1] === map (\v -> if v == VNull then Nothing else Just v) m
      primAt arr (-1) === Nothing
      primAt arr n === Nothing
      map (fmap (primValue pt)) (VG.toList (toMaybeVector arr)) === map (\v -> if v == VNull then Nothing else Just v) m
      VS.length (toStorable arr) === n
  (tm, tc) <- forAll (genColumnOf TUtf8 n)
  u <- maybe failure pure (asUtf8 tc)
  map (fmap VText . textAt u) [0 .. n - 1] === map (\v -> if v == VNull then Nothing else Just v) tm
  map (fmap (VText . TE.decodeUtf8) . bytesAt (utf8Bytes u)) [0 .. n - 1] === map (\v -> if v == VNull then Nothing else Just v) tm
  textAt u n === Nothing
  fmap VG.toList (toTextVector tc) === Right (map (\case VText x -> Just x; _ -> Nothing) tm)
  (bm, bc) <- forAll (genColumnOf TBool n)
  map (boolAt bc) [-1 .. n] === Nothing : map (\case VBool x -> Just x; _ -> Nothing) bm ++ [Nothing]
  where
    utf8Bytes (Utf8Array b) = b


prop_conversions :: Property
prop_conversions = withTests 500 . property $ do
  n <- forAll (Gen.int (Range.linear 0 30))
  key <- forAll (Gen.element keyTypes)
  w <- forAll (Gen.int (Range.linear 0 3))
  large <- forAll Gen.bool
  let int32 = TPrim (SomePrimType PInt32)
  ty <-
    forAll . Gen.element $
      [ TUtf8, TLargeUtf8, TUtf8View, TDict key TUtf8, TDict key TLargeUtf8
      , TBinary, TLargeBinary, TFixed w, TBinaryView, TDict key TBinary
      , TBool
      , TList int32, TLargeList int32, TListView large int32, TFixedList w int32
      ]
  (m, c) <- forAll (genColumnOf ty n)
  let texts = map (\case VText x -> Just x; _ -> Nothing) m
      bytes = map (\case VText x -> Just (TE.encodeUtf8 x); VBytes x -> Just x; _ -> Nothing) m
      lists = map (\case VList xs -> Just xs; _ -> Nothing) m
      leaf = \case
        TDict _ t -> leaf t
        t -> t
      textKind = case leaf ty of TUtf8 -> True; TLargeUtf8 -> True; TUtf8View -> True; _ -> False
      listKind = case ty of TList _ -> True; TLargeList _ -> True; TListView _ _ -> True; TFixedList _ _ -> True; _ -> False
  case ty of
    TBool -> fmap VG.toList (toBoolVector c) === Right (map (\case VBool x -> Just x; _ -> Nothing) m)
    _
      | listKind -> do
          fmap (map (fmap (map (maybe VNull (VInt . toInteger)) . VG.toList)) . VG.toList) (toListVector int32s c) === Right lists
          -- A child conversion that returns too few rows is an error, not a short slice.
          case ty of
            TList _ | any (maybe False (not . null)) lists -> assert (isLeft (toListVector (fmap (VG.drop 1) . int32s) c))
            _ -> success
      | otherwise -> do
          fmap VG.toList (toBytesVector c) === Right bytes
          if textKind
            then fmap VG.toList (toTextVector c) === Right texts
            else assert (isLeft (toTextVector c))
  where
    int32s ch = maybe (Left ("not an int32 child: " ++ columnTag ch)) (Right . toMaybeVector) (asPrim PInt32 ch)


prop_builders :: Property
prop_builders = withTests 300 . property $ do
  ints <- forAll (Gen.list (Range.linear 0 100) (maybeOf (Gen.int32 Range.linearBounded)))
  ints2 <- forAll (Gen.list (Range.linear 0 20) (maybeOf (Gen.int32 Range.linearBounded)))
  let (c1, c2) = runST $ do
        b <- newPrimBuilder PInt32 3
        mapM_ (appendPrimMaybe b) ints
        x <- freezeBuilder b
        mapM_ (appendPrimMaybe b) ints2
        y <- freezeBuilder b
        pure (x, y)
  c1 === fromMaybes PInt32 (V.fromList ints)
  c2 === fromMaybes PInt32 (V.fromList ints2)
  texts <- forAll (Gen.list (Range.linear 0 60) (maybeOf genText))
  let tc = runST $ do
        b <- newUtf8Builder 1
        mapM_ (appendTextMaybe b) texts
        freezeBuilder b
  tc === fromMaybeTexts (V.fromList texts)
  bytes <- forAll (Gen.list (Range.linear 0 60) (maybeOf genBytes))
  let bc = runST $ do
        b <- newLargeBinaryBuilder 0
        mapM_ (appendBytesMaybe b) bytes
        freezeBuilder b
  bc === fromMaybeLargeByteStrings (V.fromList bytes)
  bools <- forAll (Gen.list (Range.linear 0 100) (maybeOf Gen.bool))
  let boc = runST $ do
        b <- newBoolBuilder 0
        mapM_ (appendBoolMaybe b) bools
        freezeBuilder b
  boc === fromMaybeBools (V.fromList bools)
  nullCount boc === length (filter isNothing bools)


prop_eq :: Property
prop_eq = withTests 400 . property $ do
  ty <- forAll (genTy 2)
  n <- forAll (Gen.int (Range.linear 0 8))
  (m1, c1) <- forAll (genColumnOf ty n)
  (m2, c2) <- forAll (genColumnOf ty n)
  assert (c1 == c1)
  (c1 == c2) === (m1 == m2)
  k <- forAll (Gen.int (Range.linear 0 n))
  rejoined <- evalEither (concatColumnArray (sliceColumnArray 0 k c1) (sliceColumnArray k (n - k) c1))
  assert (rejoined == c1)


prop_slice :: Property
prop_slice = withTests 500 . property $ do
  (_, m, c) <- forAll genColumn
  let n = length m
  s <- forAll (Gen.int (Range.linear (-2) (n + 2)))
  l <- forAll (Gen.int (Range.linear (-2) (n + 2)))
  let s' = min n (max 0 s)
      l' = max 0 (min l (n - s'))
      sl = sliceColumnArray s l c
  columnLength sl === l'
  observe sl === take l' (drop s' m)
  columnTag sl === columnTag c
  -- The IPC layout of a sliced run-end column reads the same rows.
  rebased <- evalEither (rebaseRunEnds sl)
  observe rebased === observe sl
  case rebased of
    ColRunEndEncoded off len re _ -> do
      off === 0
      lastOr 0 (runEnds re) === len
    _ -> success
  where
    lastOr d xs = if null xs then d else last xs


prop_sliceOfSlice :: Property
prop_sliceOfSlice = withTests 300 . property $ do
  (_, m, c) <- forAll genColumn
  let n = length m
  s1 <- forAll (Gen.int (Range.linear 0 n))
  l1 <- forAll (Gen.int (Range.linear 0 (n - s1)))
  s2 <- forAll (Gen.int (Range.linear 0 l1))
  l2 <- forAll (Gen.int (Range.linear 0 (l1 - s2)))
  let a = sliceColumnArray s2 l2 (sliceColumnArray s1 l1 c)
      b = sliceColumnArray (s1 + s2) l2 c
  observe a === observe b
  assert (a == b)


prop_concatAdjacent :: Property
prop_concatAdjacent = withTests 500 . property $ do
  (_, m, c) <- forAll genColumn
  let n = length m
  ks <- forAll (fmap (sortOn id) (Gen.list (Range.linear 0 4) (Gen.int (Range.linear 0 n))))
  let cuts = zip (0 : ks) (ks ++ [n])
  joined <- evalEither (concatColumnArrays (map (\(a, b) -> sliceColumnArray a (b - a) c) cuts))
  columnLength joined === n
  observe joined === m
  assert (joined == c)


prop_concatAppends :: Property
prop_concatAppends = withTests 400 . property $ do
  ty <- forAll (genTy 2)
  ns <- forAll (Gen.list (Range.linear 1 4) (Gen.int (Range.linear 0 8)))
  parts <- forAll (traverse (genColumnOf ty) ns)
  joined <- evalEither (concatColumnArrays (map snd parts))
  columnLength joined === sum ns
  observe joined === concatMap fst parts
  -- A type mismatch is rejected.
  (_, other) <- forAll (genColumnOf TNull 1)
  case ty of
    TNull -> success
    _ -> case parts of
      (p : _) -> assert (isLeft (concatColumnArray (snd p) other))
      [] -> success


prop_take :: Property
prop_take = withTests 400 . property $ do
  (_, m, c) <- forAll genColumn
  let n = length m
  ix <- forAll (if n == 0 then pure [] else Gen.list (Range.linear 0 15) (Gen.int (Range.linear 0 (n - 1))))
  taken <- evalEither (takeColumnArray (VS.fromList ix) c)
  observe taken === map (m !!) ix
  columnTag taken === columnTag c
  assert (isLeft (takeColumnArray (VS.singleton n) c))
  assert (isLeft (takeColumnArray (VS.singleton (-1)) c))


prop_copyColumn :: Property
prop_copyColumn = withTests 300 . property $ do
  (_, m, c) <- forAll genColumn
  let cp = copyColumn c
  observe cp === m
  assert (cp == c)
  let regions = filter (\b -> BS.length b > 0) . columnBuffers
      overlaps x y =
        let (BSI.BS fx lx) = x
            (BSI.BS fy ly) = y
            d = unsafeForeignPtrToPtr fy `minusPtr` unsafeForeignPtrToPtr fx
        in d < lx && negate d < ly
  assert (not (any (\a -> any (overlaps a) (regions c)) (regions cp)))


prop_expandDictionary :: Property
prop_expandDictionary = withTests 300 . property $ do
  kt <- forAll (Gen.element keyTypes)
  vt <- forAll (genTy 0)
  n <- forAll (Gen.int (Range.linear 0 10))
  (m, c) <- forAll (genColumnOf (TDict kt vt) n)
  e <- evalEither (expandDictionary c)
  columnLength e === n
  observe e === m
  assert (columnTag e /= "ColDictionary")


prop_maskValidity :: Property
prop_maskValidity = withTests 300 . property $ do
  (ty, m, c) <- forAll genColumn
  mask <- forAll (Gen.list (Range.singleton (length m)) Gen.bool)
  case maskValidity (validityOf mask) c of
    Left _ -> assert (not (hasValiditySlot c) && not (and mask) && not (isNullTy ty))
    Right masked -> observe masked === zipWith (\ok v -> if ok then v else VNull) mask m
  where
    isNullTy = \case
      TNull -> True
      _ -> False


prop_mapKeys :: Property
prop_mapKeys = withTests 300 . property $ do
  n <- forAll (Gen.int (Range.linear 0 6))
  (m, c) <- forAll (genColumnOf (TMap TBool) n)
  let entrySorted = \case
        VMap kvs ->
          let ks = map fst kvs
          in notElem VNull ks && and (zipWith (\x y -> keyInt x <= keyInt y) ks (drop 1 ks))
        _ -> True
      keyInt = \case
        VInt x -> x
        _ -> 0
  isRight (validateMapKeysSorted c) === all entrySorted m


prop_mkRejects :: Property
prop_mkRejects = withTests 300 . property $ do
  texts <- forAll (Gen.list (Range.linear 1 10) genText)
  let dat = TE.encodeUtf8 (T.concat texts)
      offs = VS.fromList (map fromIntegral (scanl (+) 0 (map (BS.length . TE.encodeUtf8) texts))) :: VS.Vector Int32
      n = length texts
  good <- evalEither (mkUtf8 Nothing offs dat)
  good === fromTexts (V.fromList texts)
  -- Offsets past the data, decreasing, or starting below zero.
  assert (isLeft (mkUtf8 Nothing (VS.snoc offs (fromIntegral (BS.length dat) + 1)) dat))
  assert (isLeft (mkBinary Nothing (VS.fromList [0, 2, 1]) "abc"))
  assert (isLeft (mkBinary Nothing (VS.fromList [-1, 0]) "abc"))
  assert (isLeft (mkBinary Nothing VS.empty "abc"))
  -- Invalid UTF-8, and an offset inside a character.
  assert (isLeft (mkUtf8 Nothing (VS.fromList [0, 2]) (BS.pack [0xC0, 0x80])))
  assert (isLeft (mkUtf8 Nothing (VS.fromList [0, 3]) (BS.pack [0xED, 0xA0, 0x80])))
  assert (isLeft (mkUtf8 Nothing (VS.fromList [0, 1, 2]) (BS.pack [0xC3, 0xA9])))
  assert (isRight (mkUtf8 Nothing (VS.fromList [0, 2]) (BS.pack [0xC3, 0xA9])))
  -- Validity of the wrong length.
  assert (isLeft (mkUtf8 (validityOf (replicate (n + 1) False)) offs dat))
  -- Lists, fixed-size lists, structs, unions, dictionaries, run ends.
  let child = fromTexts (V.fromList texts)
  assert (isLeft (mkList Nothing (VS.fromList [0, fromIntegral n + 1]) child))
  assert (isRight (mkList Nothing (VS.fromList [0, fromIntegral n]) child))
  assert (isLeft (mkFixedSizeList 2 n Nothing child))
  assert (isLeft (mkStruct (n + 1) Nothing (V.singleton ("a", child))))
  assert (isLeft (mkDenseUnion (VS.fromList [0]) (VS.fromList [fromIntegral n]) (V.singleton child)))
  assert (isLeft (mkDenseUnion (VS.fromList [1]) (VS.fromList [0]) (V.singleton child)))
  assert (isLeft (mkSparseUnion (VS.fromList (replicate (n + 1) 0)) (V.singleton child)))
  let keys ks = fromMaybes PInt16 (V.fromList ks)
  assert (isLeft (mkDictionary 1 (keys [Just (fromIntegral n)]) child))
  assert (isLeft (mkDictionary 1 (keys [Just (-1)]) child))
  assert (isRight (mkDictionary 1 (keys [Just 0, Nothing]) child))
  assert (isLeft (mkDictionary 1 child child))
  assert (isLeft (mkRunEndEncoded (primColumn PInt32 (VS.fromList [2, 2])) child))
  assert (isLeft (mkRunEndEncoded (primColumn PInt32 (VS.fromList [0])) child))
  assert (isLeft (mkRunEndEncoded (primColumn PDouble (VS.fromList [1])) child))
  -- Views: a long view pointing past its buffer.
  let longText = T.replicate 4 "abcd"
      ok = fromMaybeUtf8View (V.singleton (Just longText))
  case ok of
    ColUtf8View v views bufs -> do
      assert (isRight (mkUtf8View v views bufs))
      assert (isLeft (mkUtf8View v views (V.map (BS.take 3) bufs)))
      assert (isLeft (mkUtf8View v views V.empty))
      assert (isLeft (mkUtf8View v (BS.take 15 views) bufs))
    _ -> failure
