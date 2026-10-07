{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Hedgehog generators for Arrow schemas with matching column
batches, plus a logical row model ('Value' / 'columnValues') that
compares columns by meaning rather than by physical layout (offsets,
dictionary encoding, run-end encoding, list views and float bit
patterns all reduce to the same row values).

Generated schemas cover every 'ArrowType' the codec supports, nullable
and not, nested to depth 3, with dictionary-encoded fields over every
index type. Generated column batches respect the reader/writer
conventions: a nullable field gets the @*Maybe@ constructor, unions and
run-end-encoded fields have no validity, map keys are non-null.
-}
module Test.Arrow.Gen (
  -- * Schemas and tables
  genField,
  genFieldType,
  genSchema,
  genColumnFor,
  genBatchFor,
  genTable,
  genOrderableKeyField,

  -- * Logical row model
  Value (..),
  columnValues,
  batchValues,
  compareKeyValues,
  hasDictionaries,
) where

import Arrow.Column (ColumnArray (..), takeColumnArray)
import Arrow.Types
import Control.Monad (forM, replicateM)
import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (sortBy)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range


-- ============================================================
-- Schemas
-- ============================================================

-- | A schema of 1 to 4 uniquely named top-level fields, nested up to depth 3.
genSchema :: Gen Schema
genSchema = do
  n <- Gen.int (Range.linear 1 4)
  fs <- forM [0 .. n - 1] $ \i -> genField 3 (T.pack ("c" ++ show i))
  endian <- pure Little
  pure (numberDictionaries (Schema (V.fromList fs) endian V.empty V.empty))


{- | Give every dictionary-encoded field a distinct dictionary id (in
pre-order), so each id names exactly one value type.
-}
numberDictionaries :: Schema -> Schema
numberDictionaries sch =
  let go !next f =
        let (next1, dict') = case fieldDictionary f of
              Just de -> (next + 1, Just de {deId = next})
              Nothing -> (next, Nothing)
            (next2, kids) = goList next1 (V.toList (fieldChildren f))
        in (next2, f {fieldDictionary = dict', fieldChildren = V.fromList kids})
      goList next = \case
        [] -> (next, [])
        (f : fs) ->
          let (n1, f') = go next f
              (n2, fs') = goList n1 fs
          in (n2, f' : fs')
      (_, fields) = goList 0 (V.toList (arrowFields sch))
  in sch {arrowFields = V.fromList fields}


-- | One field named @name@; @depth@ bounds the remaining nesting.
genField :: Int -> Text -> Gen Field
genField depth name =
  Gen.frequency
    [ (8, genLeafField name)
    , (if depth > 0 then 6 else 0, genNestedField depth name)
    , (2, genDictionaryField name)
    ]


leaf :: Text -> Bool -> ArrowType -> Field
leaf name nullable ty = Field name nullable ty V.empty Nothing V.empty


genLeafField :: Text -> Gen Field
genLeafField name = do
  ty <- genFieldType
  nullable <- case ty of
    ANull -> pure True
    _ -> Gen.bool
  pure (leaf name nullable ty)


-- | Any non-nested Arrow type.
genFieldType :: Gen ArrowType
genFieldType =
  Gen.choice
    [ pure ANull
    , AInt <$> Gen.element [8, 16, 32, 64] <*> Gen.bool
    , AFloatingPoint <$> Gen.element [Half, Single, DoublePrecision]
    , pure ABinary
    , pure AUtf8
    , pure ABool
    , ADecimal <$> Gen.int (Range.linear 1 38) <*> Gen.int (Range.linear 0 10)
    , ADecimal256 <$> Gen.int (Range.linear 1 76) <*> Gen.int (Range.linear 0 10)
    , ADate <$> Gen.element [DateDay, DateMillisecond]
    , Gen.element [ATime Second 32, ATime Millisecond 32, ATime Microsecond 64, ATime Nanosecond 64]
    , ATimestamp <$> Gen.element [Second, Millisecond, Microsecond, Nanosecond] <*> Gen.element [Nothing, Just "UTC"]
    , AInterval <$> Gen.element [YearMonth, DayTime, MonthDayNano]
    , AFixedSizeBinary <$> Gen.int (Range.linear 1 20)
    , ADuration <$> Gen.element [Second, Millisecond, Microsecond, Nanosecond]
    , pure ALargeBinary
    , pure ALargeUtf8
    , pure AUtf8View
    , pure ABinaryView
    ]


genNestedField :: Int -> Text -> Gen Field
genNestedField depth name = do
  let child = genField (depth - 1)
  Gen.choice
    [ do
        n <- Gen.int (Range.linear 1 3)
        kids <- forM [0 .. n - 1] $ \i -> child (T.pack ("f" ++ show i))
        nullable <- Gen.bool
        pure (Field name nullable AStruct (V.fromList kids) Nothing V.empty)
    , listLike AList
    , listLike ALargeList
    , listLike AListView
    , listLike ALargeListView
    , do
        w <- Gen.int (Range.linear 1 3)
        listLike (AFixedSizeList w)
    , do
        sorted <- Gen.bool
        k <- if sorted then genOrderableKeyField "key" else (\f -> f {fieldNullable = False}) <$> genLeafKey
        v <- child "value"
        nullable <- Gen.bool
        let entries = Field "entries" False AStruct (V.fromList [k, v]) Nothing V.empty
        pure (Field name nullable (AMap sorted) (V.singleton entries) Nothing V.empty)
    , do
        mode <- Gen.element [Dense, Sparse]
        n <- Gen.int (Range.linear 1 3)
        kids <- forM [0 .. n - 1] $ \i -> child (T.pack ("u" ++ show i))
        ids <-
          Gen.choice
            [ pure V.empty
            , V.fromList . take n <$> Gen.shuffle [3, 7, 11, 0, 5]
            ]
        pure (Field name False (AUnion mode ids) (V.fromList kids) Nothing V.empty)
    , do
        reTy <- Gen.element [AInt 16 True, AInt 32 True, AInt 64 True]
        v <- child "values"
        let v' = case fieldType v of
              ARunEndEncoded -> v {fieldType = AInt 32 True, fieldChildren = V.empty, fieldDictionary = Nothing}
              _ -> v
        pure (Field name False ARunEndEncoded (V.fromList [leaf "run_ends" False reTy, v']) Nothing V.empty)
    ]
  where
    listLike ty = do
      c <- genField (depth - 1) "item"
      nullable <- Gen.bool
      pure (Field name nullable ty (V.singleton c) Nothing V.empty)
    genLeafKey = do
      ty <- Gen.filter (/= ANull) genFieldType
      pure (leaf "key" False ty)


{- | A non-null key field whose 'Value' order agrees with the map-key
order of 'Arrow.Column.validateMapKeysSorted', so sorted keys can be
generated by sorting values.
-}
genOrderableKeyField :: Text -> Gen Field
genOrderableKeyField name =
  leaf name False
    <$> Gen.element
      [ AInt 8 True
      , AInt 16 False
      , AInt 32 True
      , AInt 64 False
      , AUtf8
      , ALargeUtf8
      , ABinary
      , ABool
      , ADate DateDay
      , ATimestamp Microsecond Nothing
      , AFixedSizeBinary 3
      , AFloatingPoint DoublePrecision
      , ADecimal 10 2
      , AUtf8View
      ]


-- | A dictionary-encoded field over a flat value type.
genDictionaryField :: Text -> Gen Field
genDictionaryField name = do
  ty <- Gen.filter (/= ANull) genFieldType
  idx <- AInt <$> Gen.element [8, 16, 32, 64] <*> Gen.bool
  nullable <- Gen.bool
  ordered <- Gen.bool
  pure (Field name nullable ty V.empty (Just (DictionaryEncoding 0 idx ordered)) V.empty)


-- ============================================================
-- Columns
-- ============================================================

{- | One batch (0 to 4 of them) of 0 to 8 rows per table; every
batch draws its own data, so dictionary columns carry different
dictionaries in different batches.
-}
genTable :: Gen (Schema, [V.Vector ColumnArray])
genTable = do
  sch <- genSchema
  nBatches <- Gen.int (Range.linear 0 4)
  batches <- replicateM nBatches (genBatchFor sch)
  pure (sch, batches)


-- | A batch for the schema with 0 to 8 rows.
genBatchFor :: Schema -> Gen (V.Vector ColumnArray)
genBatchFor sch = do
  rows <- Gen.frequency [(1, pure 0), (6, Gen.int (Range.linear 1 8))]
  V.mapM (`genColumnFor` rows) (arrowFields sch)


-- | A column of exactly @n@ rows for the field.
genColumnFor :: Field -> Int -> Gen ColumnArray
genColumnFor f n = case fieldDictionary f of
  Just de -> do
    k <- Gen.int (Range.linear 1 5)
    valuesNullable <- if fieldType f == ANull then pure False else Gen.bool
    vals <- genColumnFor f {fieldDictionary = Nothing, fieldNullable = valuesNullable} k
    ix <- replicateM n (Gen.int32 (Range.linear 0 (fromIntegral k - 1)))
    if fieldNullable f
      then do
        mix <- forM ix $ \i -> Gen.frequency [(1, pure Nothing), (4, pure (Just i))]
        pure (ColDictionaryMaybe (deId de) (V.fromList mix) vals)
      else pure (ColDictionary (deId de) (VP.fromList ix) vals)
  Nothing -> case fieldType f of
    ANull -> pure (ColNull n)
    AInt 8 True -> prim ColInt8 ColInt8Maybe genI8
    AInt 16 True -> prim ColInt16 ColInt16Maybe genI16
    AInt 32 True -> prim ColInt32 ColInt32Maybe genI32
    AInt 64 True -> prim ColInt64 ColInt64Maybe genI64
    AInt 8 False -> prim ColUInt8 ColUInt8Maybe genW8
    AInt 16 False -> prim ColUInt16 ColUInt16Maybe genW16
    AInt 32 False -> prim ColUInt32 ColUInt32Maybe genW32
    AInt 64 False -> prim ColUInt64 ColUInt64Maybe genW64
    AInt w _ -> error ("Test.Arrow.Gen: unsupported int width " ++ show w)
    AFloatingPoint Half -> prim ColFloat16 ColFloat16Maybe genW16
    AFloatingPoint Single -> prim ColFloat ColFloatMaybe genFloat
    AFloatingPoint DoublePrecision -> prim ColDouble ColDoubleMaybe genDouble
    ABinary -> boxed ColBinary ColBinaryMaybe genBytes
    ALargeBinary -> boxed ColLargeBinary ColLargeBinaryMaybe genBytes
    ABinaryView -> boxed ColBinaryView ColBinaryViewMaybe genViewBytes
    AUtf8 -> boxed ColUtf8 ColUtf8Maybe genText
    ALargeUtf8 -> boxed ColLargeUtf8 ColLargeUtf8Maybe genText
    AUtf8View -> boxed ColUtf8View ColUtf8ViewMaybe genViewText
    ABool -> boxed ColBool ColBoolMaybe Gen.bool
    ADecimal p s -> boxed (ColDecimal128 p s) (ColDecimal128Maybe p s) (genFixed 16)
    ADecimal256 p s -> boxed (ColDecimal256 p s) (ColDecimal256Maybe p s) (genFixed 32)
    AFixedSizeBinary w -> boxed (ColFixedSizeBinary w) (ColFixedSizeBinaryMaybe w) (genFixed w)
    ADate DateDay -> prim ColDate32 ColDate32Maybe genI32
    ADate DateMillisecond -> prim ColDate64 ColDate64Maybe genI64
    ATime u _
      | u == Second || u == Millisecond -> prim ColTime32 ColTime32Maybe genI32
      | otherwise -> prim ColTime64 ColTime64Maybe genI64
    ATimestamp _ _ -> prim ColTimestamp ColTimestampMaybe genI64
    ADuration _ -> prim ColDuration ColDurationMaybe genI64
    AInterval YearMonth -> prim ColIntervalYearMonth ColIntervalYearMonthMaybe genI32
    AInterval DayTime ->
      if fieldNullable f
        then ColIntervalDayTimeMaybe <$> maybes ((,) <$> genI32 <*> genI32)
        else do
          xs <- replicateM n ((,) <$> genI32 <*> genI32)
          pure (ColIntervalDayTime (VP.fromList (map fst xs)) (VP.fromList (map snd xs)))
    AInterval MonthDayNano ->
      if fieldNullable f
        then ColIntervalMonthDayNanoMaybe <$> maybes ((,,) <$> genI32 <*> genI32 <*> genI64)
        else do
          xs <- replicateM n ((,,) <$> genI32 <*> genI32 <*> genI64)
          pure
            ( ColIntervalMonthDayNano
                (VP.fromList (map (\(m, _, _) -> m) xs))
                (VP.fromList (map (\(_, d, _) -> d) xs))
                (VP.fromList (map (\(_, _, ns) -> ns) xs))
            )
    AStruct -> do
      kids <- V.mapM (\c -> (,) (fieldName c) <$> genColumnFor c n) (fieldChildren f)
      if fieldNullable f
        then (\v -> ColStructMaybe v kids) <$> validity
        else pure (ColStruct kids)
    AList -> do
      (offs, total) <- genOffsets
      c <- genColumnFor (onlyChild f) total
      withValidity (ColList (VP.fromList (map fromIntegral offs)) c) (\v -> ColListMaybe v (VP.fromList (map fromIntegral offs)) c)
    ALargeList -> do
      (offs, total) <- genOffsets
      c <- genColumnFor (onlyChild f) total
      withValidity (ColLargeList (VP.fromList (map fromIntegral offs)) c) (\v -> ColLargeListMaybe v (VP.fromList (map fromIntegral offs)) c)
    AFixedSizeList w -> do
      c <- genColumnFor (onlyChild f) (n * w)
      withValidity (ColFixedSizeList w c) (\v -> ColFixedSizeListMaybe w v c)
    AListView -> do
      (offs, sizes, c) <- genViews
      withValidity
        (ColListView (VP.fromList (map fromIntegral offs)) (VP.fromList (map fromIntegral sizes)) c)
        (\v -> ColListViewMaybe v (VP.fromList (map fromIntegral offs)) (VP.fromList (map fromIntegral sizes)) c)
    ALargeListView -> do
      (offs, sizes, c) <- genViews
      withValidity
        (ColLargeListView (VP.fromList (map fromIntegral offs)) (VP.fromList (map fromIntegral sizes)) c)
        (\v -> ColLargeListViewMaybe v (VP.fromList (map fromIntegral offs)) (VP.fromList (map fromIntegral sizes)) c)
    AMap sorted -> do
      let entries = onlyChild f
          (kf, vf) = case V.toList (fieldChildren entries) of
            [a, b] -> (a, b)
            _ -> error "Test.Arrow.Gen: map entries must have key and value"
      lens <- replicateM n (Gen.int (Range.linear 0 3))
      let offs = scanl (+) 0 lens
          total = last offs
      keys0 <- genColumnFor kf total
      keys <- if sorted then pure (sortKeysWithin offs keys0) else pure keys0
      vals <- genColumnFor vf total
      let o = VP.fromList (map fromIntegral offs)
      withValidity (ColMap o keys vals) (\v -> ColMapMaybe v o keys vals)
    AUnion mode _ -> do
      let kids = fieldChildren f
          k = V.length kids
      tids <- replicateM n (Gen.int (Range.linear 0 (k - 1)))
      case mode of
        Sparse -> do
          cs <- V.mapM (`genColumnFor` n) kids
          pure (ColSparseUnion (VP.fromList (map fromIntegral tids)) cs)
        Dense -> do
          let counts = map (\c -> length (filter (== c) tids)) [0 .. k - 1]
          extras <- replicateM k (Gen.int (Range.linear 0 1))
          cs <- V.imapM (\i c -> genColumnFor c (counts !! i + extras !! i)) kids
          let offs = denseOffsets k tids
          pure (ColDenseUnion (VP.fromList (map fromIntegral tids)) (VP.fromList offs) cs)
    ARunEndEncoded -> case V.toList (fieldChildren f) of
      [ref, vf] -> do
        runs <- genRuns n
        let ends = drop 1 (scanl (+) 0 runs)
        vals <- genColumnFor vf (length runs)
        let re = case fieldType ref of
              AInt 16 _ -> ColInt16 (VP.fromList (map fromIntegral ends))
              AInt 64 _ -> ColInt64 (VP.fromList (map fromIntegral ends))
              _ -> ColInt32 (VP.fromList (map fromIntegral ends))
        pure (ColRunEndEncoded re vals)
      _ -> error "Test.Arrow.Gen: run-end-encoded field needs two children"
  where
    nullable = fieldNullable f
    maybes :: Gen a -> Gen (V.Vector (Maybe a))
    maybes g = V.fromList <$> replicateM n (Gen.frequency [(1, pure Nothing), (3, Just <$> g)])
    prim :: VP.Prim a => (VP.Vector a -> ColumnArray) -> (V.Vector (Maybe a) -> ColumnArray) -> Gen a -> Gen ColumnArray
    prim con conM g
      | nullable = conM <$> maybes g
      | otherwise = con . VP.fromList <$> replicateM n g
    boxed :: (V.Vector a -> ColumnArray) -> (V.Vector (Maybe a) -> ColumnArray) -> Gen a -> Gen ColumnArray
    boxed con conM g
      | nullable = conM <$> maybes g
      | otherwise = con . V.fromList <$> replicateM n g
    validity = V.fromList <$> replicateM n (Gen.frequency [(1, pure False), (3, pure True)])
    withValidity nonNull mk = if nullable then mk <$> validity else pure nonNull
    -- Offsets may start past zero (a slice of a larger child).
    genOffsets = do
      start <- Gen.int (Range.linear 0 2)
      lens <- replicateM n (Gen.int (Range.linear 0 3))
      tailExtra <- Gen.int (Range.linear 0 1)
      let offs = scanl (+) start lens
      pure (offs, last offs + tailExtra)
    genViews = do
      m <- Gen.int (Range.linear 0 6)
      rows <- replicateM n $ do
        o <- Gen.int (Range.linear 0 m)
        s <- Gen.int (Range.linear 0 (m - o))
        pure (o, s)
      c <- genColumnFor (onlyChild f) m
      pure (map fst rows, map snd rows, c)


onlyChild :: Field -> Field
onlyChild f = case V.toList (fieldChildren f) of
  [c] -> c
  _ -> error ("Test.Arrow.Gen: field " ++ show (fieldName f) ++ " needs exactly one child")


-- | Per-row offsets of a dense union: the i-th row choosing child c gets that child's next slot.
denseOffsets :: Int -> [Int] -> [Int32]
denseOffsets k = go (replicate k (0 :: Int))
  where
    go _ [] = []
    go counters (t : ts) =
      let o = counters !! t
          counters' = zipWith (\i c -> if i == t then c + 1 else c) [0 ..] counters
      in fromIntegral o : go counters' ts


-- | Positive run lengths summing to @n@.
genRuns :: Int -> Gen [Int]
genRuns n
  | n <= 0 = pure []
  | otherwise = do
      r <- Gen.int (Range.linear 1 n)
      rest <- genRuns (n - r)
      pure (r : rest)


-- | Reorder keys so each entry's keys are non-decreasing.
sortKeysWithin :: [Int] -> ColumnArray -> ColumnArray
sortKeysWithin offs keys =
  case columnValues keys of
    Left e -> error e
    Right vs ->
      let entries = zip offs (drop 1 offs)
          perm = concatMap (\(s, e) -> sortBy (\i j -> compareKeyValues (vs V.! i) (vs V.! j)) [s .. e - 1]) entries
      in either error id (takeColumnArray (VP.fromList perm) keys)


-- ============================================================
-- Leaf value generators
-- ============================================================

genI8 :: Gen Int8
genI8 = Gen.frequency [(1, Gen.element [minBound, maxBound, 0, -1]), (4, Gen.int8 Range.linearBounded)]

genI16 :: Gen Int16
genI16 = Gen.frequency [(1, Gen.element [minBound, maxBound, 0, -1]), (4, Gen.int16 Range.linearBounded)]

genI32 :: Gen Int32
genI32 = Gen.frequency [(1, Gen.element [minBound, maxBound, 0, -1]), (4, Gen.int32 Range.linearBounded)]

genI64 :: Gen Int64
genI64 = Gen.frequency [(1, Gen.element [minBound, maxBound, 0, -1]), (4, Gen.int64 Range.linearBounded)]

genW8 :: Gen Word8
genW8 = Gen.frequency [(1, Gen.element [minBound, maxBound]), (4, Gen.word8 Range.linearBounded)]

genW16 :: Gen Word16
genW16 = Gen.frequency [(1, Gen.element [minBound, maxBound, 0x7e00, 0x7c00, 0xfc00, 0x8000]), (4, Gen.word16 Range.linearBounded)]

genW32 :: Gen Word32
genW32 = Gen.frequency [(1, Gen.element [minBound, maxBound]), (4, Gen.word32 Range.linearBounded)]

genW64 :: Gen Word64
genW64 = Gen.frequency [(1, Gen.element [minBound, maxBound]), (4, Gen.word64 Range.linearBounded)]

genFloat :: Gen Float
genFloat =
  Gen.frequency
    [ (1, Gen.element [0 / 0, 1 / 0, -1 / 0, -0.0, 0, 3.4028235e38, -1.1754944e-38])
    , (4, Gen.float (Range.linearFracFrom 0 (-1e10) 1e10))
    ]

genDouble :: Gen Double
genDouble =
  Gen.frequency
    [ (1, Gen.element [0 / 0, 1 / 0, -1 / 0, -0.0, 0, 1.7976931348623157e308, 5.0e-324])
    , (4, Gen.double (Range.linearFracFrom 0 (-1e100) 1e100))
    ]

genBytes :: Gen ByteString
genBytes = Gen.bytes (Range.linear 0 20)

genFixed :: Int -> Gen ByteString
genFixed w = Gen.bytes (Range.singleton w)

genText :: Gen Text
genText = Gen.frequency [(1, pure ""), (4, Gen.text (Range.linear 0 16) Gen.unicode)]

-- | View payloads straddle the 12-byte inline limit.
genViewBytes :: Gen ByteString
genViewBytes = Gen.choice [Gen.bytes (Range.linear 0 12), Gen.bytes (Range.linear 13 40)]

genViewText :: Gen Text
genViewText =
  Gen.choice
    [ Gen.text (Range.linear 0 3) Gen.unicode
    , Gen.text (Range.linear 0 12) Gen.alphaNum
    , Gen.text (Range.linear 5 30) Gen.unicode
    ]


-- ============================================================
-- Logical row model
-- ============================================================

{- | The meaning of one row, independent of physical layout. Floats are
kept as bit patterns so NaN rows compare equal to themselves.
-}
data Value
  = VNull
  | VInt Integer
  | VF16 Word16
  | VF32 Word32
  | VF64 Word64
  | VBool Bool
  | VText Text
  | VBytes ByteString
  | VDecimal Integer
  | VPair Int32 Int32
  | VTriple Int32 Int32 Int64
  | VList [Value]
  | VStruct [(Text, Value)]
  | VMap [(Value, Value)]
  | VUnion Int8 Value
  deriving stock (Show, Eq)


-- | Logical rows of every column of a batch.
batchValues :: V.Vector ColumnArray -> Either String [[Value]]
batchValues = traverse (fmap V.toList . columnValues) . V.toList


-- | Whether any column (at any depth) is dictionary-encoded.
hasDictionaries :: ColumnArray -> Bool
hasDictionaries = \case
  ColDictionary {} -> True
  ColDictionaryMaybe {} -> True
  ColStruct cs -> any (hasDictionaries . snd) cs
  ColStructMaybe _ cs -> any (hasDictionaries . snd) cs
  ColList _ c -> hasDictionaries c
  ColListMaybe _ _ c -> hasDictionaries c
  ColLargeList _ c -> hasDictionaries c
  ColLargeListMaybe _ _ c -> hasDictionaries c
  ColFixedSizeList _ c -> hasDictionaries c
  ColFixedSizeListMaybe _ _ c -> hasDictionaries c
  ColMap _ k v -> hasDictionaries k || hasDictionaries v
  ColMapMaybe _ _ k v -> hasDictionaries k || hasDictionaries v
  ColDenseUnion _ _ cs -> any hasDictionaries cs
  ColSparseUnion _ cs -> any hasDictionaries cs
  ColRunEndEncoded _ v -> hasDictionaries v
  ColListView _ _ c -> hasDictionaries c
  ColListViewMaybe _ _ _ c -> hasDictionaries c
  ColLargeListView _ _ c -> hasDictionaries c
  ColLargeListViewMaybe _ _ _ c -> hasDictionaries c
  _ -> False


-- | Logical rows of a column; any out-of-range reference is a 'Left'.
columnValues :: ColumnArray -> Either String (V.Vector Value)
columnValues col = case col of
  ColInt8 v -> ints v
  ColInt16 v -> ints v
  ColInt32 v -> ints v
  ColInt64 v -> ints v
  ColUInt8 v -> ints v
  ColUInt16 v -> ints v
  ColUInt32 v -> ints v
  ColUInt64 v -> ints v
  ColDate32 v -> ints v
  ColDate64 v -> ints v
  ColTime32 v -> ints v
  ColTime64 v -> ints v
  ColTimestamp v -> ints v
  ColDuration v -> ints v
  ColIntervalYearMonth v -> ints v
  ColFloat16 v -> Right (V.map VF16 (V.convert v))
  ColFloat v -> Right (V.map (VF32 . castFloatToWord32) (V.convert v))
  ColDouble v -> Right (V.map (VF64 . castDoubleToWord64) (V.convert v))
  ColBool v -> Right (V.map VBool v)
  ColUtf8 v -> Right (V.map VText v)
  ColLargeUtf8 v -> Right (V.map VText v)
  ColUtf8View v -> Right (V.map VText v)
  ColBinary v -> Right (V.map VBytes v)
  ColLargeBinary v -> Right (V.map VBytes v)
  ColBinaryView v -> Right (V.map VBytes v)
  ColFixedSizeBinary _ v -> Right (V.map VBytes v)
  ColDecimal128 _ _ v -> Right (V.map decimal v)
  ColDecimal256 _ _ v -> Right (V.map decimal v)
  ColIntervalDayTime ds ms -> Right (V.zipWith VPair (V.convert ds) (V.convert ms))
  ColIntervalMonthDayNano ms ds ns -> Right (V.zipWith3 VTriple (V.convert ms) (V.convert ds) (V.convert ns))
  ColInt8Maybe v -> mInts v
  ColInt16Maybe v -> mInts v
  ColInt32Maybe v -> mInts v
  ColInt64Maybe v -> mInts v
  ColUInt8Maybe v -> mInts v
  ColUInt16Maybe v -> mInts v
  ColUInt32Maybe v -> mInts v
  ColUInt64Maybe v -> mInts v
  ColDate32Maybe v -> mInts v
  ColDate64Maybe v -> mInts v
  ColTime32Maybe v -> mInts v
  ColTime64Maybe v -> mInts v
  ColTimestampMaybe v -> mInts v
  ColDurationMaybe v -> mInts v
  ColIntervalYearMonthMaybe v -> mInts v
  ColFloat16Maybe v -> m VF16 v
  ColFloatMaybe v -> m (VF32 . castFloatToWord32) v
  ColDoubleMaybe v -> m (VF64 . castDoubleToWord64) v
  ColBoolMaybe v -> m VBool v
  ColUtf8Maybe v -> m VText v
  ColLargeUtf8Maybe v -> m VText v
  ColUtf8ViewMaybe v -> m VText v
  ColBinaryMaybe v -> m VBytes v
  ColLargeBinaryMaybe v -> m VBytes v
  ColBinaryViewMaybe v -> m VBytes v
  ColFixedSizeBinaryMaybe _ v -> m VBytes v
  ColDecimal128Maybe _ _ v -> m decimal v
  ColDecimal256Maybe _ _ v -> m decimal v
  ColIntervalDayTimeMaybe v -> m (uncurry VPair) v
  ColIntervalMonthDayNanoMaybe v -> m (\(a, b, c) -> VTriple a b c) v
  ColNull n -> Right (V.replicate n VNull)
  ColStruct cs -> do
    kids <- V.mapM (traverse columnValues) cs
    let n = if V.null kids then 0 else V.length (snd (V.head kids))
    Right (V.generate n (\i -> VStruct (V.toList (V.map (\(nm, vs) -> (nm, vs V.! i)) kids))))
  ColStructMaybe valid cs -> do
    kids <- V.mapM (traverse columnValues) cs
    masked valid (\i -> VStruct (V.toList (V.map (\(nm, vs) -> (nm, vs V.! i)) kids)))
  ColList o c -> listRows Nothing (VP.map fromIntegral o) c
  ColListMaybe valid o c -> listRows (Just valid) (VP.map fromIntegral o) c
  ColLargeList o c -> listRows Nothing (VP.map fromIntegral o) c
  ColLargeListMaybe valid o c -> listRows (Just valid) (VP.map fromIntegral o) c
  ColFixedSizeList w c -> fixedRows Nothing w c
  ColFixedSizeListMaybe w valid c -> fixedRows (Just valid) w c
  ColMap o k v -> mapRows Nothing o k v
  ColMapMaybe valid o k v -> mapRows (Just valid) o k v
  ColListView o s c -> viewRows Nothing (VP.map fromIntegral o) (VP.map fromIntegral s) c
  ColListViewMaybe valid o s c -> viewRows (Just valid) (VP.map fromIntegral o) (VP.map fromIntegral s) c
  ColLargeListView o s c -> viewRows Nothing (VP.map fromIntegral o) (VP.map fromIntegral s) c
  ColLargeListViewMaybe valid o s c -> viewRows (Just valid) (VP.map fromIntegral o) (VP.map fromIntegral s) c
  ColDenseUnion ts offs cs -> do
    kids <- V.mapM columnValues cs
    V.generateM (VP.length ts) $ \i -> do
      let t = VP.unsafeIndex ts i
      kid <- at "dense union child" kids (fromIntegral t)
      VUnion t <$> at "dense union offset" kid (fromIntegral (VP.unsafeIndex offs i))
  ColSparseUnion ts cs -> do
    kids <- V.mapM columnValues cs
    V.generateM (VP.length ts) $ \i -> do
      let t = VP.unsafeIndex ts i
      kid <- at "sparse union child" kids (fromIntegral t)
      VUnion t <$> at "sparse union row" kid i
  ColDictionary _ ix vals -> do
    vs <- columnValues vals
    V.mapM (at "dictionary index" vs . fromIntegral) (V.convert ix)
  ColDictionaryMaybe _ ix vals -> do
    vs <- columnValues vals
    V.mapM (maybe (Right VNull) (at "dictionary index" vs . fromIntegral)) ix
  ColRunEndEncoded re vals -> do
    ends <- columnValues re
    vs <- columnValues vals
    let endInts = map (\case VInt e -> fromIntegral e; _ -> 0 :: Int) (V.toList ends)
        runs = zip endInts (0 : endInts)
    rows <- forM (zip [0 ..] runs) $ \(r, (e, s)) -> do
      x <- at "run value" vs r
      Right (replicate (e - s) x)
    Right (V.fromList (concat rows))
  where
    ints :: (VP.Prim a, Integral a) => VP.Vector a -> Either String (V.Vector Value)
    ints v = Right (V.map (VInt . toInteger) (V.convert v))
    mInts :: Integral a => V.Vector (Maybe a) -> Either String (V.Vector Value)
    mInts = m (VInt . toInteger)
    m :: (a -> Value) -> V.Vector (Maybe a) -> Either String (V.Vector Value)
    m f v = Right (V.map (maybe VNull f) v)
    masked valid row = Right (V.imap (\i ok -> if ok then row i else VNull) valid)
    listRows mvalid offs c = do
      vs <- columnValues c
      let n = max 0 (VP.length offs - 1)
      V.generateM n $ \i ->
        if maybe True (V.! i) mvalid
          then VList <$> slice vs (VP.unsafeIndex offs i) (VP.unsafeIndex offs (i + 1) - VP.unsafeIndex offs i)
          else Right VNull
    viewRows mvalid offs sizes c = do
      vs <- columnValues c
      V.generateM (VP.length offs) $ \i ->
        if maybe True (V.! i) mvalid
          then VList <$> slice vs (VP.unsafeIndex offs i) (VP.unsafeIndex sizes i)
          else Right VNull
    fixedRows mvalid w c = do
      vs <- columnValues c
      let n = maybe (if w > 0 then V.length vs `div` w else 0) V.length mvalid
      V.generateM n $ \i ->
        if maybe True (V.! i) mvalid
          then VList <$> slice vs (i * w) w
          else Right VNull
    mapRows mvalid offs k v = do
      ks <- columnValues k
      vs <- columnValues v
      let n = max 0 (VP.length offs - 1)
      V.generateM n $ \i -> do
        let s = fromIntegral (VP.unsafeIndex offs i)
            e = fromIntegral (VP.unsafeIndex offs (i + 1))
        if maybe True (V.! i) mvalid
          then do
            kk <- slice ks s (e - s)
            vv <- slice vs s (e - s)
            Right (VMap (zip kk vv))
          else Right VNull
    slice :: V.Vector Value -> Int -> Int -> Either String [Value]
    slice vs s l
      | s < 0 || l < 0 || s + l > V.length vs = Left ("Test.Arrow.Gen: slice out of range " ++ show (s, l, V.length vs))
      | otherwise = Right (V.toList (V.slice s l vs))
    at :: String -> V.Vector a -> Int -> Either String a
    at what vs i = maybe (Left ("Test.Arrow.Gen: " ++ what ++ " out of range")) Right (vs V.!? i)


-- | Little-endian two's-complement bytes as an integer.
decimal :: ByteString -> Value
decimal bs =
  let unsigned = BS.foldr (\b acc -> acc `shiftL` 8 .|. toInteger b) 0 bs
      bits = 8 * BS.length bs
  in VDecimal (if bits > 0 && testBit unsigned (bits - 1) then unsigned - (1 `shiftL` bits) else unsigned)


{- | The map-key order of 'Arrow.Column.validateMapKeysSorted', on
model values: numbers numerically (floats with -0 == 0 and every NaN
equal and largest), text by code point, bytes unsigned-lexicographic,
booleans False first.
-}
compareKeyValues :: Value -> Value -> Ordering
compareKeyValues a b = case (a, b) of
  (VInt x, VInt y) -> compare x y
  (VDecimal x, VDecimal y) -> compare x y
  (VF16 x, VF16 y) -> floating (halfToDouble x) (halfToDouble y)
  (VF32 x, VF32 y) -> floating (realToFrac (castWord32ToFloat x)) (realToFrac (castWord32ToFloat y))
  (VF64 x, VF64 y) -> floating (castWord64ToDouble x) (castWord64ToDouble y)
  (VText x, VText y) -> compare (TE.encodeUtf8 x) (TE.encodeUtf8 y)
  (VBytes x, VBytes y) -> compare x y
  (VBool x, VBool y) -> compare x y
  _ -> EQ
  where
    floating :: Double -> Double -> Ordering
    floating x y = case (isNaN x, isNaN y) of
      (True, True) -> EQ
      (True, False) -> GT
      (False, True) -> LT
      (False, False) -> compare x y


halfToDouble :: Word16 -> Double
halfToDouble w =
  let sign = if w .&. 0x8000 /= 0 then -1 else 1
      ex = fromIntegral ((w `shiftR` 10) .&. 0x1f) :: Int
      mant = fromIntegral (w .&. 0x3ff) :: Double
  in case ex of
       0 -> sign * mant * 2 ** (-24)
       31 -> if mant == 0 then sign * (1 / 0) else 0 / 0
       _ -> sign * (1 + mant / 1024) * 2 ^^ (ex - 15)
