{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Hedgehog generators for Arrow schemas with matching column
batches, plus a logical row model ('Value' / 'columnValues') that
compares columns by meaning rather than by physical layout (offsets,
dictionary encoding, run-end encoding, list views and float bit
patterns all reduce to the same row values).

Generated schemas cover every 'ArrowType' the codec supports, nullable
and not, nested to depth 3, with dictionary-encoded fields over every
index type whose value types may themselves be nested and contain
dictionary-encoded fields. Structs may have no fields and fixed-size
lists may have size 0.

Columns are built through the public construction API only: the
@from*@ conversions, the growable builders and the validating @mk*@
constructors (with arbitrary bytes in null slots and around the
referenced ranges). On top of that the generated physical layouts are
deliberately untidy, so the writer's rebase and realign paths run:

* columns are often a window of a longer column ('sliceColumnArray'),
  so bitmaps start at non-zero bit offsets, offsets at non-zero bases,
  run-end-encoded columns carry a logical offset and struct children
  are longer than the struct;
* var-length data carries junk (even invalid UTF-8) outside the
  referenced range, struct, fixed-size-list and sparse-union children
  carry extra rows, list offsets start past zero;
* with 'layoutDenormalised', a nullable column without nulls sometimes
  carries an all-valid validity bitmap with a zero null count.

Unions and run-end-encoded fields have no validity, map keys are
non-null, and a nullable dictionary column whose rows are all null may
reference an empty dictionary.
-}
module Test.Arrow.Gen (
  -- * Schemas and tables
  genField,
  genFieldType,
  genSchema,
  genColumnFor,
  genColumnForWith,
  genBatchFor,
  genTable,
  genOrderableKeyField,
  genDictionaryField,
  keyColumn,
  Layout (..),
  tidyLayout,
  untidyLayout,

  -- * Logical row model
  Value (..),
  columnValues,
  batchValues,
  compareKeyValues,
  hasDictionaries,
  childColumns,
) where

import Arrow.Column (
  ColumnArray,
  Decimal128 (..),
  Decimal256 (..),
  Float16 (..),
  IntervalDayTime (..),
  IntervalMonthDayNano (..),
  PrimType (..),
  SomePrimType (..),
  appendBoolMaybe,
  appendBytesMaybe,
  appendPrimMaybe,
  appendTextMaybe,
  bitAt,
  bitmapFromBools,
  bitmapGenerate,
  bitmapLength,
  decimal128ToInteger,
  decimal256ToInteger,
  freezeBuilder,
  fromBools,
  fromByteStrings,
  fromMaybeBinaryView,
  fromMaybeBools,
  fromMaybeByteStrings,
  fromMaybeFixedSizeBinary,
  fromMaybeLargeByteStrings,
  fromMaybeLargeTexts,
  fromMaybeTexts,
  fromMaybeUtf8View,
  fromMaybes,
  fromTexts,
  isValidAt,
  mkBinary,
  mkBool,
  mkDenseUnion,
  mkDictionary,
  mkFixedSizeBinary,
  mkFixedSizeList,
  mkLargeBinary,
  mkLargeList,
  mkLargeListView,
  mkLargeUtf8,
  mkList,
  mkListView,
  mkMap,
  mkPrim,
  mkRunEndEncoded,
  mkSparseUnion,
  mkStruct,
  mkUtf8,
  newBinaryBuilder,
  newBoolBuilder,
  newLargeBinaryBuilder,
  newLargeUtf8Builder,
  newPrimBuilder,
  newUtf8Builder,
  primColumn,
  primTypeFor,
  sliceColumnArray,
  takeColumnArray,
  validityFromBools,
  withPrim,
  pattern ColBinary,
  pattern ColBinaryView,
  pattern ColBool,
  pattern ColDenseUnion,
  pattern ColDictionary,
  pattern ColFixedSizeBinary,
  pattern ColFixedSizeList,
  pattern ColLargeBinary,
  pattern ColLargeList,
  pattern ColLargeListView,
  pattern ColLargeUtf8,
  pattern ColList,
  pattern ColListView,
  pattern ColMap,
  pattern ColNull,
  pattern ColPrim,
  pattern ColRunEndEncoded,
  pattern ColSparseUnion,
  pattern ColStruct,
  pattern ColUtf8,
  pattern ColUtf8View,
 )
import Arrow.Column.Internal qualified as I
import Arrow.Types
import Control.Monad (forM, replicateM)
import Control.Monad.ST (runST)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (sortBy)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range


-- ============================================================
-- Schemas
-- ============================================================

{- | A schema of 1 to 4 uniquely named top-level fields, nested up to
depth 3, plus (in a third of the schemas) one field of a shape that
random nesting reaches rarely (see 'genEdgeField').
-}
genSchema :: Gen Schema
genSchema = do
  n <- Gen.int (Range.linear 1 4)
  fs <- forM [0 .. n - 1] $ \i -> genField 3 (T.pack ("c" ++ show i))
  edge <- Gen.frequency [(2, pure []), (1, (: []) <$> genEdgeField (T.pack ("c" ++ show n)))]
  pure (numberDictionaries (Schema (V.fromList (fs ++ edge)) Little V.empty V.empty))


{- | A struct with no fields, a fixed-size list of size 0, a dictionary
whose struct or list values contain a dictionary field, or a nullable
dictionary (whose columns are sometimes all null over an empty
dictionary).
-}
genEdgeField :: Text -> Gen Field
genEdgeField name = do
  nullable <- Gen.bool
  Gen.choice
    [ pure (Field name nullable AStruct V.empty Nothing V.empty)
    , do
        c <- genField 1 "item"
        pure (Field name nullable (AFixedSizeList 0) (V.singleton c) Nothing V.empty)
    , do
        inner <- genDictionaryField 0 "e"
        container <- Gen.element [AStruct, AList, ALargeList]
        idx <- genIndexType
        pure (Field name nullable container (V.singleton inner) (Just (DictionaryEncoding 0 idx False)) V.empty)
    , (\f -> f {fieldNullable = True}) <$> genDictionaryField 0 name
    ]


genIndexType :: Gen ArrowType
genIndexType = AInt <$> Gen.element [8, 16, 32, 64] <*> Gen.bool


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
    , (2, genDictionaryField depth name)
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
        n <- Gen.frequency [(1, pure 0), (3, Gen.int (Range.linear 1 3))]
        kids <- forM [0 .. n - 1] $ \i -> child (T.pack ("f" ++ show i))
        nullable <- Gen.bool
        pure (Field name nullable AStruct (V.fromList kids) Nothing V.empty)
    , listLike AList
    , listLike ALargeList
    , listLike AListView
    , listLike ALargeListView
    , do
        w <- Gen.frequency [(1, pure 0), (3, Gen.int (Range.linear 1 3))]
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


{- | A dictionary-encoded field. The value type is flat or, while
@depth@ allows, nested (struct, list, map, union, ...), so dictionary
fields can sit inside dictionary values.
-}
genDictionaryField :: Int -> Text -> Gen Field
genDictionaryField depth name = do
  value <-
    Gen.frequency
      [ (1, leaf name False <$> Gen.filter (/= ANull) genFieldType)
      , (if depth > 0 then 1 else 0, genNestedField depth name)
      ]
  idx <- genIndexType
  nullable <- Gen.bool
  ordered <- Gen.bool
  pure value {fieldNullable = nullable, fieldDictionary = Just (DictionaryEncoding 0 idx ordered)}


-- ============================================================
-- Columns
-- ============================================================

-- | How untidy generated physical layouts may be.
data Layout = Layout
  { layoutSlices :: !Bool
  -- ^ build longer columns and slice a window out of them
  , layoutDenormalised :: !Bool
  -- ^ sometimes give a nullable column without nulls an all-valid validity
  }
  deriving stock (Show, Eq)


-- | Every column is built exactly as long as it needs to be.
tidyLayout :: Layout
tidyLayout = Layout {layoutSlices = False, layoutDenormalised = False}


-- | Windows of longer columns and denormalised validity.
untidyLayout :: Layout
untidyLayout = Layout {layoutSlices = True, layoutDenormalised = True}


{- | One batch (0 to 4 of them) of 0 to 8 rows per table; every
batch draws its own data, so dictionary columns carry different
dictionaries in different batches. Layouts are 'untidyLayout'.
-}
genTable :: Gen (Schema, [V.Vector ColumnArray])
genTable = do
  sch <- genSchema
  nBatches <- Gen.int (Range.linear 0 4)
  batches <- replicateM nBatches (genBatchFor sch)
  pure (sch, batches)


-- | A batch for the schema with 0 to 8 rows ('untidyLayout').
genBatchFor :: Schema -> Gen (V.Vector ColumnArray)
genBatchFor sch = do
  rows <- Gen.frequency [(1, pure 0), (6, Gen.int (Range.linear 1 8))]
  V.mapM (\f -> genColumnForWith untidyLayout f rows) (arrowFields sch)


{- | A column of exactly @n@ rows for the field, often a window of a
longer column (but never with denormalised validity, so every core
row operation's contract applies).
-}
genColumnFor :: Field -> Int -> Gen ColumnArray
genColumnFor = genColumnForWith untidyLayout {layoutDenormalised = False}


-- | A column of exactly @n@ rows for the field under a layout policy.
genColumnForWith :: Layout -> Field -> Int -> Gen ColumnArray
genColumnForWith layout f n
  | layoutSlices layout = do
      pad <- Gen.frequency [(3, pure Nothing), (2, Just <$> ((,) <$> Gen.int (Range.linear 0 3) <*> Gen.int (Range.linear 0 3)))]
      case pad of
        Nothing -> buildColumn layout f n >>= denormalise
        Just (pre, post) -> do
          c <- buildColumn layout f (pre + n + post)
          denormalise (sliceColumnArray pre n c)
  | otherwise = buildColumn layout f n >>= denormalise
  where
    denormalise c
      | layoutDenormalised layout && fieldNullable f = Gen.frequency [(5, pure c), (1, pure (allValidValidity c))]
      | otherwise = pure c


{- | Replace an absent validity of a flat column by an all-valid bitmap
with null count 0 (raw construction: the public API never builds this).
Other columns are returned unchanged.
-}
allValidValidity :: ColumnArray -> ColumnArray
allValidValidity c = case c of
  I.ColPrim t Nothing xs -> withPrim t (I.ColPrim t (allValid (VS.length xs)) xs)
  I.ColBool Nothing bits -> I.ColBool (allValid (bitmapLength bits)) bits
  I.ColUtf8 Nothing o d -> I.ColUtf8 (allValid (VS.length o - 1)) o d
  I.ColBinary Nothing o d -> I.ColBinary (allValid (VS.length o - 1)) o d
  I.ColLargeUtf8 Nothing o d -> I.ColLargeUtf8 (allValid (VS.length o - 1)) o d
  I.ColLargeBinary Nothing o d -> I.ColLargeBinary (allValid (VS.length o - 1)) o d
  I.ColFixedSizeBinary w n Nothing d -> I.ColFixedSizeBinary w n (allValid n) d
  I.ColList Nothing o x -> I.ColList (allValid (VS.length o - 1)) o x
  I.ColStruct n Nothing kids -> I.ColStruct n (allValid n) kids
  _ -> c
  where
    allValid k = Just (I.Validity (bitmapGenerate k (const True)) 0)


-- | Build a column of exactly @n@ rows (before any windowing by the caller).
buildColumn :: Layout -> Field -> Int -> Gen ColumnArray
buildColumn layout f n = case fieldDictionary f of
  Just de -> do
    emptyDict <- if fieldNullable f then Gen.frequency [(3, pure False), (1, pure True)] else pure False
    k <- if emptyDict then pure 0 else Gen.int (Range.linear 1 5)
    valuesNullable <- if fieldType f == ANull then pure False else Gen.bool
    vals <- sub f {fieldDictionary = Nothing, fieldNullable = valuesNullable} k
    keys <-
      if emptyDict
        then pure (V.replicate n Nothing)
        else V.fromList <$> replicateM n (nullableRow (Gen.int (Range.linear 0 (k - 1))))
    built (mkDictionary (deId de) (keyColumn (deIndexType de) keys) vals)
  Nothing -> case fieldType f of
    ANull -> pure (ColNull n)
    AInt w _ | w `notElem` [8, 16, 32, 64] -> error ("Test.Arrow.Gen: unsupported int width " ++ show w)
    ABinary -> varBytes mkBinary (Just fromByteStrings) fromMaybeByteStrings binaryViaBuilder genBytes
    ALargeBinary -> varBytes mkLargeBinary Nothing fromMaybeLargeByteStrings largeBinaryViaBuilder genBytes
    AUtf8 -> varText mkUtf8 (Just fromTexts) fromMaybeTexts utf8ViaBuilder
    ALargeUtf8 -> varText mkLargeUtf8 Nothing fromMaybeLargeTexts largeUtf8ViaBuilder
    ABinaryView -> fromMaybeBinaryView . V.fromList <$> replicateM n (nullableRow genViewBytes)
    AUtf8View -> fromMaybeUtf8View . V.fromList <$> replicateM n (nullableRow genViewText)
    ABool -> do
      bools <- replicateM n Gen.bool
      mvalid <- genValidityBools
      method <- Gen.int (Range.linear 0 2)
      let rows = rowsOf mvalid bools
      case method of
        0 -> pure (maybe (fromBools (V.fromList bools)) (const (fromMaybeBools (V.fromList rows))) mvalid)
        1 -> pure $ runST $ do
          b <- newBoolBuilder n
          mapM_ (appendBoolMaybe b) rows
          freezeBuilder b
        _ -> built (mkBool (validityOf mvalid) (bitmapFromBools (V.fromList bools)))
    AFixedSizeBinary w -> do
      payloads <- replicateM n (genFixed w)
      mvalid <- genValidityBools
      useMk <- Gen.bool
      if useMk
        then do
          junk <- genBytes
          built (mkFixedSizeBinary w n (validityOf mvalid) (BS.concat payloads <> junk))
        else built (fromMaybeFixedSizeBinary w (V.fromList (rowsOf mvalid payloads)))
    AStruct -> do
      extra <- genExtraRows
      kids <- V.mapM (\c -> (,) (fieldName c) <$> sub c (n + extra)) (fieldChildren f)
      mvalid <- genValidityBools
      built (mkStruct n (validityOf mvalid) kids)
    AList -> do
      (offs, total) <- genOffsets
      c <- sub (onlyChild f) total
      mvalid <- genValidityBools
      built (mkList (validityOf mvalid) (VS.fromList (map fromIntegral offs)) c)
    ALargeList -> do
      (offs, total) <- genOffsets
      c <- sub (onlyChild f) total
      mvalid <- genValidityBools
      built (mkLargeList (validityOf mvalid) (VS.fromList (map fromIntegral offs)) c)
    AFixedSizeList w -> do
      extra <- genExtraRows
      c <- sub (onlyChild f) (n * w + extra)
      mvalid <- genValidityBools
      built (mkFixedSizeList w n (validityOf mvalid) c)
    AListView -> do
      (offs, sizes, c) <- genViews
      mvalid <- genValidityBools
      built (mkListView (validityOf mvalid) (VS.fromList (map fromIntegral offs)) (VS.fromList (map fromIntegral sizes)) c)
    ALargeListView -> do
      (offs, sizes, c) <- genViews
      mvalid <- genValidityBools
      built (mkLargeListView (validityOf mvalid) (VS.fromList (map fromIntegral offs)) (VS.fromList (map fromIntegral sizes)) c)
    AMap sorted -> do
      let entries = onlyChild f
          (kf, vf) = case V.toList (fieldChildren entries) of
            [a, b] -> (a, b)
            _ -> error "Test.Arrow.Gen: map entries must have key and value"
      start <- Gen.frequency [(3, pure 0), (1, Gen.int (Range.linear 1 2))]
      lens <- replicateM n (Gen.int (Range.linear 0 3))
      extra <- genExtraRows
      let offs = scanl (+) start lens
          total = last offs + extra
      keys0 <- sub kf total
      let keys = if sorted then sortKeysWithin offs keys0 else keys0
      vals <- sub vf total
      mvalid <- genValidityBools
      built (mkMap (validityOf mvalid) (VS.fromList (map fromIntegral offs)) keys vals)
    AUnion mode _ -> do
      let kids = fieldChildren f
          k = V.length kids
      tids <- replicateM n (Gen.int (Range.linear 0 (k - 1)))
      let types = VS.fromList (map fromIntegral tids)
      case mode of
        Sparse -> do
          extra <- genExtraRows
          cs <- V.mapM (`sub` (n + extra)) kids
          built (mkSparseUnion types cs)
        Dense -> do
          let counts = map (\c -> length (filter (== c) tids)) [0 .. k - 1]
          extras <- replicateM k (Gen.int (Range.linear 0 1))
          cs <- V.imapM (\i c -> sub c (counts !! i + extras !! i)) kids
          built (mkDenseUnion types (VS.fromList (denseOffsets k tids)) cs)
    ARunEndEncoded -> case V.toList (fieldChildren f) of
      [ref, vf] -> do
        runs <- genRuns n
        let ends = drop 1 (scanl (+) 0 runs)
        vals <- sub vf (length runs)
        let re = case fieldType ref of
              AInt 16 _ -> primColumn PInt16 (VS.fromList (map fromIntegral ends))
              AInt 64 _ -> primColumn PInt64 (VS.fromList (map fromIntegral ends))
              _ -> primColumn PInt32 (VS.fromList (map fromIntegral ends))
        built (mkRunEndEncoded re vals)
      _ -> error "Test.Arrow.Gen: run-end-encoded field needs two children"
    ty -> case primTypeFor ty of
      Just (SomePrimType t) -> primCol t
      Nothing -> error ("Test.Arrow.Gen: no generator for " ++ show ty)
  where
    nullable = fieldNullable f
    sub = genColumnForWith layout
    built = either (\e -> error ("Test.Arrow.Gen: generated column rejected: " ++ e)) pure

    -- Validity as bools (True = valid); nullable columns are sometimes null-free.
    genValidityBools :: Gen (Maybe [Bool])
    genValidityBools
      | not nullable = pure Nothing
      | otherwise =
          Gen.frequency
            [ (1, pure (Just (replicate n True)))
            , (9, Just <$> replicateM n (Gen.frequency [(1, pure False), (2, pure True)]))
            ]
    validityOf = maybe Nothing (validityFromBools . V.fromList)
    rowsOf :: Maybe [Bool] -> [a] -> [Maybe a]
    rowsOf mvalid xs = maybe (map Just xs) (zipWith (\x ok -> if ok then Just x else Nothing) xs) mvalid
    nullableRow :: Gen a -> Gen (Maybe a)
    nullableRow g
      | nullable = Gen.frequency [(1, pure Nothing), (2, Just <$> g)]
      | otherwise = Just <$> g

    -- Fixed width: fromMaybes, a builder, or mkPrim with arbitrary null slots.
    primCol :: PrimType a -> Gen ColumnArray
    primCol t = withPrim t $ do
      xs <- replicateM n (genPrimValue t)
      mvalid <- genValidityBools
      method <- Gen.int (Range.linear 0 2)
      let rows = rowsOf mvalid xs
      case method of
        0 -> pure (maybe (primColumn t (VS.fromList xs)) (const (fromMaybes t (V.fromList rows))) mvalid)
        1 -> pure $ runST $ do
          b <- newPrimBuilder t n
          mapM_ (appendPrimMaybe b) rows
          freezeBuilder b
        _ -> built (mkPrim t (validityOf mvalid) (VS.fromList xs))

    -- Var-length bytes: from*, a builder, or mk* over data with junk
    -- (arbitrary bytes) before and after the referenced range.
    varBytes ::
      (Num o, Storable o) =>
      (Maybe I.Validity -> VS.Vector o -> ByteString -> Either String ColumnArray) ->
      Maybe (V.Vector ByteString -> ColumnArray) ->
      (V.Vector (Maybe ByteString) -> ColumnArray) ->
      ([Maybe ByteString] -> ColumnArray) ->
      Gen ByteString ->
      Gen ColumnArray
    varBytes mk fromAll fromSome viaBuilder genPayload = do
      payloads <- replicateM n genPayload
      mvalid <- genValidityBools
      method <- Gen.int (Range.linear 0 2)
      let rows = rowsOf mvalid payloads
      case method of
        0 -> pure $ case (mvalid, fromAll) of
          (Nothing, Just g) -> g (V.fromList payloads)
          _ -> fromSome (V.fromList rows)
        1 -> pure (viaBuilder rows)
        _ -> do
          prefix <- genJunk
          suffix <- genJunk
          let offs = scanl (+) (BS.length prefix) (map BS.length payloads)
          built (mk (validityOf mvalid) (VS.fromList (map fromIntegral offs)) (BS.concat (prefix : payloads ++ [suffix])))
    varText ::
      (Num o, Storable o) =>
      (Maybe I.Validity -> VS.Vector o -> ByteString -> Either String ColumnArray) ->
      Maybe (V.Vector Text -> ColumnArray) ->
      (V.Vector (Maybe Text) -> ColumnArray) ->
      ([Maybe Text] -> ColumnArray) ->
      Gen ColumnArray
    varText mk fromAll fromSome viaBuilder = do
      texts <- replicateM n genText
      mvalid <- genValidityBools
      method <- Gen.int (Range.linear 0 2)
      let rows = rowsOf mvalid texts
      case method of
        0 -> pure $ case (mvalid, fromAll) of
          (Nothing, Just g) -> g (V.fromList texts)
          _ -> fromSome (V.fromList rows)
        1 -> pure (viaBuilder rows)
        _ -> do
          prefix <- genTextJunk
          suffix <- genTextJunk
          let payloads = map TE.encodeUtf8 texts
              offs = scanl (+) (BS.length prefix) (map BS.length payloads)
          built (mk (validityOf mvalid) (VS.fromList (map fromIntegral offs)) (BS.concat (prefix : payloads ++ [suffix])))
    genJunk = Gen.frequency [(2, pure BS.empty), (1, Gen.bytes (Range.linear 1 5))]
    -- Outside the referenced range of UTF-8 data: valid text, so the
    -- offsets next to it stay on character boundaries.
    genTextJunk = Gen.frequency [(2, pure BS.empty), (1, TE.encodeUtf8 <$> Gen.text (Range.linear 1 3) Gen.unicode)]
    genExtraRows = Gen.frequency [(3, pure 0), (1, Gen.int (Range.linear 1 2))]

    -- Offsets may start past zero (a slice of a larger child), and the
    -- child may extend past the last offset.
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
      c <- sub (onlyChild f) m
      pure (map fst rows, map snd rows, c)


binaryViaBuilder, largeBinaryViaBuilder :: [Maybe ByteString] -> ColumnArray
binaryViaBuilder rows = runST $ do
  b <- newBinaryBuilder (length rows)
  mapM_ (appendBytesMaybe b) rows
  freezeBuilder b
largeBinaryViaBuilder rows = runST $ do
  b <- newLargeBinaryBuilder (length rows)
  mapM_ (appendBytesMaybe b) rows
  freezeBuilder b


utf8ViaBuilder, largeUtf8ViaBuilder :: [Maybe Text] -> ColumnArray
utf8ViaBuilder rows = runST $ do
  b <- newUtf8Builder (length rows)
  mapM_ (appendTextMaybe b) rows
  freezeBuilder b
largeUtf8ViaBuilder rows = runST $ do
  b <- newLargeUtf8Builder (length rows)
  mapM_ (appendTextMaybe b) rows
  freezeBuilder b


-- | Dictionary keys at the index type's wire width (null rows are 'Nothing').
keyColumn :: ArrowType -> V.Vector (Maybe Int) -> ColumnArray
keyColumn idx ks = case idx of
  AInt 8 True -> fromMaybes PInt8 (V.map (fmap fromIntegral) ks)
  AInt 16 True -> fromMaybes PInt16 (V.map (fmap fromIntegral) ks)
  AInt 32 True -> fromMaybes PInt32 (V.map (fmap fromIntegral) ks)
  AInt 64 True -> fromMaybes PInt64 (V.map (fmap fromIntegral) ks)
  AInt 8 False -> fromMaybes PUInt8 (V.map (fmap fromIntegral) ks)
  AInt 16 False -> fromMaybes PUInt16 (V.map (fmap fromIntegral) ks)
  AInt 32 False -> fromMaybes PUInt32 (V.map (fmap fromIntegral) ks)
  AInt 64 False -> fromMaybes PUInt64 (V.map (fmap fromIntegral) ks)
  other -> error ("Test.Arrow.Gen: unsupported dictionary index type " ++ show other)


-- | Values of every fixed-width element type, edge values included.
genPrimValue :: PrimType a -> Gen a
genPrimValue t = case t of
  PInt8 -> genI8
  PInt16 -> genI16
  PInt32 -> genI32
  PInt64 -> genI64
  PUInt8 -> genW8
  PUInt16 -> genW16
  PUInt32 -> genW32
  PUInt64 -> genW64
  PFloat16 -> Float16 <$> genW16
  PFloat -> genFloat
  PDouble -> genDouble
  PDate32 -> genI32
  PDate64 -> genI64
  PTime32 -> genI32
  PTime64 -> genI64
  PTimestamp -> genI64
  PDuration -> genI64
  PIntervalYearMonth -> genI32
  PIntervalDayTime -> IntervalDayTime <$> genI32 <*> genI32
  PIntervalMonthDayNano -> IntervalMonthDayNano <$> genI32 <*> genI32 <*> genI64
  PDecimal128 _ _ -> Decimal128 <$> genW64 <*> genW64
  PDecimal256 _ _ -> Decimal256 <$> genW64 <*> genW64 <*> genW64 <*> genW64


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


{- | Reorder keys so each entry's keys are non-decreasing; rows before
the first offset and after the last one stay where they are.
-}
sortKeysWithin :: [Int] -> ColumnArray -> ColumnArray
sortKeysWithin offs keys =
  case columnValues keys of
    Left e -> error e
    Right vs ->
      let entries = zip offs (drop 1 offs)
          inEntries = concatMap (\(s, e) -> sortBy (\i j -> compareKeyValues (vs V.! i) (vs V.! j)) [s .. e - 1]) entries
          (firstOff, lastOff) = case offs of
            [] -> (0, 0)
            (o : _) -> (o, last offs)
          perm = [0 .. firstOff - 1] ++ inEntries ++ [lastOff .. V.length vs - 1]
      in either error id (takeColumnArray (VS.fromList perm) keys)


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


-- | Direct children of a column, run ends and dictionary keys and values included.
childColumns :: ColumnArray -> [ColumnArray]
childColumns = \case
  ColStruct _ _ cs -> map snd (V.toList cs)
  ColList _ _ x -> [x]
  ColLargeList _ _ x -> [x]
  ColFixedSizeList _ _ _ x -> [x]
  ColMap _ _ k v -> [k, v]
  ColDenseUnion _ _ cs -> V.toList cs
  ColSparseUnion _ cs -> V.toList cs
  ColRunEndEncoded _ _ r v -> [r, v]
  ColListView _ _ _ x -> [x]
  ColLargeListView _ _ _ x -> [x]
  ColDictionary _ k v -> [k, v]
  _ -> []


-- | Whether any column (at any depth) is dictionary-encoded.
hasDictionaries :: ColumnArray -> Bool
hasDictionaries = \case
  ColDictionary {} -> True
  c -> any hasDictionaries (childColumns c)


{- | Logical rows of a column, read straight from its buffers (not
through the accessors under test); any out-of-range reference or
invalid UTF-8 inside a referenced range is a 'Left'.
-}
columnValues :: ColumnArray -> Either String (V.Vector Value)
columnValues col = case col of
  ColNull n -> Right (V.replicate n VNull)
  ColPrim t v xs -> withPrim t (Right (V.generate (VS.length xs) (\i -> if isValidAt v i then primValue t (xs VS.! i) else VNull)))
  ColBool v bits -> Right (V.generate (bitmapLength bits) (\i -> if isValidAt v i then VBool (bitAt bits i) else VNull))
  ColUtf8 v o d -> varRows utf8 v o d
  ColLargeUtf8 v o d -> varRows utf8 v o d
  ColBinary v o d -> varRows (Right . VBytes) v o d
  ColLargeBinary v o d -> varRows (Right . VBytes) v o d
  ColFixedSizeBinary w n v d ->
    V.generateM n $ \i ->
      if isValidAt v i then VBytes <$> cut "fixed-size binary row" d (i * w) w else Right VNull
  ColUtf8View v views bufs -> viewValues utf8 v views bufs
  ColBinaryView v views bufs -> viewValues (Right . VBytes) v views bufs
  ColStruct n v cs -> do
    kids <- V.mapM (traverse columnValues) cs
    V.generateM n $ \i ->
      if isValidAt v i
        then VStruct . V.toList <$> V.mapM (\(nm, vs) -> (,) nm <$> at "struct child row" vs i) kids
        else Right VNull
  ColList v o c -> listRows v (ints o) c
  ColLargeList v o c -> listRows v (ints o) c
  ColListView v o s c -> viewRows v (ints o) (ints s) c
  ColLargeListView v o s c -> viewRows v (ints o) (ints s) c
  ColFixedSizeList w n v c -> do
    vs <- columnValues c
    V.generateM n $ \i -> if isValidAt v i then VList <$> slice vs (i * w) w else Right VNull
  ColMap v o k x -> do
    ks <- columnValues k
    xs <- columnValues x
    let offs = ints o
    V.generateM (max 0 (VS.length o - 1)) $ \i -> do
      let s = offs V.! i
          e = offs V.! (i + 1)
      if isValidAt v i
        then do
          kk <- slice ks s (e - s)
          vv <- slice xs s (e - s)
          Right (VMap (zip kk vv))
        else Right VNull
  ColDenseUnion ts offs cs -> do
    kids <- V.mapM columnValues cs
    V.generateM (VS.length ts) $ \i -> do
      let t = ts VS.! i
      kid <- at "dense union child" kids (fromIntegral t)
      VUnion t <$> at "dense union offset" kid (fromIntegral (offs VS.! i))
  ColSparseUnion ts cs -> do
    kids <- V.mapM columnValues cs
    V.generateM (VS.length ts) $ \i -> do
      let t = ts VS.! i
      kid <- at "sparse union child" kids (fromIntegral t)
      VUnion t <$> at "sparse union row" kid i
  ColDictionary _ keys vals -> do
    ks <- columnValues keys
    vs <- columnValues vals
    V.mapM
      ( \case
          VNull -> Right VNull
          VInt k -> at "dictionary key" vs (fromIntegral k)
          other -> Left ("Test.Arrow.Gen: dictionary key " ++ show other)
      )
      ks
  ColRunEndEncoded off len re vals -> do
    ends <- columnValues re
    vs <- columnValues vals
    endInts <- traverse (\case VInt e -> Right (fromIntegral e :: Int); other -> Left ("Test.Arrow.Gen: run end " ++ show other)) ends
    V.generateM len $ \i -> case V.findIndex (> off + i) endInts of
      Nothing -> Left "Test.Arrow.Gen: row past the last run end"
      Just r -> at "run value" vs r
  where
    utf8 bs = either (const (Left "Test.Arrow.Gen: invalid UTF-8 in a referenced range")) (Right . VText) (TE.decodeUtf8' bs)
    ints :: (Storable o, Integral o) => VS.Vector o -> V.Vector Int
    ints = V.map fromIntegral . V.convert
    varRows :: (Storable o, Integral o) => (ByteString -> Either String Value) -> Maybe I.Validity -> VS.Vector o -> ByteString -> Either String (V.Vector Value)
    varRows decode v o d =
      let offs = ints o
      in V.generateM (max 0 (V.length offs - 1)) $ \i ->
           if isValidAt v i
             then cut "var-length row" d (offs V.! i) (offs V.! (i + 1) - offs V.! i) >>= decode
             else Right VNull
    viewValues decode v views bufs =
      V.generateM (BS.length views `quot` 16) $ \i ->
        if isValidAt v i
          then do
            let base = 16 * i
                len = le32 views base
            bytes <-
              if len <= 12
                then cut "inline view" views (base + 4) len
                else do
                  buf <- at "view buffer" bufs (le32 views (base + 8))
                  cut "view data" buf (le32 views (base + 12)) len
            decode bytes
          else Right VNull
    listRows v offs c = do
      vs <- columnValues c
      V.generateM (max 0 (V.length offs - 1)) $ \i ->
        if isValidAt v i
          then VList <$> slice vs (offs V.! i) (offs V.! (i + 1) - offs V.! i)
          else Right VNull
    viewRows v offs sizes c = do
      vs <- columnValues c
      V.generateM (V.length offs) $ \i ->
        if isValidAt v i
          then VList <$> slice vs (offs V.! i) (sizes V.! i)
          else Right VNull
    slice :: V.Vector Value -> Int -> Int -> Either String [Value]
    slice vs s l
      | s < 0 || l < 0 || s + l > V.length vs = Left ("Test.Arrow.Gen: slice out of range " ++ show s ++ "+" ++ show l ++ " of " ++ show (V.length vs))
      | otherwise = Right (V.toList (V.slice s l vs))
    cut :: String -> ByteString -> Int -> Int -> Either String ByteString
    cut what bs s l
      | s < 0 || l < 0 || s + l > BS.length bs = Left ("Test.Arrow.Gen: " ++ what ++ " out of range")
      | otherwise = Right (BS.take l (BS.drop s bs))
    at :: String -> V.Vector a -> Int -> Either String a
    at what vs i = maybe (Left ("Test.Arrow.Gen: " ++ what ++ " out of range")) Right (vs V.!? i)


-- | Little-endian signed 32-bit integer at a byte offset (0 past the end).
le32 :: ByteString -> Int -> Int
le32 bs o
  | o < 0 || o + 4 > BS.length bs = 0
  | otherwise =
      let w = foldr (\k acc -> acc `shiftL` 8 .|. fromIntegral (BS.index bs (o + k))) (0 :: Word32) [0 .. 3]
      in fromIntegral (fromIntegral w :: Int32)


-- | The logical value of one fixed-width element.
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
  PDate32 -> VInt (toInteger x)
  PDate64 -> VInt (toInteger x)
  PTime32 -> VInt (toInteger x)
  PTime64 -> VInt (toInteger x)
  PTimestamp -> VInt (toInteger x)
  PDuration -> VInt (toInteger x)
  PIntervalYearMonth -> VInt (toInteger x)
  PFloat16 -> let Float16 w = x in VF16 w
  PFloat -> VF32 (castFloatToWord32 x)
  PDouble -> VF64 (castDoubleToWord64 x)
  PIntervalDayTime -> let IntervalDayTime d m = x in VPair d m
  PIntervalMonthDayNano -> let IntervalMonthDayNano m d ns = x in VTriple m d ns
  PDecimal128 _ _ -> VDecimal (decimal128ToInteger x)
  PDecimal256 _ _ -> VDecimal (decimal256ToInteger x)


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
