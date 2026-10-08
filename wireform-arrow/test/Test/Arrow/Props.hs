{-# LANGUAGE OverloadedStrings #-}

{- | Generated round-trip and algebraic properties for wireform-arrow.

Columns are compared through the logical row model of
"Test.Arrow.Gen" ('columnValues'), so dictionary unification, offset
rebasing and NaN bit patterns do not cause spurious mismatches while
any change of meaning does.
-}
module Test.Arrow.Props (tests) where

import Arrow.Column (
  ColumnArray (..),
  columnLength,
  concatColumnArray,
  expandDictionary,
  isNullableColumn,
  sliceColumnArray,
  takeColumnArray,
  validateMapKeysSorted,
 )
import Arrow.File (readArrowFileColumns)
import Arrow.FlatBufferIPC (
  DictBatch (..),
  StreamFrame (..),
  buildDictionaryBatchMessage,
  buildRecordBatchBytes,
  buildRecordBatchMessage,
  buildSchemaMessage,
  encapsulateMessage,
  readArrowStreamFBInterleaved,
  readArrowStreamFBWithDicts,
  writeArrowStreamFBWithDicts,
 )
import Arrow.Stream (
  DictHandling (..),
  WriteOptions (..),
  bodyCompressionAvailable,
  decodeArrowFile,
  decodeArrowStream,
  defaultWriteOptions,
  encodeArrowFile,
  encodeArrowStream,
  openStreamReader,
  streamReaderProjected,
  streamReaderToList,
 )
import Arrow.Types
import Arrow.Write (writeArrowFile, writeArrowStream)
import Control.Monad (forM_, replicateM)
import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.List (elemIndex)
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Arrow.Gen


tests :: IO Bool
tests =
  checkParallel $
    Group
      "Arrow.Props"
      [ ("stream round-trip preserves every row", prop_streamRoundTrip)
      , ("file round-trip (encodeArrowFile / writeArrowFile / readArrowFileColumns)", prop_fileRoundTrip)
      , ("Arrow.Write.writeArrowStream round-trip", prop_writeStreamRoundTrip)
      , ("replacement-dictionary stream round-trip (eager and iterator)", prop_replaceDictsRoundTrip)
      , ("body compression round-trip", prop_compressionRoundTrip)
      , ("schema fingerprint survives round-trip", prop_fingerprintStable)
      , ("projection equals projecting the decoded table", prop_projection)
      , ("slice length and contents", prop_sliceContents)
      , ("slice of slice composes", prop_sliceOfSlice)
      , ("concat of adjacent slices is the original", prop_concatAdjacentSlices)
      , ("concat appends rows and lengths add", prop_concatAppends)
      , ("slice then round-trip equals round-trip then slice", prop_sliceCommutesWithRoundTrip)
      , ("take gathers rows by index", prop_take)
      , ("dictionary expansion keeps row values", prop_expandDictionary)
      , ("validateMapKeysSorted agrees with the model", prop_mapKeysSorted)
      , ("delta dictionary batches append", prop_deltaDictionary)
      , ("dictionary placeholder has the value type", prop_dictionaryPlaceholder)
      , ("reads a pyarrow-written Arrow file", prop_pyarrowFile)
      , ("fieldless structs and size-0 fixed-size lists keep their row count", prop_zeroWidthRowCount)
      , ("nested dictionaries round-trip, inner dictionaries written first", prop_nestedDictionaries)
      , ("nested dictionary batches resolve across deltas and replacements", prop_nestedDictionaryDeltaReplace)
      , ("emit-once rejects a combined dictionary its index type cannot address", prop_emitOnceIndexOverflow)
      , ("dictionary value columns that cannot be combined are rejected", prop_inconsistentDictionaryValues)
      , ("batches that do not fit the schema are rejected", prop_schemaMismatch)
      ]


-- ============================================================
-- Helpers
-- ============================================================

-- | Compare two tables by logical rows and top-level constructor.
sameTable :: MonadTest m => [V.Vector ColumnArray] -> [V.Vector ColumnArray] -> m ()
sameTable expected actual = do
  length actual === length expected
  forM_ (zip expected actual) $ \(e, a) -> do
    map tag (V.toList a) === map tag (V.toList e)
    evalEither (batchValues a) >>= \av -> evalEither (batchValues e) >>= \ev -> av === ev


tag :: ColumnArray -> String
tag = takeWhile (/= ' ') . show


values :: MonadTest m => ColumnArray -> m [Value]
values c = V.toList <$> evalEither (columnValues c)


-- | Constructor names of a column and all its children (dictionary values included).
allTags :: ColumnArray -> [String]
allTags c = tag c : concatMap allTags (children c)


-- | Whether any column in the tree (dictionary values included) satisfies the predicate.
anyColumn :: (ColumnArray -> Bool) -> ColumnArray -> Bool
anyColumn p c = p c || any (anyColumn p) (children c)


-- | Direct children of a column, dictionary values included.
children :: ColumnArray -> [ColumnArray]
children = \case
  ColStruct _ cs -> map snd (V.toList cs)
  ColStructMaybe _ cs -> map snd (V.toList cs)
  ColList _ x -> [x]
  ColListMaybe _ _ x -> [x]
  ColLargeList _ x -> [x]
  ColLargeListMaybe _ _ x -> [x]
  ColFixedSizeList _ _ x -> [x]
  ColFixedSizeListMaybe _ _ x -> [x]
  ColMap _ k v -> [k, v]
  ColMapMaybe _ _ k v -> [k, v]
  ColDenseUnion _ _ cs -> V.toList cs
  ColSparseUnion _ cs -> V.toList cs
  ColRunEndEncoded r v -> [r, v]
  ColListView _ _ x -> [x]
  ColListViewMaybe _ _ _ x -> [x]
  ColLargeListView _ _ x -> [x]
  ColLargeListViewMaybe _ _ _ x -> [x]
  ColDictionary _ _ v -> [v]
  ColDictionaryMaybe _ _ v -> [v]
  _ -> []


fieldlessStruct, sizeZeroList, nestedDictionary, emptyDictionary :: ColumnArray -> Bool
fieldlessStruct = \case
  ColStruct _ cs -> V.null cs
  ColStructMaybe _ cs -> V.null cs
  _ -> False
sizeZeroList = \case
  ColFixedSizeList 0 _ _ -> True
  ColFixedSizeListMaybe 0 _ _ -> True
  _ -> False
nestedDictionary = \case
  ColDictionary _ _ v -> hasDictionaries v
  ColDictionaryMaybe _ _ v -> hasDictionaries v
  _ -> False
emptyDictionary = \case
  ColDictionaryMaybe _ ix v -> columnLength v == 0 && not (V.null ix)
  _ -> False


-- | Require the generator to reach the shapes the properties are about.
coverShapes :: MonadTest m => [ColumnArray] -> m ()
coverShapes cols = do
  let tags = concatMap allTags cols
      has t = t `elem` tags
  forM_
    [ "ColDenseUnion"
    , "ColSparseUnion"
    , "ColRunEndEncoded"
    , "ColListView"
    , "ColLargeListViewMaybe"
    , "ColMapMaybe"
    , "ColFixedSizeList"
    , "ColDictionary"
    , "ColDictionaryMaybe"
    , "ColDecimal128Maybe"
    , "ColDecimal256Maybe"
    , "ColInterval*Maybe"
    , "ColUtf8View"
    , "ColBinaryViewMaybe"
    , "ColNull"
    , "ColStructMaybe"
    ]
    (\t -> cover 1 (fromString t) (if t == "ColInterval*Maybe" then any has ["ColIntervalYearMonthMaybe", "ColIntervalDayTimeMaybe", "ColIntervalMonthDayNanoMaybe"] else has t))
  cover 1 "struct with no fields" (any (anyColumn fieldlessStruct) cols)
  cover 1 "fixed-size list of size 0" (any (anyColumn sizeZeroList) cols)
  cover 1 "dictionary inside dictionary values" (any (anyColumn nestedDictionary) cols)
  cover 1 "all-null rows over an empty dictionary" (any (anyColumn emptyDictionary) cols)



-- | A single generated column (any type, nested to depth 3) with its field.
genColumn :: Gen (Field, ColumnArray)
genColumn = do
  f <- genField 3 "x"
  n <- Gen.int (Range.linear 0 10)
  c <- genColumnFor f n
  pure (f, c)


-- ============================================================
-- Round trips
-- ============================================================

prop_streamRoundTrip :: Property
prop_streamRoundTrip = withTests 500 . property $ do
  (sch, batches) <- forAll genTable
  coverShapes (concatMap V.toList batches)
  bytes <- evalEither (encodeArrowStream defaultWriteOptions sch batches)
  (_, got) <- evalEither (decodeArrowStream bytes)
  sameTable batches got


prop_fileRoundTrip :: Property
prop_fileRoundTrip = withTests 200 . property $ do
  (sch, batches) <- forAll genTable
  file <- evalEither (encodeArrowFile defaultWriteOptions sch batches)
  (_, got1) <- evalEither (decodeArrowFile file)
  sameTable batches got1
  written <- evalEither (writeArrowFile sch (V.fromList batches))
  (_, got2) <- evalEither (readArrowFileColumns written)
  sameTable batches (V.toList got2)
  (_, got3) <- evalEither (readArrowFileColumns file)
  sameTable batches (V.toList got3)


prop_writeStreamRoundTrip :: Property
prop_writeStreamRoundTrip = withTests 100 . property $ do
  (sch, batches) <- forAll genTable
  bytes <- evalEither (writeArrowStream sch (V.fromList batches))
  (_, got) <- evalEither (decodeArrowStream bytes)
  sameTable batches got


prop_replaceDictsRoundTrip :: Property
prop_replaceDictsRoundTrip = withTests 200 . property $ do
  (sch, batches) <- forAll genTable
  bytes <- evalEither (encodeArrowStream defaultWriteOptions {writeDictHandling = DictReplaceOnChange} sch batches)
  (_, got) <- evalEither (decodeArrowStream bytes)
  sameTable batches got
  rd <- evalEither (openStreamReader bytes)
  got' <- evalEither (streamReaderToList rd)
  sameTable batches got'

{- | Compression round-trips. A codec that is not compiled in (cabal
flags @zstd@ / @lz4@) is documented to fall back to uncompressed
batches; when the codec is available every batch must say so.
-}
prop_compressionRoundTrip :: Property
prop_compressionRoundTrip = withTests 100 . property $ do
  (sch, batches) <- forAll genTable
  codec <- forAll (Gen.element [LZ4Frame, BodyZstd])
  bytes <- evalEither (encodeArrowStream defaultWriteOptions {writeBodyCompression = Just codec} sch batches)
  let expected = if bodyCompressionAvailable codec then Just codec else Nothing
  (_, got) <- evalEither (decodeArrowStream bytes)
  sameTable batches got
  (_, dicts, frames) <- evalEither (readArrowStreamFBWithDicts bytes)
  map (rbBodyCompression . fst) frames === map (const expected) frames
  map (rbBodyCompression . dbData) dicts === map (const expected) dicts


prop_fingerprintStable :: Property
prop_fingerprintStable = withTests 200 . property $ do
  (sch, batches) <- forAll genTable
  (sch1, _) <- evalEither (encodeArrowStream defaultWriteOptions sch batches >>= decodeArrowStream)
  (sch2, _) <- evalEither (encodeArrowFile defaultWriteOptions sch batches >>= decodeArrowFile)
  schemaFingerprint sch1 === schemaFingerprint sch
  schemaFingerprint sch2 === schemaFingerprint sch
  assert (schemaEquivalent sch1 sch)


prop_projection :: Property
prop_projection = withTests 200 . property $ do
  (sch, batches) <- forAll genTable
  let names = V.toList (V.map fieldName (arrowFields sch))
  picked <- forAll (Gen.subsequence names >>= Gen.shuffle)
  bytes <- evalEither (encodeArrowStream defaultWriteOptions sch batches)
  rd <- evalEither (openStreamReader bytes)
  (psch, got) <- evalEither (streamReaderProjected picked rd)
  (_, full) <- evalEither (decodeArrowStream bytes)
  let idx nm = V.findIndex ((== nm) . fieldName) (arrowFields sch)
      project cols = V.fromList (map (\nm -> maybe (error "missing") (cols V.!) (idx nm)) picked)
  map fieldName (V.toList (arrowFields psch)) === picked
  sameTable (map project full) got


-- ============================================================
-- Slicing, concatenation, gathering
-- ============================================================

prop_sliceContents :: Property
prop_sliceContents = withTests 500 . property $ do
  (_, c) <- forAll genColumn
  let n = columnLength c
  s <- forAll (Gen.int (Range.linear (-2) (n + 2)))
  l <- forAll (Gen.int (Range.linear (-2) (n + 2)))
  let s' = min n (max 0 s)
      l' = max 0 (min l (n - s'))
      sl = sliceColumnArray s l c
  columnLength sl === l'
  vs <- values c
  svs <- values sl
  svs === take l' (drop s' vs)
  tag sl === tag c


prop_sliceOfSlice :: Property
prop_sliceOfSlice = withTests 300 . property $ do
  (_, c) <- forAll genColumn
  let n = columnLength c
  s1 <- forAll (Gen.int (Range.linear 0 n))
  l1 <- forAll (Gen.int (Range.linear 0 (n - s1)))
  s2 <- forAll (Gen.int (Range.linear 0 l1))
  l2 <- forAll (Gen.int (Range.linear 0 (l1 - s2)))
  a <- values (sliceColumnArray s2 l2 (sliceColumnArray s1 l1 c))
  b <- values (sliceColumnArray (s1 + s2) l2 c)
  a === b


prop_concatAdjacentSlices :: Property
prop_concatAdjacentSlices = withTests 500 . property $ do
  (_, c) <- forAll genColumn
  let n = columnLength c
  k <- forAll (Gen.int (Range.linear 0 n))
  joined <- evalEither (concatColumnArray (sliceColumnArray 0 k c) (sliceColumnArray k (n - k) c))
  columnLength joined === n
  a <- values joined
  b <- values c
  a === b


prop_concatAppends :: Property
prop_concatAppends = withTests 300 . property $ do
  f <- forAll (genField 3 "x")
  n1 <- forAll (Gen.int (Range.linear 0 8))
  n2 <- forAll (Gen.int (Range.linear 0 8))
  c1 <- forAll (genColumnFor f n1)
  c2 <- forAll (genColumnFor f n2)
  joined <- evalEither (concatColumnArray c1 c2)
  columnLength joined === n1 + n2
  a <- values joined
  v1 <- values c1
  v2 <- values c2
  a === v1 ++ v2


prop_sliceCommutesWithRoundTrip :: Property
prop_sliceCommutesWithRoundTrip = withTests 200 . property $ do
  sch <- forAll genSchema
  batch <- forAll (genBatchFor sch)
  let n = if V.null batch then 0 else columnLength (V.head batch)
  s <- forAll (Gen.int (Range.linear 0 n))
  l <- forAll (Gen.int (Range.linear 0 (n - s)))
  (_, sliced) <- evalEither (encodeArrowStream defaultWriteOptions sch [V.map (sliceColumnArray s l) batch] >>= decodeArrowStream)
  (_, whole) <- evalEither (encodeArrowStream defaultWriteOptions sch [batch] >>= decodeArrowStream)
  sameTable (map (V.map (sliceColumnArray s l)) whole) sliced


prop_take :: Property
prop_take = withTests 300 . property $ do
  (_, c) <- forAll genColumn
  let n = columnLength c
  ix <- forAll (if n == 0 then pure [] else Gen.list (Range.linear 0 12) (Gen.int (Range.linear 0 (n - 1))))
  taken <- evalEither (takeColumnArray (VP.fromList ix) c)
  vs <- values c
  tvs <- values taken
  tvs === map (vs !!) ix
  if n > 0 then pure () else assert (not (isRight (takeColumnArray (VP.singleton 0) c)))


{- | Expansion keeps every row's value; a nullable dictionary column
whose rows are all null over an empty dictionary expands to an
all-null column of the value type.
-}
prop_expandDictionary :: Property
prop_expandDictionary = withTests 300 . property $ do
  f <- forAll (Gen.choice [genField 0 "x", genDictionaryField 0 "x"])
  n <- forAll (Gen.int (Range.linear 0 10))
  c <- forAll (genColumnFor f n)
  cover 1 "all-null rows over an empty dictionary" (emptyDictionary c)
  e <- evalEither (expandDictionary c)
  columnLength e === n
  a <- values e
  b <- values c
  a === b
  assert (not (hasDictionaries e))
  case c of
    ColDictionaryMaybe {} -> assert (isNullableColumn e)
    _ -> success


-- ============================================================
-- Map key order
-- ============================================================

{- | 'validateMapKeysSorted' accepts a map exactly when every non-null
entry's keys are non-null and non-decreasing under the documented
order, and rejects key types without an order.
-}
prop_mapKeysSorted :: Property
prop_mapKeysSorted = withTests 1000 . property $ do
  kind <- forAll (Gen.element ["orderable", "dictionary", "nullable", "unorderable" :: Text])
  lens <- forAll (Gen.list (Range.linear 0 5) (Gen.int (Range.linear 0 4)))
  let offs = scanl (+) 0 lens
      total = last offs
  keyField <- forAll $ case kind of
    "unorderable" -> do
      ty <- Gen.element [AInterval YearMonth, AInterval DayTime, AInterval MonthDayNano, ANull]
      pure (Field "key" False ty V.empty Nothing V.empty)
    "dictionary" -> do
      f <- genOrderableKeyField "key"
      pure f {fieldDictionary = Just (DictionaryEncoding 0 (AInt 16 True) False)}
    "nullable" -> (\f -> f {fieldNullable = True}) <$> genOrderableKeyField "key"
    _ ->
      Gen.choice
        [ genOrderableKeyField "key"
        , pure (Field "key" False (AFloatingPoint Half) V.empty Nothing V.empty)
        , pure (Field "key" False (AFloatingPoint Single) V.empty Nothing V.empty)
        , pure (Field "key" False (ADecimal256 40 0) V.empty Nothing V.empty)
        , pure (Field "key" False ABinaryView V.empty Nothing V.empty)
        , pure (Field "key" False (ADuration Second) V.empty Nothing V.empty)
        ]
  keys <- forAll (genColumnFor keyField total)
  mapValid <- forAll (Gen.list (Range.singleton (length lens)) (Gen.frequency [(1, pure False), (4, pure True)]))
  nullableMap <- forAll Gen.bool
  let o = VP.fromList (map fromIntegral offs)
      vals = ColNull total
      col =
        if nullableMap
          then ColMapMaybe (V.fromList mapValid) o keys vals
          else ColMap o keys vals
      entryOn i = not nullableMap || (mapValid !! i)
  kvs <- values keys
  let entries = zip [0 ..] (zip offs (drop 1 offs))
      entryKeys (s, e) = take (e - s) (drop s kvs)
      ok = all (\(i, se) -> not (entryOn i) || entrySorted (entryKeys se)) entries
      entrySorted ks = notElem VNull ks && and (zipWith (\x y -> compareKeyValues x y /= GT) ks (drop 1 ks))
      expected = kind /= "unorderable" && ok
  annotateShow (validateMapKeysSorted col)
  isRight (validateMapKeysSorted col) === expected


-- ============================================================
-- Dictionaries at the wire level
-- ============================================================

{- | A dictionary followed by a delta dictionary batch: the record
batch's indices address the concatenation of both value columns.
-}
prop_deltaDictionary :: Property
prop_deltaDictionary = withTests 300 . property $ do
  ty <- forAll (Gen.filter (/= ANull) genFieldType)
  valuesNullable <- forAll Gen.bool
  idxTy <- forAll (AInt <$> Gen.element [8, 16, 32, 64] <*> Gen.bool)
  let field = Field "d" False ty V.empty (Just (DictionaryEncoding 7 idxTy False)) V.empty
      valuesField = field {fieldDictionary = Nothing, fieldNullable = valuesNullable}
      valuesSchema = defaultSchema (V.singleton valuesField)
      sch = defaultSchema (V.singleton field)
  k1 <- forAll (Gen.int (Range.linear 1 4))
  k2 <- forAll (Gen.int (Range.linear 1 4))
  part1 <- forAll (genColumnFor valuesField k1)
  part2 <- forAll (genColumnFor valuesField k2)
  n <- forAll (Gen.int (Range.linear 0 10))
  ix <- forAll (replicateM n (Gen.int32 (Range.linear 0 (fromIntegral (k1 + k2) - 1))))
  let dict isDelta vals =
        let (drb, dbody) = buildRecordBatchBytes valuesSchema (V.singleton vals)
            db = DictBatch {dbId = 7, dbIsDelta = isDelta, dbData = drb, dbBody = dbody}
        in encapsulateMessage (buildDictionaryBatchMessage db) dbody
      (rb, body) = buildRecordBatchBytes sch (V.singleton (ColDictionary 7 (VP.fromList ix) part1))
      bytes =
        BS.concat
          [ encapsulateMessage (buildSchemaMessage sch) BS.empty
          , dict False part1
          , dict True part2
          , encapsulateMessage (buildRecordBatchMessage rb (fromIntegral (BS.length body))) body
          , BS.pack [0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0]
          ]
  (_, got) <- evalEither (decodeArrowStream bytes)
  v1 <- values part1
  v2 <- values part2
  let allVals = v1 ++ v2
  case got of
    [cols] | [c] <- V.toList cols -> do
      gv <- values c
      gv === map (\i -> allVals !! fromIntegral i) ix
    _ -> failure


{- | A batch with no dictionary rows and no dictionary batch keeps a
typed empty placeholder whose constructor matches the value type.
-}
prop_dictionaryPlaceholder :: Property
prop_dictionaryPlaceholder = withTests 200 . property $ do
  ty <- forAll (Gen.filter (/= ANull) genFieldType)
  nullable <- forAll Gen.bool
  let field = Field "d" nullable ty V.empty (Just (DictionaryEncoding 3 (AInt 32 True) False)) V.empty
      sch = defaultSchema (V.singleton field)
  empty <- forAll (genColumnFor field 0)
  sample <- forAll (genColumnFor field {fieldDictionary = Nothing, fieldNullable = False} 0)
  let bytes = writeArrowStreamFBWithDicts sch [] [buildRecordBatchBytes sch (V.singleton empty)]
  (_, got) <- evalEither (decodeArrowStream bytes)
  case got of
    [cols] | [c] <- V.toList cols -> case c of
      ColDictionary _ ix vals -> do
        assert (not nullable)
        VP.length ix === 0
        tag vals === tag sample
      ColDictionaryMaybe _ ix vals -> do
        assert nullable
        V.length ix === 0
        tag vals === tag sample
      other -> annotateShow other >> failure
    _ -> failure


{- | @test/golden/pa_file.arrow@ was written by pyarrow 25.0.1 with
@pa.ipc.new_file@: two batches over
@i: int32, s: string, d: dictionary<string, int32>, n: decimal128(10, 2), l: list<int64>@
(all fields nullable, as pyarrow defaults), batch 1 =
@i=[1,2,3], s=["a",null,"h\\u00e9llo"], d=["x","y",null], n=[1.50,null,-2.25], l=[[1,2],null,[]]@,
batch 2 = @i=[-7], s=[""], d=["y"], n=[null], l=[[9]]@, dictionary @["x","y"]@.
-}
prop_pyarrowFile :: Property
prop_pyarrowFile = withTests 1 . property $ do
  bytes <- evalIO (BS.readFile "test/golden/pa_file.arrow")
  (sch, batches) <- evalEither (readArrowFileColumns bytes)
  map fieldName (V.toList (arrowFields sch)) === ["i", "s", "d", "n", "l"]
  (_, viaStream) <- evalEither (decodeArrowFile bytes)
  map (map tag . V.toList) viaStream
    === replicate 2 ["ColInt32Maybe", "ColUtf8Maybe", "ColDictionaryMaybe", "ColDecimal128Maybe", "ColListMaybe"]
  got <- evalEither (traverse batchValues (V.toList batches))
  got
    === [ [ [VInt 1, VInt 2, VInt 3]
          , [VText "a", VNull, VText "h\233llo"]
          , [VText "x", VText "y", VNull]
          , [VDecimal 150, VNull, VDecimal (-225)]
          , [VList [VInt 1, VInt 2], VNull, VList []]
          ]
        , [ [VInt (-7)]
          , [VText ""]
          , [VText "y"]
          , [VNull]
          , [VList [VInt 9]]
          ]
        ]


-- ============================================================
-- Fieldless structs, size-0 fixed-size lists, nested dictionaries,
-- writer errors
-- ============================================================

{- | A struct with no fields and a fixed-size list of size 0 carry their
row count explicitly: it survives stream and file round trips at the
top level and nested in a list, nullable or not, and slicing and
concatenation keep it.
-}
prop_zeroWidthRowCount :: Property
prop_zeroWidthRowCount = withTests 200 . property $ do
  n <- forAll (Gen.int (Range.linear 0 20))
  structNullable <- forAll Gen.bool
  listNullable <- forAll Gen.bool
  valid <- forAll (V.fromList <$> Gen.list (Range.singleton n) Gen.bool)
  let item = Field "item" False (AInt 32 True) V.empty Nothing V.empty
      fieldless nm nullable = Field nm nullable AStruct V.empty Nothing V.empty
      sch =
        defaultSchema
          ( V.fromList
              [ fieldless "s" structNullable
              , Field "f" listNullable (AFixedSizeList 0) (V.singleton item) Nothing V.empty
              , Field "l" False AList (V.singleton (fieldless "item" False)) Nothing V.empty
              ]
          )
      structCol = if structNullable then ColStructMaybe valid V.empty else ColStruct n V.empty
      listCol =
        if listNullable
          then ColFixedSizeListMaybe 0 valid (ColInt32 VP.empty)
          else ColFixedSizeList 0 n (ColInt32 VP.empty)
      -- Every list row holds two fieldless structs.
      nestedCol = ColList (VP.generate (n + 1) (fromIntegral . (* 2))) (ColStruct (2 * n) V.empty)
      batch = V.fromList [structCol, listCol, nestedCol]
  stream <- evalEither (encodeArrowStream defaultWriteOptions sch [batch])
  (_, got) <- evalEither (decodeArrowStream stream)
  sameTable [batch] got
  map (map columnLength . V.toList) got === [[n, n, n]]
  file <- evalEither (encodeArrowFile defaultWriteOptions sch [batch])
  (_, gotFile) <- evalEither (decodeArrowFile file)
  sameTable [batch] gotFile
  k <- forAll (Gen.int (Range.linear 0 n))
  forM_ [structCol, listCol] $ \c -> do
    joined <- evalEither (concatColumnArray (sliceColumnArray 0 k c) (sliceColumnArray k (n - k) c))
    columnLength joined === n
    columnLength (sliceColumnArray k n c) === n - k


{- | Dictionaries nested in dictionary values,
@o: dictionary\<struct\<e: dictionary\<utf8\>\>\>@ and
@p: dictionary\<list\<dictionary\<utf8\>\>\>@, round-trip through both
dictionary modes (eager and iterator reads) and the file writer, and
every inner dictionary batch is written before the outer dictionary
batch that needs it (in replacement mode, an outer dictionary is
re-sent with every inner one).
-}
prop_nestedDictionaries :: Property
prop_nestedDictionaries = withTests 200 . property $ do
  innerIdx <- forAll (AInt <$> Gen.element [8, 16, 32, 64] <*> Gen.bool)
  outerIdx <- forAll (AInt <$> Gen.element [8, 16, 32, 64] <*> Gen.bool)
  oNull <- forAll Gen.bool
  eNull <- forAll Gen.bool
  pNull <- forAll Gen.bool
  qNull <- forAll Gen.bool
  let dict did idx = Just (DictionaryEncoding did idx False)
      e = Field "e" eNull AUtf8 V.empty (dict 2 innerIdx) V.empty
      o = Field "o" oNull AStruct (V.singleton e) (dict 1 outerIdx) V.empty
      q = Field "item" qNull AUtf8 V.empty (dict 4 innerIdx) V.empty
      p = Field "p" pNull AList (V.singleton q) (dict 3 outerIdx) V.empty
      sch = defaultSchema (V.fromList [o, p])
  nBatches <- forAll (Gen.int (Range.linear 1 4))
  batches <- forAll (replicateM nBatches (genBatchFor sch))
  mode <- forAll (Gen.element [DictEmitOnce, DictReplaceOnChange])
  let opts = defaultWriteOptions {writeDictHandling = mode}
  bytes <- evalEither (encodeArrowStream opts sch batches)
  (_, got) <- evalEither (decodeArrowStream bytes)
  sameTable batches got
  rd <- evalEither (openStreamReader bytes)
  got' <- evalEither (streamReaderToList rd)
  sameTable batches got'
  file <- evalEither (encodeArrowFile opts sch batches)
  (_, gotFile) <- evalEither (decodeArrowFile file)
  sameTable batches gotFile
  (_, frames) <- evalEither (readArrowStreamFBInterleaved bytes)
  let groups = dictGroups frames
  annotateShow groups
  forM_ groups $ \ids ->
    forM_ [(2, 1), (4, 3)] $ \(inner, outer) -> case elemIndex inner ids of
      Nothing -> success
      Just i -> assert (maybe False (> i) (elemIndex outer ids))
  where
    -- Dictionary ids sent before each record batch, in stream order.
    dictGroups :: [StreamFrame] -> [[Int64]]
    dictGroups = go []
      where
        go acc [] = if null acc then [] else [reverse acc]
        go acc (SFDict db : rest) = go (dbId db : acc) rest
        go acc (SFBatch {} : rest) = reverse acc : go [] rest


{- | Wire-level nested dictionaries: inner dictionary 2 (utf8) inside the
struct values of outer dictionary 1. The stream sends inner, outer, a
batch; an inner delta, an outer delta, a batch; an inner replacement,
an outer replacement, a batch; and a second inner replacement alone,
then a batch. Each outer dictionary batch resolves against the inner
dictionary in force when it arrives, so the last batch still reads the
values of the third.
-}
prop_nestedDictionaryDeltaReplace :: Property
prop_nestedDictionaryDeltaReplace = withTests 200 . property $ do
  let word = Gen.text (Range.linear 0 4) Gen.alphaNum
      indexInto k = Gen.int (Range.linear 0 (k - 1))
      rowsOver k = Gen.list (Range.linear 0 6) (indexInto k)
  a <- forAll (Gen.list (Range.linear 1 4) word)
  aDelta <- forAll (Gen.list (Range.linear 1 3) word)
  b <- forAll (Gen.list (Range.linear 1 4) word)
  c <- forAll (Gen.list (Range.linear 1 4) word)
  o1 <- forAll (Gen.list (Range.linear 1 4) (indexInto (length a)))
  o1Delta <- forAll (Gen.list (Range.linear 1 3) (indexInto (length a + length aDelta)))
  o2 <- forAll (Gen.list (Range.linear 1 4) (indexInto (length b)))
  r1 <- forAll (rowsOver (length o1))
  r2 <- forAll (rowsOver (length o1 + length o1Delta))
  r3 <- forAll (rowsOver (length o2))
  r4 <- forAll (rowsOver (length o2))
  let e = Field "e" False AUtf8 V.empty (Just (DictionaryEncoding 2 (AInt 8 True) False)) V.empty
      o = Field "o" False AStruct (V.singleton e) (Just (DictionaryEncoding 1 (AInt 16 True) False)) V.empty
      sch = defaultSchema (V.singleton o)
      ints = VP.fromList . map fromIntegral
      innerPlaceholder = ColDictionary 2 VP.empty (ColUtf8 V.empty)
      dictMsg schema did isDelta col =
        let (drb, dbody) = buildRecordBatchBytes schema (V.singleton col)
            db = DictBatch {dbId = did, dbIsDelta = isDelta, dbData = drb, dbBody = dbody}
        in encapsulateMessage (buildDictionaryBatchMessage db) dbody
      inner isDelta ws = dictMsg (defaultSchema (V.singleton e {fieldDictionary = Nothing})) 2 isDelta (ColUtf8 (V.fromList ws))
      outer isDelta ix =
        dictMsg
          (defaultSchema (V.singleton o {fieldDictionary = Nothing}))
          1
          isDelta
          (ColStruct (length ix) (V.singleton ("e", ColDictionary 2 (ints ix) (ColUtf8 V.empty))))
      batch ix =
        let (rb, body) = buildRecordBatchBytes sch (V.singleton (ColDictionary 1 (ints ix) (ColStruct 0 (V.singleton ("e", innerPlaceholder)))))
        in encapsulateMessage (buildRecordBatchMessage rb (fromIntegral (BS.length body))) body
      bytes =
        BS.concat
          [ encapsulateMessage (buildSchemaMessage sch) BS.empty
          , inner False a
          , outer False o1
          , batch r1
          , inner True aDelta
          , outer True o1Delta
          , batch r2
          , inner False b
          , outer False o2
          , batch r3
          , inner False c
          , batch r4
          , BS.pack [0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0]
          ]
      structRows rows outerVals = map (\i -> VStruct [("e", VText (outerVals !! i))]) rows
      afterDelta = map ((a ++ aDelta) !!) (o1 ++ o1Delta)
      replaced = map (b !!) o2
      expected =
        [ [structRows r1 (map (a !!) o1)]
        , [structRows r2 afterDelta]
        , [structRows r3 replaced]
        , [structRows r4 replaced]
        ]
  (_, got) <- evalEither (decodeArrowStream bytes)
  evalEither (traverse batchValues got) >>= (=== expected)
  rd <- evalEither (openStreamReader bytes)
  got' <- evalEither (streamReaderToList rd)
  evalEither (traverse batchValues got') >>= (=== expected)


{- | With emit-once dictionaries an int8-indexed field whose batches
combine to more than 128 dictionary values cannot be written (the
shifted indices would wrap); replacement dictionaries write each
batch's own dictionary and round-trip.
-}
prop_emitOnceIndexOverflow :: Property
prop_emitOnceIndexOverflow = withTests 100 . property $ do
  k1 <- forAll (Gen.int (Range.constant 1 120))
  k2 <- forAll (Gen.int (Range.constant 1 120))
  let field = Field "d" False AUtf8 V.empty (Just (DictionaryEncoding 0 (AInt 8 True) False)) V.empty
      sch = defaultSchema (V.singleton field)
      dictCol prefix k = ColDictionary 0 (VP.generate k fromIntegral) (ColUtf8 (V.generate k (\i -> prefix <> T.pack (show i))))
      batches = [V.singleton (dictCol "a" k1), V.singleton (dictCol "b" k2)]
      fits = k1 + k2 <= 128
  cover 20 "combined dictionary overflows int8" (not fits)
  cover 20 "combined dictionary fits int8" fits
  isRight (encodeArrowStream defaultWriteOptions sch batches) === fits
  isRight (encodeArrowFile defaultWriteOptions sch batches) === fits
  isRight (writeArrowStream sch (V.fromList batches)) === fits
  replaced <- evalEither (encodeArrowStream defaultWriteOptions {writeDictHandling = DictReplaceOnChange} sch batches)
  (_, got) <- evalEither (decodeArrowStream replaced)
  sameTable batches got


{- | Value columns of one dictionary id that cannot be concatenated
(struct values whose child names differ between batches) are rejected
by the emit-once and file writers, and a values column of the wrong
type is rejected by every writer. Replacement dictionaries never
concatenate, so the first pair still writes in that mode.
-}
prop_inconsistentDictionaryValues :: Property
prop_inconsistentDictionaryValues = withTests 100 . property $ do
  n1 <- forAll (Gen.int (Range.linear 1 5))
  n2 <- forAll (Gen.int (Range.linear 1 5))
  let child = Field "x" False (AInt 32 True) V.empty Nothing V.empty
      field = Field "d" False AStruct (V.singleton child) (Just (DictionaryEncoding 0 (AInt 32 True) False)) V.empty
      sch = defaultSchema (V.singleton field)
      ix k = VP.generate k fromIntegral
      structBatch nm k = V.singleton (ColDictionary 0 (ix k) (ColStruct k (V.singleton (nm, ColInt32 (ix k)))))
      renamed = [structBatch "x" n1, structBatch "y" n2]
      wrongType = [structBatch "x" n1, V.singleton (ColDictionary 0 (ix n2) (ColUtf8 (V.replicate n2 "v")))]
      replace = defaultWriteOptions {writeDictHandling = DictReplaceOnChange}
  assert (isLeft (encodeArrowStream defaultWriteOptions sch renamed))
  assert (isLeft (encodeArrowFile defaultWriteOptions sch renamed))
  assert (isRight (encodeArrowStream replace sch renamed))
  assert (isLeft (encodeArrowStream defaultWriteOptions sch wrongType))
  assert (isLeft (encodeArrowStream replace sch wrongType))
  assert (isLeft (encodeArrowFile defaultWriteOptions sch wrongType))


{- | A batch that does not fit its schema is rejected by every writer:
a missing or extra column, a column whose type its field cannot hold,
columns of unequal length, and a struct or fixed-size list whose row
count disagrees with its child. The unbroken batch writes.
-}
prop_schemaMismatch :: Property
prop_schemaMismatch = withTests 300 . property $ do
  sch <- forAll genSchema
  batch <- forAll (Gen.filter (\b -> columnLength (V.head b) > 0) (genBatchFor sch))
  let n = columnLength (V.head batch)
      f0 = V.head (arrowFields sch)
      c0 = V.head batch
      withFields fs = sch {arrowFields = V.fromList fs}
  breakage <- forAll (Gen.element ["missing", "extra", "type", "length", "struct", "fixed-size list" :: Text])
  doubled <- evalEither (concatColumnArray c0 c0)
  let (sch', bad) = case breakage of
        "missing" -> (sch, V.tail batch)
        "extra" -> (sch, V.snoc batch (ColNull n))
        "type" ->
          let wrong = if fieldType f0 == ABool && isNothing (fieldDictionary f0) then ColNull n else ColBool (V.replicate n False)
          in (sch, V.cons wrong (V.tail batch))
        "length" -> (withFields (f0 : f0 {fieldName = "dup"} : V.toList (V.tail (arrowFields sch))), V.cons c0 (V.cons doubled (V.tail batch)))
        "struct" ->
          ( withFields [Field "s" False AStruct (V.singleton f0) Nothing V.empty]
          , V.singleton (ColStruct (n + 1) (V.singleton (fieldName f0, c0)))
          )
        _ ->
          ( withFields [Field "l" False (AFixedSizeList 1) (V.singleton f0) Nothing V.empty]
          , V.singleton (ColFixedSizeList 1 (n + 1) c0)
          )
      replace = defaultWriteOptions {writeDictHandling = DictReplaceOnChange}
  assert (isRight (encodeArrowStream defaultWriteOptions sch [batch]))
  annotateShow (encodeArrowStream defaultWriteOptions sch' [bad])
  assert (isLeft (encodeArrowStream defaultWriteOptions sch' [bad]))
  assert (isLeft (encodeArrowStream replace sch' [bad]))
  assert (isLeft (encodeArrowFile defaultWriteOptions sch' [bad]))
  assert (isLeft (writeArrowStream sch' (V.singleton bad)))
  assert (isLeft (writeArrowFile sch' (V.singleton bad)))
