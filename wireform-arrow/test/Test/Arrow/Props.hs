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
  buildDictionaryBatchMessage,
  buildRecordBatchBytes,
  buildRecordBatchMessage,
  buildSchemaMessage,
  encapsulateMessage,
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
import Data.Either (isRight)
import Data.Text (Text)
import Data.String (fromString)
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
allTags c = tag c : concatMap allTags (kids c)
  where
    kids = \case
      ColStruct cs -> map snd (V.toList cs)
      ColStructMaybe _ cs -> map snd (V.toList cs)
      ColList _ x -> [x]
      ColListMaybe _ _ x -> [x]
      ColLargeList _ x -> [x]
      ColLargeListMaybe _ _ x -> [x]
      ColFixedSizeList _ x -> [x]
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
  (_, got) <- evalEither (decodeArrowStream (encodeArrowStream defaultWriteOptions sch batches))
  sameTable batches got


prop_fileRoundTrip :: Property
prop_fileRoundTrip = withTests 200 . property $ do
  (sch, batches) <- forAll genTable
  (_, got1) <- evalEither (decodeArrowFile (encodeArrowFile defaultWriteOptions sch batches))
  sameTable batches got1
  (_, got2) <- evalEither (readArrowFileColumns (writeArrowFile sch (V.fromList batches)))
  sameTable batches (V.toList got2)
  (_, got3) <- evalEither (readArrowFileColumns (encodeArrowFile defaultWriteOptions sch batches))
  sameTable batches (V.toList got3)


prop_writeStreamRoundTrip :: Property
prop_writeStreamRoundTrip = withTests 100 . property $ do
  (sch, batches) <- forAll genTable
  (_, got) <- evalEither (decodeArrowStream (writeArrowStream sch (V.fromList batches)))
  sameTable batches got


prop_replaceDictsRoundTrip :: Property
prop_replaceDictsRoundTrip = withTests 200 . property $ do
  (sch, batches) <- forAll genTable
  let bytes = encodeArrowStream defaultWriteOptions {writeDictHandling = DictReplaceOnChange} sch batches
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
  let bytes = encodeArrowStream defaultWriteOptions {writeBodyCompression = Just codec} sch batches
      expected = if bodyCompressionAvailable codec then Just codec else Nothing
  (_, got) <- evalEither (decodeArrowStream bytes)
  sameTable batches got
  (_, dicts, frames) <- evalEither (readArrowStreamFBWithDicts bytes)
  map (rbBodyCompression . fst) frames === map (const expected) frames
  map (rbBodyCompression . dbData) dicts === map (const expected) dicts


prop_fingerprintStable :: Property
prop_fingerprintStable = withTests 200 . property $ do
  (sch, batches) <- forAll genTable
  (sch1, _) <- evalEither (decodeArrowStream (encodeArrowStream defaultWriteOptions sch batches))
  (sch2, _) <- evalEither (decodeArrowFile (encodeArrowFile defaultWriteOptions sch batches))
  schemaFingerprint sch1 === schemaFingerprint sch
  schemaFingerprint sch2 === schemaFingerprint sch
  assert (schemaEquivalent sch1 sch)


prop_projection :: Property
prop_projection = withTests 200 . property $ do
  (sch, batches) <- forAll genTable
  let names = V.toList (V.map fieldName (arrowFields sch))
  picked <- forAll (Gen.subsequence names >>= Gen.shuffle)
  let bytes = encodeArrowStream defaultWriteOptions sch batches
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
  (_, sliced) <- evalEither (decodeArrowStream (encodeArrowStream defaultWriteOptions sch [V.map (sliceColumnArray s l) batch]))
  (_, whole) <- evalEither (decodeArrowStream (encodeArrowStream defaultWriteOptions sch [batch]))
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


prop_expandDictionary :: Property
prop_expandDictionary = withTests 200 . property $ do
  f <- forAll (genField 0 "x")
  n <- forAll (Gen.int (Range.linear 0 10))
  c <- forAll (genColumnFor f n)
  e <- evalEither (expandDictionary c)
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
