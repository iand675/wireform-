{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Generated round-trip and algebraic properties for wireform-arrow.

Columns are compared through the logical row model of
"Test.Arrow.Gen" ('columnValues') and through the logical 'Eq' of
'ColumnArray', so dictionary unification, offset rebasing, slicing and
NaN bit patterns do not cause spurious mismatches while any change of
meaning does.

Besides the round trips, these properties pin the representation's
guarantees: decoded columns alias the input buffer (and stay correct
when the input is misaligned), 'copyColumn' detaches from it, the
writer's output is a fixed point of decode then encode, and the lazy
encoders produce the strict encoders' bytes.
-}
module Test.Arrow.Props (tests) where

import Arrow.Column (
  ColumnArray,
  PrimType (..),
  bitmapOffset,
  columnLength,
  columnTag,
  concatColumnArray,
  copyColumn,
  expandDictionary,
  fromBools,
  fromTexts,
  hasValiditySlot,
  mkDictionary,
  mkFixedSizeList,
  mkList,
  mkStruct,
  nullCount,
  primColumn,
  sliceColumnArray,
  takeColumnArray,
  validateMapKeysSorted,
  validity,
  validityBits,
  validityFromBools,
  validityNullCount,
  pattern ColDictionary,
  pattern ColFixedSizeList,
  pattern ColLargeList,
  pattern ColLargeBinary,
  pattern ColLargeUtf8,
  pattern ColBinary,
  pattern ColList,
  pattern ColMap,
  pattern ColNull,
  pattern ColRunEndEncoded,
  pattern ColStruct,
  pattern ColUtf8,
 )
import Arrow.Column.Internal qualified as I
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
  encodeArrowFileLazy,
  encodeArrowStream,
  encodeArrowStreamLazy,
  openStreamReader,
  streamReaderProjected,
  streamReaderToList,
 )
import Arrow.Types
import Arrow.Write (writeArrowFile, writeArrowStream)
import Control.Monad (forM_, replicateM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.List (elemIndex)
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, minusPtr, plusPtr)
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
      , ("decoded columns alias an aligned input and are correct for a misaligned one", prop_zeroCopyDecode)
      , ("copyColumn detaches a decoded column from the input", prop_copyColumnDetaches)
      , ("decode then encode reproduces writer output byte for byte", prop_canonicalRoundTrip)
      , ("lazy and strict encoders produce identical bytes", prop_lazyStrictIdentical)
      ]


-- ============================================================
-- Helpers
-- ============================================================

-- | Compare two tables by logical rows, logical 'Eq' and top-level tag.
sameTable :: MonadTest m => [V.Vector ColumnArray] -> [V.Vector ColumnArray] -> m ()
sameTable expected actual = do
  length actual === length expected
  forM_ (zip expected actual) $ \(e, a) -> do
    map tag (V.toList a) === map tag (V.toList e)
    evalEither (batchValues a) >>= \av -> evalEither (batchValues e) >>= \ev -> av === ev
    V.toList a === V.toList e


tag :: ColumnArray -> String
tag = columnTag


values :: MonadTest m => ColumnArray -> m [Value]
values c = V.toList <$> evalEither (columnValues c)


-- | Fixture construction that cannot fail unless the test itself is wrong.
fixture :: Either String ColumnArray -> ColumnArray
fixture = either (error . ("Test.Arrow.Props: bad fixture: " ++)) id


-- | Constructor names of a column and all its children (dictionary keys and values included).
allTags :: ColumnArray -> [String]
allTags c = tag c : concatMap allTags (childColumns c)


-- | Whether any column in the tree (dictionary values included) satisfies the predicate.
anyColumn :: (ColumnArray -> Bool) -> ColumnArray -> Bool
anyColumn p c = p c || any (anyColumn p) (childColumns c)


fieldlessStruct, sizeZeroList, nestedDictionary, emptyDictionary, withNulls, offsetBase, bitOffset, denormalisedValidity, windowedRuns :: ColumnArray -> Bool
fieldlessStruct = \case
  ColStruct _ _ cs -> V.null cs
  _ -> False
sizeZeroList = \case
  ColFixedSizeList 0 _ _ _ -> True
  _ -> False
nestedDictionary = \case
  ColDictionary _ _ v -> hasDictionaries v
  _ -> False
emptyDictionary = \case
  ColDictionary _ keys v -> columnLength v == 0 && columnLength keys > 0
  _ -> False
withNulls c = nullCount c > 0
-- Offsets that do not start at zero (a window of a longer column).
offsetBase = \case
  ColUtf8 _ o _ -> firstNonZero o
  ColBinary _ o _ -> firstNonZero o
  ColLargeUtf8 _ o _ -> firstNonZero o
  ColLargeBinary _ o _ -> firstNonZero o
  ColList _ o _ -> firstNonZero o
  ColLargeList _ o _ -> firstNonZero o
  ColMap _ o _ _ -> firstNonZero o
  _ -> False
  where
    firstNonZero :: (VS.Storable o, Eq o, Num o) => VS.Vector o -> Bool
    firstNonZero o = not (VS.null o) && VS.head o /= 0
bitOffset c = maybe False ((/= 0) . bitmapOffset . validityBits) (validity c)
denormalisedValidity c = maybe False ((== 0) . validityNullCount) (validity c)
windowedRuns = \case
  ColRunEndEncoded off _ _ _ -> off /= 0
  _ -> False


{- | Require the generator to reach the shapes the properties are about.
Nullability is a property of the field (a nullable field's column may
happen to hold no nulls), so the nullable shapes are counted on the
schema of a table with at least one batch; the physical shapes are
counted on the columns.
-}
coverShapes :: MonadTest m => Schema -> [ColumnArray] -> m ()
coverShapes sch cols = do
  let tags = concatMap allTags cols
      has t = t `elem` tags
      some p = any (anyColumn p) cols
      fields = concatMap fieldTree (V.toList (arrowFields sch))
      fieldTree f = f : concatMap fieldTree (V.toList (fieldChildren f))
      nullableField p = not (null cols) && any (\f -> fieldNullable f && p f) fields
      plainType p f = isNothing (fieldDictionary f) && p (fieldType f)
  forM_
    ["ColDenseUnion", "ColSparseUnion", "ColRunEndEncoded", "ColListView", "ColFixedSizeList", "ColDictionary", "ColUtf8View", "ColNull"]
    (\t -> cover 1 (fromString t) (has t))
  cover 1 "nullable large list view" (nullableField (plainType (== ALargeListView)))
  cover 1 "nullable map" (nullableField (plainType (\case AMap _ -> True; _ -> False)))
  cover 1 "nullable dictionary" (nullableField (\f -> fieldDictionary f /= Nothing))
  cover 1 "nullable decimal128" (nullableField (plainType (\case ADecimal {} -> True; _ -> False)))
  cover 1 "nullable decimal256" (nullableField (plainType (\case ADecimal256 {} -> True; _ -> False)))
  cover 1 "nullable binary view" (nullableField (plainType (== ABinaryView)))
  cover 1 "nullable struct" (nullableField (plainType (== AStruct)))
  cover 1 "nullable interval" (nullableField (plainType (\case AInterval _ -> True; _ -> False)))
  cover 1 "nulls in a column" (some withNulls)
  cover 1 "struct with no fields" (some fieldlessStruct)
  cover 1 "fixed-size list of size 0" (some sizeZeroList)
  cover 1 "dictionary inside dictionary values" (some nestedDictionary)
  cover 1 "all-null rows over an empty dictionary" (some emptyDictionary)
  cover 1 "offsets with a non-zero base" (some offsetBase)
  cover 1 "validity at a non-zero bit offset" (some bitOffset)
  cover 1 "validity present with zero nulls" (some denormalisedValidity)
  cover 1 "run-end-encoded window with a logical offset" (some windowedRuns)


-- | A single generated column (any type, nested to depth 3) with its field.
genColumn :: Gen (Field, ColumnArray)
genColumn = do
  f <- genField 3 "x"
  n <- Gen.int (Range.linear 0 10)
  c <- genColumnFor f n
  pure (f, c)


-- | Whether the bytes of @inner@ lie inside the memory of @outer@.
regionWithin :: ByteString -> ByteString -> Bool
regionWithin outer inner =
  let (fo, lo) = BSI.toForeignPtr0 outer
      (fi, li) = BSI.toForeignPtr0 inner
      d = unsafeForeignPtrToPtr fi `minusPtr` unsafeForeignPtrToPtr fo
  in d >= 0 && d + li <= lo


-- | The bytes copied to a fresh 64-byte aligned buffer at byte offset @shift@.
placedAt :: Int -> ByteString -> ByteString
placedAt shift bs =
  BS.drop shift $ I.createAligned (BS.length bs + shift) $ \p ->
    BSU.unsafeUseAsCStringLen bs $ \(src, n) -> copyBytes (p `plusPtr` shift) (castPtr src) n


-- | Every column of a table, children included.
allColumns :: [V.Vector ColumnArray] -> [ColumnArray]
allColumns = concatMap (concatMap tree . V.toList)
  where
    tree c = c : concatMap tree (childColumns c)


-- ============================================================
-- Round trips
-- ============================================================

prop_streamRoundTrip :: Property
prop_streamRoundTrip = withTests 1000 . property $ do
  (sch, batches) <- forAll genTable
  coverShapes sch (concatMap V.toList batches)
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
  let twice = sliceColumnArray s2 l2 (sliceColumnArray s1 l1 c)
      once = sliceColumnArray (s1 + s2) l2 c
  a <- values twice
  b <- values once
  a === b
  twice === once


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
  joined === c


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
  taken <- evalEither (takeColumnArray (VS.fromList ix) c)
  vs <- values c
  tvs <- values taken
  tvs === map (vs !!) ix
  if n > 0 then pure () else assert (not (isRight (takeColumnArray (VS.singleton 0) c)))


{- | Expansion keeps every row's value; a nullable dictionary column
whose rows are all null over an empty dictionary expands to an
all-null column of the value type. A dictionary's null rows stay null
in the expansion (the expanded column can hold them).
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
    ColDictionary _ keys _ -> do
      assert (hasValiditySlot e || nullCount keys == 0)
      assert (nullCount e >= nullCount keys)
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
  let o = VS.fromList (map fromIntegral offs)
      vals = ColNull total
      -- Raw construction: null keys are not something mkMap has to accept.
      col = I.ColMap (if nullableMap then validityFromBools (V.fromList mapValid) else Nothing) o keys vals
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
  ix <- forAll (replicateM n (Gen.int (Range.linear 0 (k1 + k2 - 1))))
  let dict isDelta vals =
        let (drb, dbody) = layoutBatch valuesSchema (V.singleton vals)
            db = DictBatch {dbId = 7, dbIsDelta = isDelta, dbData = drb, dbBody = dbody}
        in encapsulateMessage (buildDictionaryBatchMessage db) dbody
      -- Raw: the keys address values this batch does not carry yet.
      keys = keyColumn idxTy (V.fromList (map Just ix))
      (rb, body) = layoutBatch sch (V.singleton (I.ColDictionary 7 keys part1))
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
      gv === map (allVals !!) ix
    _ -> failure


{- | A batch with no dictionary rows and no dictionary batch keeps a
typed empty placeholder whose constructor matches the value type, and
empty keys at the index type's width.
-}
prop_dictionaryPlaceholder :: Property
prop_dictionaryPlaceholder = withTests 200 . property $ do
  ty <- forAll (Gen.filter (/= ANull) genFieldType)
  nullable <- forAll Gen.bool
  let field = Field "d" nullable ty V.empty (Just (DictionaryEncoding 3 (AInt 32 True) False)) V.empty
      sch = defaultSchema (V.singleton field)
  empty <- forAll (genColumnFor field 0)
  sample <- forAll (genColumnFor field {fieldDictionary = Nothing, fieldNullable = False} 0)
  let bytes = writeArrowStreamFBWithDicts sch [] [layoutBatch sch (V.singleton empty)]
  (_, got) <- evalEither (decodeArrowStream bytes)
  case got of
    [cols] | [c] <- V.toList cols -> case c of
      ColDictionary _ keys vals -> do
        columnLength keys === 0
        tag keys === "ColInt32"
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
  map fieldNullable (V.toList (arrowFields sch)) === replicate 5 True
  (_, viaStream) <- evalEither (decodeArrowFile bytes)
  map (map tag . V.toList) viaStream
    === replicate 2 ["ColInt32", "ColUtf8", "ColDictionary", "ColDecimal128", "ColList"]
  map (map nullCount . V.toList) viaStream === [[0, 1, 1, 1, 1], [0, 0, 0, 1, 0]]
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
      mask nullable = if nullable then validityFromBools valid else Nothing
      structCol = fixture (mkStruct n (mask structNullable) V.empty)
      listCol = fixture (mkFixedSizeList 0 n (mask listNullable) (primColumn PInt32 VS.empty))
      -- Every list row holds two fieldless structs.
      nestedCol = fixture (mkList Nothing (VS.generate (n + 1) (fromIntegral . (* 2))) (fixture (mkStruct (2 * n) Nothing V.empty)))
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
      innerKeys = primColumn PInt8 . VS.fromList . map fromIntegral
      outerKeys = primColumn PInt16 . VS.fromList . map fromIntegral
      -- Raw: keys address dictionaries sent in separate batches.
      innerPlaceholder = I.ColDictionary 2 (innerKeys []) (fromTexts V.empty)
      dictMsg schema did isDelta col =
        let (drb, dbody) = layoutBatch schema (V.singleton col)
            db = DictBatch {dbId = did, dbIsDelta = isDelta, dbData = drb, dbBody = dbody}
        in encapsulateMessage (buildDictionaryBatchMessage db) dbody
      inner isDelta ws = dictMsg (defaultSchema (V.singleton e {fieldDictionary = Nothing})) 2 isDelta (fromTexts (V.fromList ws))
      outer isDelta ix =
        dictMsg
          (defaultSchema (V.singleton o {fieldDictionary = Nothing}))
          1
          isDelta
          (I.ColStruct (length ix) Nothing (V.singleton ("e", I.ColDictionary 2 (innerKeys ix) (fromTexts V.empty))))
      batch ix =
        let (rb, body) = layoutBatch sch (V.singleton (I.ColDictionary 1 (outerKeys ix) (I.ColStruct 0 Nothing (V.singleton ("e", innerPlaceholder)))))
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
      dictCol prefix k = fixture (mkDictionary 0 (primColumn PInt8 (VS.generate k fromIntegral)) (fromTexts (V.generate k (\i -> prefix <> T.pack (show i)))))
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
      ix k = primColumn PInt32 (VS.generate k fromIntegral)
      structBatch nm k = V.singleton (fixture (mkDictionary 0 (ix k) (fixture (mkStruct k Nothing (V.singleton (nm, ix k))))))
      renamed = [structBatch "x" n1, structBatch "y" n2]
      wrongType = [structBatch "x" n1, V.singleton (fixture (mkDictionary 0 (ix n2) (fromTexts (V.replicate n2 "v"))))]
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
count disagrees with its child (built raw: the public constructors
refuse them). The unbroken batch writes.
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
          let wrong = if fieldType f0 == ABool && isNothing (fieldDictionary f0) then ColNull n else fromBools (V.replicate n False)
          in (sch, V.cons wrong (V.tail batch))
        "length" -> (withFields (f0 : f0 {fieldName = "dup"} : V.toList (V.tail (arrowFields sch))), V.cons c0 (V.cons doubled (V.tail batch)))
        "struct" ->
          ( withFields [Field "s" False AStruct (V.singleton f0) Nothing V.empty]
          , V.singleton (I.ColStruct (n + 1) Nothing (V.singleton (fieldName f0, c0)))
          )
        _ ->
          ( withFields [Field "l" False (AFixedSizeList 1) (V.singleton f0) Nothing V.empty]
          , V.singleton (I.ColFixedSizeList 1 (n + 1) Nothing c0)
          )
      replace = defaultWriteOptions {writeDictHandling = DictReplaceOnChange}
  assert (isRight (encodeArrowStream defaultWriteOptions sch [batch]))
  annotateShow (encodeArrowStream defaultWriteOptions sch' [bad])
  assert (isLeft (encodeArrowStream defaultWriteOptions sch' [bad]))
  assert (isLeft (encodeArrowStream replace sch' [bad]))
  assert (isLeft (encodeArrowFile defaultWriteOptions sch' [bad]))
  assert (isLeft (writeArrowStream sch' (V.singleton bad)))
  assert (isLeft (writeArrowFile sch' (V.singleton bad)))


-- ============================================================
-- Representation guarantees
-- ============================================================

{- | Flat fixed-width, boolean, var-length and view columns decode
without copying: placed in an aligned buffer, every buffer of every
decoded column lies inside the input. Placed one byte off alignment
(so 2, 4, 8 and 16-byte elements are misaligned) the decode still
yields the same rows.
-}
prop_zeroCopyDecode :: Property
prop_zeroCopyDecode = withTests 300 . property $ do
  k <- forAll (Gen.int (Range.linear 1 4))
  fields <- forAll $ forM' [0 .. k - 1] $ \i -> do
    ty <- Gen.filter (/= ANull) genFieldType
    nullable <- Gen.bool
    pure (Field (T.pack ("c" ++ show i)) nullable ty V.empty Nothing V.empty)
  let sch = defaultSchema (V.fromList fields)
  nBatches <- forAll (Gen.int (Range.linear 1 3))
  batches <- forAll (replicateM nBatches (genBatchFor sch))
  bytes <- evalEither (encodeArrowStream defaultWriteOptions sch batches)
  let aligned = placedAt 0 bytes
      misaligned = placedAt 1 bytes
  (_, got) <- evalEither (decodeArrowStream aligned)
  sameTable batches got
  let outside = filter (\b -> not (BS.null b) && not (regionWithin aligned b)) (concatMap I.columnBuffers (allColumns got))
  annotateShow (length outside)
  assert (null outside)
  cover 10 "has a validity bitmap" (any (any (\c -> nullCount c > 0) . V.toList) got)
  (_, gotMis) <- evalEither (decodeArrowStream misaligned)
  sameTable batches gotMis


{- | 'copyColumn' of any decoded column is equal to it and shares no
memory with the input bytes.
-}
prop_copyColumnDetaches :: Property
prop_copyColumnDetaches = withTests 200 . property $ do
  (sch, batches) <- forAll genTable
  bytes <- evalEither (encodeArrowStream defaultWriteOptions sch batches)
  (_, got) <- evalEither (decodeArrowStream bytes)
  let copies = map (V.map copyColumn) got
  sameTable got copies
  let attached = filter (\b -> not (BS.null b) && regionWithin bytes b) (concatMap I.columnBuffers (allColumns copies))
  annotateShow (length attached)
  assert (null attached)


{- | Writer output is canonical: decoding it and encoding the decoded
schema and columns again with the same options gives the same bytes,
for streams and files, every dictionary mode and body compression.
-}
prop_canonicalRoundTrip :: Property
prop_canonicalRoundTrip = withTests 300 . property $ do
  (sch, batches) <- forAll genTable
  mode <- forAll (Gen.element [DictEmitOnce, DictReplaceOnChange])
  codec <- forAll (Gen.element [Nothing, Just LZ4Frame, Just BodyZstd])
  let opts = defaultWriteOptions {writeDictHandling = mode, writeBodyCompression = codec}
  stream <- evalEither (encodeArrowStream opts sch batches)
  (sch1, got1) <- evalEither (decodeArrowStream stream)
  again <- evalEither (encodeArrowStream opts sch1 got1)
  again === stream
  file <- evalEither (encodeArrowFile opts sch batches)
  (sch2, got2) <- evalEither (decodeArrowFile file)
  againFile <- evalEither (encodeArrowFile opts sch2 got2)
  againFile === file


-- | The lazy encoders' output, made strict, is the strict encoders' output.
prop_lazyStrictIdentical :: Property
prop_lazyStrictIdentical = withTests 200 . property $ do
  (sch, batches) <- forAll genTable
  mode <- forAll (Gen.element [DictEmitOnce, DictReplaceOnChange])
  codec <- forAll (Gen.element [Nothing, Just LZ4Frame, Just BodyZstd])
  let opts = defaultWriteOptions {writeDictHandling = mode, writeBodyCompression = codec}
  strict <- evalEither (encodeArrowStream opts sch batches)
  lazy <- evalEither (encodeArrowStreamLazy opts sch batches)
  BL.toStrict lazy === strict
  strictFile <- evalEither (encodeArrowFile opts sch batches)
  lazyFile <- evalEither (encodeArrowFileLazy opts sch batches)
  BL.toStrict lazyFile === strictFile


forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' xs f = mapM f xs


-- | 'buildRecordBatchBytes' for hand-built fixtures that must lay out.
layoutBatch :: Schema -> V.Vector ColumnArray -> (RecordBatchDef, ByteString)
layoutBatch sch cols = either (error . ("Test.Arrow.Props: fixture does not lay out: " ++)) id (buildRecordBatchBytes sch cols)
