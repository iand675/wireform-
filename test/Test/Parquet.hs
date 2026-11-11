module Test.Parquet (parquetTests) where

import Data.Bits (shiftL)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Either (isLeft)
import Data.Int (Int32, Int64)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric (showHex)
import Parquet.BloomFilter (
  decodeBloomFilter,
  encodeBloomFilter,
  newSbbf,
  optimalNumBytes,
  sbbfCheck,
  sbbfInsert,
  sbbfNumBytes,
 )
import Parquet.Footer
import Parquet.Levels (
  NestedValue (..),
  materializePlainBoolOptional,
  materializePlainByteArrayOptional,
  materializePlainInt32Optional,
  materializePlainInt64Optional,
  materializeRepeatedByNested,
  materializeRepeatedDouble,
  materializeRepeatedFloat,
  materializeRepeatedInt32,
  materializeRepeatedInt64,
  maxLevelsForColumnPath,
  parseDataPageV1Levels,
 )
import Parquet.Page
import Parquet.PageIndex (
  decodeColumnIndex,
  decodeOffsetIndex,
  encodeColumnIndex,
  encodeOffsetIndex,
 )
import Parquet.Read
import Parquet.Types
import Parquet.Write
import Test.Syd
import Test.Syd.Hedgehog ()
import Thrift.Encode (encodeCompact)
import Thrift.Value qualified as TV
import Wireform.Hash qualified as Hash


parquetTests :: Spec
parquetTests =
  describe "Parquet" $
    sequence_
      [ footerRoundtrips
      , plainDecoderTests
      , hybridRleDecoderTests
      , levelsAndSchemaTests
      , magicTests
      , edgeCases
      , propertyRoundtrips
      , dictionaryOptionalTests
      , deltaBinaryPackedTests
      , dataPageV2Tests
      , writerRoundtripTests
      , pageIndexTests
      , xxh64Tests
      , bloomFilterTests
      ]


plainDecoderTests :: Spec
plainDecoderTests =
  describe "PLAIN column decoders" $
    sequence_
      [ it "INT32 little-endian" $ do
          let bs = BS.pack [0x07, 0x00, 0x00, 0x00, 0xFE, 0xFF, 0xFF, 0xFF]
          decodePlainInt32 2 bs `shouldBe` Right (VP.fromList [(7 :: Int32), (-2 :: Int32)])
      , it "INT64 little-endian" $ do
          let bs =
                BS.pack
                  [ 0x2A
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0xFF
                  , 0xFF
                  , 0xFF
                  , 0xFF
                  , 0xFF
                  , 0xFF
                  , 0xFF
                  , 0xFF
                  ]
          decodePlainInt64 2 bs `shouldBe` Right (VP.fromList [(42 :: Int64), (-1 :: Int64)])
      , it "DOUBLE little-endian" $ do
          let bs = BS.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
          decodePlainDouble 1 bs `shouldBe` Right (VP.fromList [0.0])
      , it "BYTE_ARRAY length-prefixed" $ do
          let bs = BS.pack [0x03, 0x00, 0x00, 0x00, 0x61, 0x62, 0x63, 0x01, 0x00, 0x00, 0x00, 0x7A]
          decodePlainByteArray 2 bs
            `shouldBe` Right (V.fromList [BS.pack [0x61, 0x62, 0x63], BS.pack [0x7A]])
      ]


hybridRleDecoderTests :: Spec
hybridRleDecoderTests =
  describe "Hybrid RLE" $
    sequence_
      [ describe "dictionary indices (width byte + body)" $
          sequence_
            [ it "RLE run of zeros (width 2)" $ do
                let bs = BS.pack [2, 0x06, 0x00]
                decodeDictionaryIndices 3 bs
                  `shouldBe` Right (VP.fromList [(0 :: Int32), 0, 0])
            , it "Bit-packed 0..7 (width 3, Apache example bytes)" $ do
                let bs = BS.pack [3, 0x03, 0x88, 0xC6, 0xFA]
                decodeDictionaryIndices 8 bs
                  `shouldBe` Right (VP.fromList (map (fromIntegral @Int @Int32) [0 .. 7]))
            ]
      , describe "length-prefixed body (data page v1 levels)" $
          sequence_
            [ it "same RLE run as dictionary case (inner = tail of dict layout)" $ do
                let inner = BS.pack [0x06, 0x00]
                    bs = BS.append (BS.pack [0x02, 0x00, 0x00, 0x00]) inner
                decodeHybridRleLengthPrefixed 2 3 bs
                  `shouldBe` Right (VP.fromList [(0 :: Int32), 0, 0])
            , it "same bit-packed run as dictionary case" $ do
                let inner = BS.pack [0x03, 0x88, 0xC6, 0xFA]
                    bs = BS.append (BS.pack [0x04, 0x00, 0x00, 0x00]) inner
                decodeHybridRleLengthPrefixed 3 8 bs
                  `shouldBe` Right (VP.fromList (map (fromIntegral @Int @Int32) [0 .. 7]))
            , it "reject declared length past buffer" $ do
                let bs = BS.pack [0xFF, 0x00, 0x00, 0x00, 0x00]
                case decodeHybridRleLengthPrefixed 1 1 bs of
                  Left _ -> pure ()
                  Right v -> expectationFailure ("expected Left, got " ++ show v)
            ]
      ]


levelsAndSchemaTests :: Spec
levelsAndSchemaTests =
  describe "Levels + schema max levels" $
    sequence_
      [ it "maxLevels optional and required leaves" $ do
          let schOpt =
                V.fromList
                  [ SchemaElement (T.pack "schema") Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                  , SchemaElement (T.pack "x") (Just Optional) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                  ]
              schReq =
                V.fromList
                  [ SchemaElement (T.pack "schema") Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                  , SchemaElement (T.pack "y") (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                  ]
          maxLevelsForColumnPath schOpt (V.singleton (T.pack "x")) `shouldBe` Right (0, 1)
          maxLevelsForColumnPath schReq (V.singleton (T.pack "y")) `shouldBe` Right (0, 0)
      , it "materializePlainInt32Optional with null middle" $ do
          let defs = VP.fromList [(1 :: Int32), 0, 1]
              plain = BS.pack [0x0A, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00]
          materializePlainInt32Optional defs 1 plain
            `shouldBe` Right (V.fromList [Just 10, Nothing, Just 3])
      , it "parseDataPageV1Levels + materialize (all present)" $ do
          let defSec =
                BS.append
                  (BS.pack [0x02, 0x00, 0x00, 0x00])
                  (BS.pack [0x06, 0x01])
              plain = BS.pack [0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00]
              raw = BS.append defSec plain
          case parseDataPageV1Levels 0 1 3 raw of
            Left e -> expectationFailure e
            Right (rep, def, rest) -> do
              rep `shouldBe` VP.replicate 3 (0 :: Int32)
              def `shouldBe` VP.replicate 3 (1 :: Int32)
              rest `shouldBe` plain
              materializePlainInt32Optional def 1 rest
                `shouldBe` Right (V.fromList [Just 1, Just 2, Just 3])
      , it "materializePlainInt64Optional with null" $ do
          let defs = VP.fromList [(1 :: Int32), 0, 1]
              plain =
                BS.append
                  (BS.pack [0xE8, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
                  (BS.pack [0xD0, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
          materializePlainInt64Optional defs 1 plain
            `shouldBe` Right (V.fromList [Just 1000, Nothing, Just 2000])
      , it "materializePlainBoolOptional two defined" $ do
          let defs = VP.fromList [(1 :: Int32), 1, 0]
              plain = BS.pack [0x03]
          materializePlainBoolOptional defs 1 plain
            `shouldBe` Right (V.fromList [Just True, Just True, Nothing])
      , it "materializePlainByteArrayOptional null second row" $ do
          let defs = VP.fromList [(1 :: Int32), 0]
              plain = BS.pack [0x02, 0x00, 0x00, 0x00, 0x61, 0x62]
          materializePlainByteArrayOptional defs 1 plain
            `shouldBe` Right (V.fromList [Just (BS.pack [0x61, 0x62]), Nothing])
      , it "materializeRepeatedInt32 two lists of ints" $ do
          -- row 0 = [10, 20], row 1 = [30]
          let reps = VP.fromList [(0 :: Int32), 1, 0]
              defs = VP.replicate 3 (1 :: Int32) -- all present, maxDef=1
              plain =
                BS.pack
                  [ 0x0A
                  , 0x00
                  , 0x00
                  , 0x00 -- 10
                  , 0x14
                  , 0x00
                  , 0x00
                  , 0x00 -- 20
                  , 0x1E
                  , 0x00
                  , 0x00
                  , 0x00 -- 30
                  ]
          materializeRepeatedInt32 reps defs 1 plain
            `shouldBe` Right
              ( V.fromList
                  [ V.fromList [Just 10, Just 20]
                  , V.fromList [Just 30]
                  ]
              )
      , it "materializeRepeatedInt64 with null element" $ do
          let reps = VP.fromList [(0 :: Int32), 1, 0]
              defs = VP.fromList [(1 :: Int32), 0, 1]
              plain =
                BS.pack
                  [ 0xE8
                  , 0x03
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x00 -- 1000
                  , 0xD0
                  , 0x07
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x00 -- 2000
                  ]
          materializeRepeatedInt64 reps defs 1 plain
            `shouldBe` Right
              ( V.fromList
                  [ V.fromList [Just 1000, Nothing]
                  , V.fromList [Just 2000]
                  ]
              )
      , it "materializeRepeatedFloat single row" $ do
          let reps = VP.fromList [(0 :: Int32), 1]
              defs = VP.replicate 2 (1 :: Int32)
              -- 1.0 = 0x3F800000, 2.0 = 0x40000000
              plain =
                BS.pack
                  [ 0x00
                  , 0x00
                  , 0x80
                  , 0x3F
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x40
                  ]
          materializeRepeatedFloat reps defs 1 plain
            `shouldBe` Right (V.singleton (V.fromList [Just 1.0, Just 2.0]))
      , it "materializeRepeatedDouble single row" $ do
          let reps = VP.fromList [(0 :: Int32)]
              defs = VP.replicate 1 (1 :: Int32)
              -- 1.5 as little-endian double: 0x3FF8000000000000
              plain =
                BS.pack
                  [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF8, 0x3F]
          materializeRepeatedDouble reps defs 1 plain
            `shouldBe` Right (V.singleton (V.singleton (Just 1.5)))
      , it "materializeRepeatedByNested 2-deep LIST<LIST<int32>>" $ do
          -- Two top-level rows:
          --   row 0 = [[10, 20], [30]]
          --   row 1 = [[40, 50]]
          let reps = VP.fromList [(0 :: Int32), 2, 1, 0, 2]
              defs = VP.replicate 5 (2 :: Int32)
              plain =
                BS.pack
                  [ 0x0A
                  , 0x00
                  , 0x00
                  , 0x00 -- 10
                  , 0x14
                  , 0x00
                  , 0x00
                  , 0x00 -- 20
                  , 0x1E
                  , 0x00
                  , 0x00
                  , 0x00 -- 30
                  , 0x28
                  , 0x00
                  , 0x00
                  , 0x00 -- 40
                  , 0x32
                  , 0x00
                  , 0x00
                  , 0x00 -- 50
                  ]
              decI32 :: BS.ByteString -> Int -> Either String (Int32, Int)
              decI32 bs off
                | off + 4 > BS.length bs = Left "short"
                | otherwise =
                    let !v =
                          fromIntegral (BS.index bs (off + 0))
                            + (fromIntegral (BS.index bs (off + 1)) `shiftL` 8)
                            + (fromIntegral (BS.index bs (off + 2)) `shiftL` 16)
                            + (fromIntegral (BS.index bs (off + 3)) `shiftL` 24)
                    in Right (v :: Int32, off + 4)
          result <- case materializeRepeatedByNested reps defs 2 2 plain decI32 of
            Right v -> pure v
            Left e -> expectationFailure e
          let l xs = NVList xs
              n v = NVLeaf (Just v)
          V.toList result
            `shouldBe` [ l [l [n 10, n 20], l [n 30]]
                       , l [l [n 40, n 50]]
                       ]
      , it "materializeRepeatedByNested 3-deep LIST<LIST<LIST<int32>>>" $ do
          -- One row representing [[[1,2],[3]],[[4]]]:
          --   leaf 1: rep=0 (start row), depth 3
          --   leaf 2: rep=3 (innermost continuation)
          --   leaf 3: rep=2 (new innermost list at second level)
          --   leaf 4: rep=1 (new at top)
          let reps = VP.fromList [(0 :: Int32), 3, 2, 1]
              defs = VP.replicate 4 (3 :: Int32)
              plain =
                BS.pack
                  [ 0x01
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x02
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x03
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x04
                  , 0x00
                  , 0x00
                  , 0x00
                  ]
              decI32 :: BS.ByteString -> Int -> Either String (Int32, Int)
              decI32 bs off =
                let !v =
                      fromIntegral (BS.index bs (off + 0))
                        + (fromIntegral (BS.index bs (off + 1)) `shiftL` 8)
                        + (fromIntegral (BS.index bs (off + 2)) `shiftL` 16)
                        + (fromIntegral (BS.index bs (off + 3)) `shiftL` 24)
                in Right (v :: Int32, off + 4)
          result <- case materializeRepeatedByNested reps defs 3 3 plain decI32 of
            Right v -> pure v
            Left e -> expectationFailure e
          let l xs = NVList xs
              n v = NVLeaf (Just v)
          V.toList result
            `shouldBe` [ l
                           [ l [l [n 1, n 2], l [n 3]]
                           , l [l [n 4]]
                           ]
                       ]
      , it "materializeRepeatedByNested rep=1 reduces to 1-level" $ do
          -- maxRep=1 → behaves like materializeRepeatedInt32. row 0 =
          -- [v0, v1], row 1 = [v2].
          let reps = VP.fromList [(0 :: Int32), 1, 0]
              defs = VP.replicate 3 (1 :: Int32)
              plain =
                BS.pack
                  [ 0x01
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x02
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x03
                  , 0x00
                  , 0x00
                  , 0x00
                  ]
              decI32 :: BS.ByteString -> Int -> Either String (Int32, Int)
              decI32 bs off =
                let !v =
                      fromIntegral (BS.index bs (off + 0))
                        + (fromIntegral (BS.index bs (off + 1)) `shiftL` 8)
                        + (fromIntegral (BS.index bs (off + 2)) `shiftL` 16)
                        + (fromIntegral (BS.index bs (off + 3)) `shiftL` 24)
                in Right (v :: Int32, off + 4)
          result <- case materializeRepeatedByNested reps defs 1 1 plain decI32 of
            Right v -> pure v
            Left e -> expectationFailure e
          let l xs = NVList xs
              n v = NVLeaf (Just v)
          V.toList result
            `shouldBe` [ l [n 1, n 2]
                       , l [n 3]
                       ]
      ]


footerRoundtrips :: Spec
footerRoundtrips =
  describe "Footer roundtrips" $
    sequence_
      [ it "Minimal file metadata" $ do
          let fm =
                FileMetadata
                  { fmVersion = 2
                  , fmSchema =
                      V.fromList
                        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                        , SchemaElement "value" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                        ]
                  , fmNumRows = 100
                  , fmRowGroups = V.empty
                  , fmCreatedBy = Just "wireform"
                  , fmKeyValueMetadata = Nothing
                  , fmColumnOrders = Nothing
                  }
          readFooter (writeFooter fm) `shouldBe` Right fm
      , it "File metadata with row group" $ do
          let cm =
                ColumnMetadata
                  PTInt64
                  (V.fromList [Plain, RLE])
                  (V.fromList ["value"])
                  Snappy
                  1000
                  8000
                  4000
                  4
                  Nothing
                  Nothing
                  Nothing
                  Nothing
              cc = ColumnChunk Nothing 4 (Just cm) Nothing Nothing Nothing Nothing
              rg = RowGroup (V.singleton cc) 4000 1000 Nothing
              fm =
                FileMetadata
                  2
                  ( V.fromList
                      [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                      , SchemaElement "value" (Just Optional) (Just PTInt64) Nothing Nothing Nothing Nothing Nothing
                      ]
                  )
                  1000
                  (V.singleton rg)
                  Nothing
                  Nothing
                  Nothing
          readFooter (writeFooter fm) `shouldBe` Right fm
      , it "Multiple row groups" $ do
          let mkRG n = RowGroup V.empty (n * 1000) n Nothing
              fm =
                FileMetadata
                  1
                  (V.singleton (SchemaElement "root" Nothing Nothing (Just 0) Nothing Nothing Nothing Nothing))
                  3000
                  (V.fromList [mkRG 1000, mkRG 1000, mkRG 1000])
                  (Just "test-writer v1.0")
                  Nothing
                  Nothing
          readFooter (writeFooter fm) `shouldBe` Right fm
      , it "All parquet types" $ do
          let types = [PTBoolean, PTInt32, PTInt64, PTInt96, PTFloat, PTDouble, PTByteArray, PTFixedLenByteArray]
              mkSchema t = SchemaElement (T.pack (show t)) (Just Required) (Just t) Nothing Nothing Nothing Nothing (if t == PTFixedLenByteArray then Just 16 else Nothing)
              fm = FileMetadata 2 (V.fromList (map mkSchema types)) 0 V.empty Nothing Nothing Nothing
          readFooter (writeFooter fm) `shouldBe` Right fm
      , it "fmColumnOrders round-trips" $ do
          let orders = V.fromList [TypeDefinedOrder, TypeDefinedOrder, TypeDefinedOrder]
              schema =
                V.fromList
                  [ SchemaElement "schema" Nothing Nothing (Just 3) Nothing Nothing Nothing Nothing
                  , SchemaElement "a" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                  , SchemaElement "b" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                  , SchemaElement "c" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                  ]
              fm =
                FileMetadata
                  2
                  schema
                  0
                  V.empty
                  (Just "wireform")
                  (Just orders)
                  Nothing
          readFooter (writeFooter fm) `shouldBe` Right fm
      , it "rgSortingColumns round-trips" $ do
          let sc1 = SortingColumn 0 False True
              sc2 = SortingColumn 1 True False
              rg = RowGroup V.empty 0 100 (Just (V.fromList [sc1, sc2]))
              fm =
                FileMetadata
                  2
                  V.empty
                  100
                  (V.singleton rg)
                  Nothing
                  Nothing
                  Nothing
          readFooter (writeFooter fm) `shouldBe` Right fm
      ]


magicTests :: Spec
magicTests =
  describe "Magic" $
    sequence_
      [ it "Footer ends with PAR1" $ do
          let fm = FileMetadata 2 V.empty 0 V.empty Nothing Nothing Nothing
              bs = writeFooter fm
              magic = BS.drop (BS.length bs - 4) bs
          magic `shouldBe` parquetMagic
      , it "PAR1 magic bytes" $
          BS.unpack parquetMagic `shouldBe` [0x50, 0x41, 0x52, 0x31]
      ]


edgeCases :: Spec
edgeCases =
  describe "Edge cases" $
    sequence_
      [ it "Empty input fails" $
          case readFooter BS.empty of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on empty input"
      , it "Too short input fails" $
          case readFooter (BS.pack [1, 2, 3]) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on short input"
      , it "Wrong magic fails" $
          case readFooter (BS.pack [0, 0, 0, 0, 0, 0, 0, 0]) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on wrong magic"
      , it "All encodings round-trip" $ do
          let encs =
                [ Plain
                , PlainDictionary
                , RLE
                , BitPacked
                , DeltaBinaryPacked
                , DeltaLengthByteArray
                , DeltaByteArray
                , RLEDictionary
                , ByteStreamSplit
                ]
              cm = ColumnMetadata PTInt32 (V.fromList encs) (V.singleton "x") Uncompressed 0 0 0 0 Nothing Nothing Nothing Nothing
              cc = ColumnChunk Nothing 0 (Just cm) Nothing Nothing Nothing Nothing
              rg = RowGroup (V.singleton cc) 0 0 Nothing
              fm = FileMetadata 2 V.empty 0 (V.singleton rg) Nothing Nothing Nothing
          readFooter (writeFooter fm) `shouldBe` Right fm
      ]


propertyRoundtrips :: Spec
propertyRoundtrips =
  describe "Property roundtrips" $
    sequence_
      [ it "FileMetadata with random version and numRows" $ property $ do
          ver <- forAll $ Gen.int32 (Range.linear 1 3)
          nRows <- forAll $ Gen.int64 (Range.linear 0 1000000)
          createdBy <- forAll $ Gen.maybe (Gen.text (Range.linear 1 32) Gen.alphaNum)
          let fm = FileMetadata ver V.empty nRows V.empty createdBy Nothing Nothing
          readFooter (writeFooter fm) === Right fm
      , it "FileMetadata with random schema elements" $ property $ do
          nFields <- forAll $ Gen.int (Range.linear 0 5)
          fields <-
            forAll $
              traverse
                ( \_ -> do
                    name <- Gen.text (Range.linear 1 20) Gen.alphaNum
                    rep <- Gen.maybe (Gen.element [Required, Optional, Repeated])
                    pt <- Gen.maybe (Gen.element [PTBoolean, PTInt32, PTInt64, PTFloat, PTDouble, PTByteArray])
                    pure (SchemaElement name rep pt Nothing Nothing Nothing Nothing Nothing)
                )
                [1 .. nFields]
          let fm = FileMetadata 2 (V.fromList fields) 0 V.empty Nothing Nothing Nothing
          readFooter (writeFooter fm) === Right fm
      , it "FileMetadata with random row groups" $ property $ do
          nGroups <- forAll $ Gen.int (Range.linear 0 3)
          rgs <-
            forAll $
              traverse
                ( \_ -> do
                    nRows <- Gen.int64 (Range.linear 0 100000)
                    totalBytes <- Gen.int64 (Range.linear 0 1000000)
                    pure (RowGroup V.empty totalBytes nRows Nothing)
                )
                [1 .. nGroups]
          let fm = FileMetadata 2 V.empty (sum (map rgNumRows rgs)) (V.fromList rgs) Nothing Nothing Nothing
          readFooter (writeFooter fm) === Right fm
      ]


dictionaryOptionalTests :: Spec
dictionaryOptionalTests =
  describe "Dictionary optional columns" $
    sequence_
      [ it "dict page + data page with null" $ do
          let dictBody =
                BS.pack
                  [ 0x0A
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x14
                  , 0x00
                  , 0x00
                  , 0x00
                  , 0x1E
                  , 0x00
                  , 0x00
                  , 0x00
                  ]
              dictHdr =
                encodePageHeader
                  PageHeader
                    { phType = PtDictionaryPage (DictionaryPageHeader 3 0)
                    , phUncompressedPageSize = Just 12
                    , phCompressedPageSize = Just 12
                    }
              defLevels = BS.pack [0x02, 0x00, 0x00, 0x00, 0x03, 0x05]
              dictIndices = BS.pack [0x02, 0x03, 0x08, 0x00]
              dataBody = defLevels <> dictIndices
              dataHdr =
                encodePageHeader
                  PageHeader
                    { phType = PtDataPage (DataPageHeader 3 2)
                    , phUncompressedPageSize = Just (fromIntegral (BS.length dataBody))
                    , phCompressedPageSize = Just (fromIntegral (BS.length dataBody))
                    }
              chunk = dictHdr <> dictBody <> dataHdr <> dataBody
              lookupInt32 v idx =
                let i = fromIntegral idx :: Int
                in if i >= 0 && i < VP.length v then Just (VP.unsafeIndex v i) else Nothing
          case readDictionaryOptionalColumnChunk
            decodePlainInt32
            lookupInt32
            Uncompressed
            0
            1
            chunk of
            Left e -> expectationFailure e
            Right result -> result `shouldBe` V.fromList [Just (10 :: Int32), Nothing, Just 30]
      , it "all-null optional dictionary column" $ do
          let dictBody = BS.pack [0x07, 0x00, 0x00, 0x00]
              dictHdr =
                encodePageHeader
                  PageHeader
                    { phType = PtDictionaryPage (DictionaryPageHeader 1 0)
                    , phUncompressedPageSize = Just 4
                    , phCompressedPageSize = Just 4
                    }
              defLevels = BS.pack [0x02, 0x00, 0x00, 0x00, 0x03, 0x00]
              dictIndices = BS.pack [0x00]
              dataBody = defLevels <> dictIndices
              dataHdr =
                encodePageHeader
                  PageHeader
                    { phType = PtDataPage (DataPageHeader 2 2)
                    , phUncompressedPageSize = Just (fromIntegral (BS.length dataBody))
                    , phCompressedPageSize = Just (fromIntegral (BS.length dataBody))
                    }
              chunk = dictHdr <> dictBody <> dataHdr <> dataBody
              lookupInt32 v idx =
                let i = fromIntegral idx :: Int
                in if i >= 0 && i < VP.length v then Just (VP.unsafeIndex v i) else Nothing
          case readDictionaryOptionalColumnChunk
            decodePlainInt32
            lookupInt32
            Uncompressed
            0
            1
            chunk of
            Left e -> expectationFailure e
            Right result -> result `shouldBe` V.fromList [Nothing, Nothing :: Maybe Int32]
      ]


deltaBinaryPackedTests :: Spec
deltaBinaryPackedTests =
  describe "DELTA_BINARY_PACKED" $
    sequence_
      [ it "single value (header only)" $ do
          let bs = BS.pack [0x80, 0x01, 0x04, 0x01, 0x54]
          decodeDeltaBinaryPackedInt32 1 bs `shouldBe` Right (VP.fromList [42 :: Int32])
      , it "constant delta (bit_width=0)" $ do
          let bs = BS.pack [0x80, 0x01, 0x04, 0x03, 0xC8, 0x01, 0x14, 0x00, 0x00, 0x00, 0x00]
          decodeDeltaBinaryPackedInt32 3 bs `shouldBe` Right (VP.fromList [100, 110, 120 :: Int32])
      , it "variable deltas (bit_width=2)" $ do
          let bs = BS.pack [0x80, 0x01, 0x04, 0x04, 0x00, 0x02, 0x02, 0x00, 0x00, 0x00, 0x24] <> BS.replicate 7 0
          decodeDeltaBinaryPackedInt32 4 bs `shouldBe` Right (VP.fromList [0, 1, 3, 6 :: Int32])
      , it "INT64 variant" $ do
          let bs = BS.pack [0x80, 0x01, 0x04, 0x01, 0x54]
          decodeDeltaBinaryPackedInt64 1 bs `shouldBe` Right (VP.fromList [42 :: Int64])
      , it "negative first value" $ do
          let bs = BS.pack [0x80, 0x01, 0x04, 0x01, 0x09]
          decodeDeltaBinaryPackedInt32 1 bs `shouldBe` Right (VP.fromList [-5 :: Int32])
      , it "negative deltas" $ do
          let bs = BS.pack [0x80, 0x01, 0x04, 0x03, 0x14, 0x05, 0x00, 0x00, 0x00, 0x00]
          decodeDeltaBinaryPackedInt32 3 bs `shouldBe` Right (VP.fromList [10, 7, 4 :: Int32])
      , it "empty (total_values=0)" $ do
          let bs = BS.pack [0x80, 0x01, 0x04, 0x00, 0x00]
          decodeDeltaBinaryPackedInt32 0 bs `shouldBe` Right VP.empty
      , it "rejects the abbreviated, invalid 8-value block used in spec illustrations" $ do
          decodeDeltaBinaryPackedInt32 1 (BS.pack [0x08, 0x01, 0x01, 0x54]) `shouldSatisfy` isLeft
      ]


dataPageV2Tests :: Spec
dataPageV2Tests =
  describe "DATA_PAGE_V2" $
    sequence_
      [ it "header parse from Thrift bytes" $ do
          let v2Struct =
                TV.Struct $
                  V.fromList
                    [ (1, TV.I32 1000)
                    , (2, TV.I32 100)
                    , (3, TV.I32 1000)
                    , (4, TV.I32 0)
                    , (5, TV.I32 50)
                    , (6, TV.I32 25)
                    , (7, TV.Bool True)
                    ]
              pageHdrStruct =
                TV.Struct $
                  V.fromList
                    [(1, TV.I32 3), (2, TV.I32 200), (3, TV.I32 150), (8, v2Struct)]
              bs = encodeCompact pageHdrStruct
          case readPageHeaderAt bs 0 of
            Left e -> expectationFailure e
            Right (hdr, _) -> case phType hdr of
              PtDataPageV2 v2 -> do
                dph2NumValues v2 `shouldBe` 1000
                dph2NumNulls v2 `shouldBe` 100
                dph2NumRows v2 `shouldBe` 1000
                dph2Encoding v2 `shouldBe` 0
                dph2DefLevelsLen v2 `shouldBe` 50
                dph2RepLevelsLen v2 `shouldBe` 25
                dph2IsCompressed v2 `shouldBe` True
              _ -> expectationFailure "expected PtDataPageV2"
      , it "is_compressed defaults to True when absent" $ do
          let v2Struct =
                TV.Struct $
                  V.fromList
                    [ (1, TV.I32 5)
                    , (2, TV.I32 0)
                    , (3, TV.I32 5)
                    , (4, TV.I32 0)
                    , (5, TV.I32 10)
                    , (6, TV.I32 0)
                    ]
              bs =
                encodeCompact $
                  TV.Struct $
                    V.fromList
                      [(1, TV.I32 3), (2, TV.I32 40), (3, TV.I32 40), (8, v2Struct)]
          case readPageHeaderAt bs 0 of
            Left e -> expectationFailure e
            Right (hdr, _) -> case phType hdr of
              PtDataPageV2 v2 -> dph2IsCompressed v2 `shouldBe` True
              _ -> expectationFailure "expected PtDataPageV2"
      , it "page header round-trip through encode/parse" $ do
          let hdr =
                PageHeader
                  (PtDataPageV2 (DataPageHeaderV2 200 50 200 0 30 20 False))
                  (Just 500)
                  (Just 400)
              bs = encodePageHeader hdr
          case readPageHeaderAt bs 0 of
            Left e -> expectationFailure e
            Right (hdr', _) -> case phType hdr' of
              PtDataPageV2 v2 -> do
                dph2NumValues v2 `shouldBe` 200
                dph2NumNulls v2 `shouldBe` 50
                dph2IsCompressed v2 `shouldBe` False
              _ -> expectationFailure "round-trip lost PtDataPageV2"
      ]


writerRoundtripTests :: Spec
writerRoundtripTests =
  describe "Writer round-trips" $
    sequence_
      [ it "buildParquetFile -> loadParquetFile -> readPlainInt32ColumnChunk" $ do
          let schema =
                V.fromList
                  [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                  , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                  ]
              vals = VP.fromList [1, 2, 3, 4, 5 :: Int32]
              fileBytes = buildParquetFile schema (V.singleton (V.singleton (ColInt32 vals)))
          case loadParquetFile fileBytes of
            Left e -> expectationFailure e
            Right pf -> case columnChunkSlice pf 0 0 of
              Left e -> expectationFailure e
              Right chunkData ->
                readPlainInt32ColumnChunk Uncompressed chunkData `shouldBe` Right vals
      , it "multiple columns round-trip" $ do
          let schema =
                V.fromList
                  [ SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing Nothing Nothing
                  , SchemaElement "a" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                  , SchemaElement "b" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                  ]
              colA = VP.fromList [10, 20, 30 :: Int32]
              colB = VP.fromList [100, 200, 300 :: Int32]
              fileBytes =
                buildParquetFile
                  schema
                  (V.singleton (V.fromList [ColInt32 colA, ColInt32 colB]))
          case loadParquetFile fileBytes of
            Left e -> expectationFailure e
            Right pf -> do
              rA <- either expectationFailure pure (columnChunkSlice pf 0 0)
              rB <- either expectationFailure pure (columnChunkSlice pf 0 1)
              readPlainInt32ColumnChunk Uncompressed rA `shouldBe` Right colA
              readPlainInt32ColumnChunk Uncompressed rB `shouldBe` Right colB
      , it "empty column round-trip" $ do
          let schema =
                V.fromList
                  [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                  , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                  ]
              vals = VP.empty :: VP.Vector Int32
              fileBytes = buildParquetFile schema (V.singleton (V.singleton (ColInt32 vals)))
          case loadParquetFile fileBytes of
            Left e -> expectationFailure e
            Right pf -> case columnChunkSlice pf 0 0 of
              Left e -> expectationFailure e
              Right chunkData ->
                readPlainInt32ColumnChunk Uncompressed chunkData `shouldBe` Right vals
      , it "page header encode/decode round-trip" $ do
          let hdr =
                PageHeader
                  (PtDataPage (DataPageHeader 25 0))
                  (Just 100)
                  (Just 100)
              bs = encodePageHeader hdr
          case readPageHeaderAt bs 0 of
            Left e -> expectationFailure e
            Right (hdr', _) -> do
              phCompressedPageSize hdr' `shouldBe` Just 100
              case phType hdr' of
                PtDataPage dph -> do
                  dphNumValues dph `shouldBe` 25
                  dphEncoding dph `shouldBe` 0
                _ -> expectationFailure "expected PtDataPage"
      ]


pageIndexTests :: Spec
pageIndexTests =
  describe "Page index (OffsetIndex / ColumnIndex)" $
    sequence_
      [ it "OffsetIndex round-trip with page locations" $ do
          let oi =
                OffsetIndex
                  { oiPageLocations =
                      V.fromList
                        [ PageLocation 100 200 0
                        , PageLocation 300 250 50
                        , PageLocation 550 175 105
                        ]
                  , oiUnencodedByteArrayDataBytes = Nothing
                  }
          decodeOffsetIndex (encodeOffsetIndex oi) `shouldBe` Right oi
      , it "OffsetIndex round-trip with unencoded byte counts" $ do
          let oi =
                OffsetIndex
                  { oiPageLocations =
                      V.fromList
                        [PageLocation 0 50 0, PageLocation 50 60 25]
                  , oiUnencodedByteArrayDataBytes = Just (V.fromList [200, 240])
                  }
          decodeOffsetIndex (encodeOffsetIndex oi) `shouldBe` Right oi
      , it "ColumnIndex round-trip with min/max + null counts" $ do
          let ci =
                ColumnIndex
                  { ciNanCounts = Nothing
                  , ciNullPages = V.fromList [False, False, True]
                  , ciMinValues = V.fromList [BS.pack [0, 0, 0, 0], BS.pack [10, 0, 0, 0], BS.empty]
                  , ciMaxValues = V.fromList [BS.pack [9, 0, 0, 0], BS.pack [99, 0, 0, 0], BS.empty]
                  , ciBoundaryOrder = OrderAscending
                  , ciNullCounts = Just (V.fromList [0, 0, 100])
                  , ciRepetitionLevelHistograms = Nothing
                  , ciDefinitionLevelHistograms = Nothing
                  }
          decodeColumnIndex (encodeColumnIndex ci) `shouldBe` Right ci
      , it "ColumnIndex round-trip with level histograms" $ do
          let ci =
                ColumnIndex
                  { ciNanCounts = Nothing
                  , ciNullPages = V.fromList [False, False]
                  , ciMinValues = V.fromList [BS.pack [1], BS.pack [2]]
                  , ciMaxValues = V.fromList [BS.pack [3], BS.pack [4]]
                  , ciBoundaryOrder = OrderUnordered
                  , ciNullCounts = Just (V.fromList [1, 2])
                  , ciRepetitionLevelHistograms = Just (V.fromList [10, 5, 20, 7])
                  , ciDefinitionLevelHistograms = Just (V.fromList [3, 8, 9, 0])
                  }
          decodeColumnIndex (encodeColumnIndex ci) `shouldBe` Right ci
      , it "BoundaryOrder values match parquet.thrift" $ do
          boundaryOrderToInt OrderUnordered `shouldBe` 0
          boundaryOrderToInt OrderAscending `shouldBe` 1
          boundaryOrderToInt OrderDescending `shouldBe` 2
          intToBoundaryOrder 0 `shouldBe` Just OrderUnordered
          intToBoundaryOrder 1 `shouldBe` Just OrderAscending
          intToBoundaryOrder 2 `shouldBe` Just OrderDescending
          intToBoundaryOrder 3 `shouldBe` Nothing
      , it "OffsetIndex round-trip property" $ property $ do
          n <- forAll $ Gen.int (Range.linear 0 16)
          pls <-
            forAll $
              traverse
                ( \_ -> do
                    off <- Gen.int64 (Range.linear 0 100000)
                    sz <- Gen.int32 (Range.linear 1 10000)
                    fri <- Gen.int64 (Range.linear 0 1000000)
                    pure (PageLocation off sz fri)
                )
                [1 .. n]
          let oi = OffsetIndex (V.fromList pls) Nothing
          decodeOffsetIndex (encodeOffsetIndex oi) === Right oi
      , it "ColumnIndex round-trip property" $ property $ do
          n <- forAll $ Gen.int (Range.linear 0 8)
          bos <- forAll $ Gen.element [OrderUnordered, OrderAscending, OrderDescending]
          pages <-
            forAll $
              traverse
                ( \_ -> do
                    isNull <- Gen.bool
                    mn <- Gen.bytes (Range.linear 0 16)
                    mx <- Gen.bytes (Range.linear 0 16)
                    nc <- Gen.int64 (Range.linear 0 1000)
                    pure (isNull, mn, mx, nc)
                )
                [1 .. n]
          let nulls = V.fromList (map (\(a, _, _, _) -> a) pages)
              mins = V.fromList (map (\(_, b, _, _) -> b) pages)
              maxs = V.fromList (map (\(_, _, c, _) -> c) pages)
              ncs = V.fromList (map (\(_, _, _, d) -> d) pages)
              ci = ColumnIndex nulls mins maxs bos (Just ncs) Nothing Nothing Nothing
          decodeColumnIndex (encodeColumnIndex ci) === Right ci
      , it "ColumnChunk carries page-index and bloom offsets" $ do
          let cm =
                ColumnMetadata
                  PTInt32
                  (V.singleton Plain)
                  (V.singleton "x")
                  Uncompressed
                  100
                  1000
                  1000
                  4
                  Nothing
                  Nothing
                  (Just 5000)
                  (Just 256)
              cc =
                ColumnChunk
                  Nothing
                  4
                  (Just cm)
                  (Just 6000)
                  (Just 80)
                  (Just 6080)
                  (Just 200)
              rg = RowGroup (V.singleton cc) 1000 100 Nothing
              fm =
                FileMetadata
                  2
                  ( V.fromList
                      [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing Nothing
                      , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing Nothing
                      ]
                  )
                  100
                  (V.singleton rg)
                  Nothing
                  Nothing
                  Nothing
          readFooter (writeFooter fm) `shouldBe` Right fm
      ]


xxh64Tests :: Spec
xxh64Tests =
  describe "XXH64 (xxHash 0.1.1)" $
    sequence_
      -- Reference vectors from
      -- https://github.com/Cyan4973/xxHash/blob/dev/doc/xxhash_spec.md
      [ it "empty string" $
          hex (Hash.xxh64 0 (BSC.pack "")) `shouldBe` "ef46db3751d8e999"
      , it "abc" $
          hex (Hash.xxh64 0 (BSC.pack "abc")) `shouldBe` "44bc2cf5ad770999"
      , it "spammish repetition (long input)" $
          hex (Hash.xxh64 0 (BSC.pack "Nobody inspects the spammish repetition"))
            `shouldBe` "fbcea83c8a378bf1"
      , it "32-byte boundary input" $
          -- 32 bytes triggers exactly one stripe in the bulk phase.
          -- Reference via xxhsum -H1 / python xxhash on b'a' * 32:
          -- 856e843298f99ad7 (the previous expected hex was stale).
          hex (Hash.xxh64 0 (BS.replicate 32 0x61)) `shouldBe` "856e843298f99ad7"
      , it "different inputs produce different hashes (with high probability)" $
          property $ do
            a <- forAll $ Gen.bytes (Range.linear 1 64)
            b <- forAll $ Gen.bytes (Range.linear 1 64)
            if a == b then pure () else assert (Hash.xxh64 0 a /= Hash.xxh64 0 b)
      ]
  where
    hex w =
      let s = showHex w ""
      in replicate (16 - length s) '0' ++ s


bloomFilterTests :: Spec
bloomFilterTests =
  describe "BloomFilter (split-block, XXH64)" $
    sequence_
      [ it "newSbbf rounds up to block" $ do
          sbbfNumBytes (newSbbf 1) `shouldBe` 32
          sbbfNumBytes (newSbbf 32) `shouldBe` 32
          sbbfNumBytes (newSbbf 33) `shouldBe` 64
          sbbfNumBytes (newSbbf 64) `shouldBe` 64
      , it "every inserted value reports present" $ do
          let sbbf0 = newSbbf 1024
              values = ["alpha", "beta", "gamma", "delta", "epsilon"]
              sbbf = foldr (sbbfInsert . BSC.pack) sbbf0 values
          mapM_ (\v -> (if (sbbfCheck (BSC.pack v) sbbf) then pure () else expectationFailure (v ++ " missing"))) values
      , it "non-inserted values mostly absent at sensible FPP" $ do
          -- 1024 bytes / 256 distinct items at SBBF density ~ ~0.1% FPP.
          -- We use 2048 bytes to stay well under 1% over a 256-key probe set.
          let sbbf0 = newSbbf 2048
              inserted = map (BSC.pack . ("inserted-" <>) . show) [(0 :: Int) .. 255]
              probes = map (BSC.pack . ("probe-" <>) . show) [(0 :: Int) .. 255]
              sbbf = foldr sbbfInsert sbbf0 inserted
              fp = length (filter (`sbbfCheck` sbbf) probes)
          (if (fp <= 8) then pure () else expectationFailure ("too many false positives: " ++ show fp))
      , it "encode/decode round-trip preserves membership" $ do
          let sbbf0 = newSbbf 256
              xs = ["a", "ab", "abc", "abcd", "abcde", "abcdef"]
              sbbf = foldr (sbbfInsert . BSC.pack) sbbf0 xs
              bs = encodeBloomFilter sbbf
          case decodeBloomFilter bs of
            Left e -> expectationFailure e
            Right (_hdr, sbbf') -> do
              sbbfNumBytes sbbf' `shouldBe` sbbfNumBytes sbbf
              mapM_ (\x -> (if (sbbfCheck (BSC.pack x) sbbf') then pure () else expectationFailure ("after decode " ++ x ++ " missing"))) xs
      , it "optimalNumBytes returns block-aligned positive value" $ do
          let nb1 = optimalNumBytes 1000 0.01
              nb2 = optimalNumBytes 100000 0.001
          nb1 `mod` 32 `shouldBe` 0
          nb2 `mod` 32 `shouldBe` 0
          (nb1 >= 32 && nb2 >= 32) `shouldBe` True
          (nb1 < nb2) `shouldBe` True
      , it "round-trip preserves all inserted values" $
          property $ do
            nb <- forAll $ Gen.int (Range.linear 32 4096)
            xs <- forAll $ Gen.list (Range.linear 0 32) (Gen.bytes (Range.linear 0 32))
            let sbbf = foldr sbbfInsert (newSbbf nb) xs
                bs = encodeBloomFilter sbbf
            case decodeBloomFilter bs of
              Left e -> annotate e >> failure
              Right (_, sbbf') ->
                mapM_
                  ( \x ->
                      assert (sbbfCheck x sbbf')
                  )
                  xs
      ]
