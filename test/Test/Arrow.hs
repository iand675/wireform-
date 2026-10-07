module Test.Arrow (arrowTests) where

import Arrow.Column
import Arrow.File
import Arrow.FlatBufferIPC (materializeRecordBatchFB)
import Arrow.IPC
import Arrow.Types
import Arrow.Write
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()
import Wireform.Builder qualified as B


arrowTests :: Spec
arrowTests =
  describe
    "Arrow"
    $ sequence_
      [ schemaRoundtrips
      , recordBatchRoundtrips
      , edgeCases
      , wireFormatTests
      , propertyRoundtrips
      , columnTests
      , writeRoundtrips
      ]


schemaRoundtrips :: Spec
schemaRoundtrips =
  describe
    "Schema roundtrips"
    $ sequence_
      [ it "Empty schema" $ do
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields = V.empty
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      , it "Simple int field" $ do
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields =
                        V.singleton
                          Field
                            { fieldName = T.pack "id"
                            , fieldNullable = False
                            , fieldType = AInt 32 True
                            , fieldChildren = V.empty
                            , fieldDictionary = Nothing
                            , fieldMetadata = V.empty
                            }
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      , it "Multiple fields" $ do
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields =
                        V.fromList
                          [ Field (T.pack "name") True AUtf8 V.empty Nothing V.empty
                          , Field (T.pack "age") False (AInt 32 True) V.empty Nothing V.empty
                          , Field (T.pack "active") False ABool V.empty Nothing V.empty
                          , Field (T.pack "score") True (AFloatingPoint DoublePrecision) V.empty Nothing V.empty
                          ]
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      , it "Nested struct" $ do
          let childField = Field (T.pack "x") False (AInt 64 True) V.empty Nothing V.empty
              msg =
                SchemaMessage
                  Schema
                    { arrowFields =
                        V.singleton
                          Field
                            { fieldName = T.pack "point"
                            , fieldNullable = True
                            , fieldType = AStruct
                            , fieldChildren = V.singleton childField
                            , fieldDictionary = Nothing
                            , fieldMetadata = V.empty
                            }
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      , it "Big endian" $ do
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields =
                        V.singleton
                          Field
                            { fieldName = T.pack "val"
                            , fieldNullable = False
                            , fieldType = AInt 16 False
                            , fieldChildren = V.empty
                            , fieldDictionary = Nothing
                            , fieldMetadata = V.empty
                            }
                    , arrowEndianness = Big
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      , it "All basic types" $ do
          let types =
                [ ANull
                , AInt 8 True
                , AInt 16 False
                , AInt 32 True
                , AInt 64 True
                , AFloatingPoint Half
                , AFloatingPoint Single
                , AFloatingPoint DoublePrecision
                , ABinary
                , AUtf8
                , ABool
                , ADate DateDay
                , ADate DateMillisecond
                , ATime Second 32
                , ATime Nanosecond 64
                , AInterval YearMonth
                , AInterval DayTime
                , AList
                , AStruct
                , AFixedSizeBinary 16
                , AFixedSizeList 10
                , AMap False
                , AMap True
                , ADuration Millisecond
                , ALargeBinary
                , ALargeUtf8
                , ALargeList
                ]
              mkField (i, at) = Field (T.pack ("f" ++ show i)) False at V.empty Nothing V.empty
              msg =
                SchemaMessage
                  Schema
                    { arrowFields = V.fromList (map mkField (zip [(0 :: Int) ..] types))
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      , it "Timestamp with timezone" $ do
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields =
                        V.singleton
                          Field
                            { fieldName = T.pack "ts"
                            , fieldNullable = True
                            , fieldType = ATimestamp Nanosecond (Just (T.pack "UTC"))
                            , fieldChildren = V.empty
                            , fieldDictionary = Nothing
                            , fieldMetadata = V.empty
                            }
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      , it "Timestamp without timezone" $ do
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields =
                        V.singleton
                          Field
                            { fieldName = T.pack "ts"
                            , fieldNullable = False
                            , fieldType = ATimestamp Microsecond Nothing
                            , fieldChildren = V.empty
                            , fieldDictionary = Nothing
                            , fieldMetadata = V.empty
                            }
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      , it "Union type" $ do
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields =
                        V.singleton
                          Field
                            { fieldName = T.pack "u"
                            , fieldNullable = False
                            , fieldType = AUnion Dense (V.fromList [0, 1, 2])
                            , fieldChildren = V.empty
                            , fieldDictionary = Nothing
                            , fieldMetadata = V.empty
                            }
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      , it "Decimal type" $ do
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields =
                        V.singleton
                          Field
                            { fieldName = T.pack "amount"
                            , fieldNullable = True
                            , fieldType = ADecimal 10 2
                            , fieldChildren = V.empty
                            , fieldDictionary = Nothing
                            , fieldMetadata = V.empty
                            }
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      ]


recordBatchRoundtrips :: Spec
recordBatchRoundtrips =
  describe
    "RecordBatch roundtrips"
    $ sequence_
      [ it "Empty record batch" $ do
          let msg =
                RecordBatch
                  RecordBatchDef
                    { rbLength = 0
                    , rbNodes = V.empty
                    , rbBuffers = V.empty
                    , rbVariadicBufferCounts = V.empty
                    , rbBodyCompression = Nothing
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      , it "Record batch with nodes and buffers" $ do
          let msg =
                RecordBatch
                  RecordBatchDef
                    { rbLength = 1000
                    , rbNodes =
                        V.fromList
                          [ FieldNode 1000 50
                          , FieldNode 1000 0
                          ]
                    , rbBuffers =
                        V.fromList
                          [ Buffer 0 128
                          , Buffer 128 8000
                          , Buffer 8128 128
                          , Buffer 8256 8000
                          ]
                    , rbVariadicBufferCounts = V.empty
                    , rbBodyCompression = Nothing
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      ]


edgeCases :: Spec
edgeCases =
  describe
    "Edge cases"
    $ sequence_
      [ it "Decode empty input" $
          case decodeIPCMessage BS.empty of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on empty input"
      , it "DictionaryBatch roundtrip" $ do
          let msg = DictionaryBatch 3 True emptyBatchDef
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      , it "Field with empty name" $ do
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields =
                        V.singleton
                          Field
                            { fieldName = T.empty
                            , fieldNullable = False
                            , fieldType = ANull
                            , fieldChildren = V.empty
                            , fieldDictionary = Nothing
                            , fieldMetadata = V.empty
                            }
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) `shouldBe` Right msg
      ]


wireFormatTests :: Spec
wireFormatTests =
  describe
    "Wire format"
    $ sequence_
      [ it "Starts with continuation 0xFFFFFFFF" $ do
          let bs = encodeIPCMessage (DictionaryBatch 0 False emptyBatchDef)
          BS.index bs 0 `shouldBe` 0xFF
          BS.index bs 1 `shouldBe` 0xFF
          BS.index bs 2 `shouldBe` 0xFF
          BS.index bs 3 `shouldBe` 0xFF
      , it "Metadata is 8-byte aligned" $ do
          let bs = encodeIPCMessage (DictionaryBatch 0 False emptyBatchDef)
              metaLen =
                fromIntegral (BS.index bs 4)
                  + fromIntegral (BS.index bs 5) * 256
                  + fromIntegral (BS.index bs 6) * 65536
                  + fromIntegral (BS.index bs 7) * 16777216
                  :: Int
          (metaLen `mod` 8) `shouldBe` 0
      ]


propertyRoundtrips :: Spec
propertyRoundtrips =
  describe
    "Property roundtrips"
    $ sequence_
      [ it "Schema with random int fields" $ property $ do
          nFields <- forAll $ Gen.int (Range.linear 0 8)
          fields <-
            forAll $
              traverse
                ( \i -> do
                    let name = T.pack ("field_" ++ show i)
                    nullable <- Gen.bool
                    bitWidth <- Gen.element [8, 16, 32, 64]
                    signed <- Gen.bool
                    pure
                      Field
                        { fieldName = name
                        , fieldNullable = nullable
                        , fieldType = AInt bitWidth signed
                        , fieldChildren = V.empty
                        , fieldDictionary = Nothing
                        , fieldMetadata = V.empty
                        }
                )
                [0 .. nFields - 1]
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields = V.fromList fields
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) === Right msg
      , it "Schema with random type selection" $ property $ do
          fieldType <-
            forAll $
              Gen.element
                [ ANull
                , ABool
                , AUtf8
                , ABinary
                , AInt 32 True
                , AInt 64 True
                , AFloatingPoint Single
                , AFloatingPoint DoublePrecision
                , AStruct
                , AList
                ]
          name <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha
          nullable <- forAll Gen.bool
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields =
                        V.singleton
                          Field
                            { fieldName = name
                            , fieldNullable = nullable
                            , fieldType = fieldType
                            , fieldChildren = V.empty
                            , fieldDictionary = Nothing
                            , fieldMetadata = V.empty
                            }
                    , arrowEndianness = Little
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) === Right msg
      , it "Schema with random endianness" $ property $ do
          endian <- forAll $ Gen.element [Little, Big]
          let msg =
                SchemaMessage
                  Schema
                    { arrowFields = V.empty
                    , arrowEndianness = endian
                    , arrowMetadata = V.empty
                    , arrowFeatures = V.empty
                    }
          decodeIPCMessage (encodeIPCMessage msg) === Right msg
      , it "RecordBatch with random dimensions" $ property $ do
          len <- forAll $ Gen.int64 (Range.linear 0 100000)
          nNodes <- forAll $ Gen.int (Range.linear 0 5)
          nodes <-
            forAll $
              traverse
                ( \_ -> do
                    nodeLen <- Gen.int64 (Range.linear 0 100000)
                    nullCount <- Gen.int64 (Range.linear 0 1000)
                    pure (FieldNode nodeLen nullCount)
                )
                [1 .. nNodes]
          nBufs <- forAll $ Gen.int (Range.linear 0 8)
          bufs <-
            forAll $
              traverse
                ( \_ -> do
                    bOff <- Gen.int64 (Range.linear 0 100000)
                    bLen <- Gen.int64 (Range.linear 0 100000)
                    pure (Buffer bOff bLen)
                )
                [1 .. nBufs]
          let msg =
                RecordBatch
                  RecordBatchDef
                    { rbLength = len
                    , rbNodes = V.fromList nodes
                    , rbBuffers = V.fromList bufs
                    , rbVariadicBufferCounts = V.empty
                    , rbBodyCompression = Nothing
                    }
          decodeIPCMessage (encodeIPCMessage msg) === Right msg
      ]


columnTests :: Spec
columnTests =
  describe
    "Column materialization"
    $ sequence_
      [ it "Nullable Int32" $ do
          let schema =
                Schema
                  { arrowFields =
                      V.singleton
                        Field
                          { fieldName = "x"
                          , fieldNullable = True
                          , fieldType = AInt 32 True
                          , fieldChildren = V.empty
                          , fieldDictionary = Nothing
                          , fieldMetadata = V.empty
                          }
                  , arrowEndianness = Little
                  , arrowMetadata = V.empty
                  , arrowFeatures = V.empty
                  }
              rb =
                RecordBatchDef
                  { rbLength = 3
                  , rbNodes = V.singleton (FieldNode 3 1)
                  , rbBuffers = V.fromList [Buffer 0 1, Buffer 8 12]
                  , rbVariadicBufferCounts = V.empty
                  , rbBodyCompression = Nothing
                  }
              validityByte = BS.pack [0x05]
              body =
                validityByte
                  <> BS.replicate 7 0
                  <> leI32 10
                  <> leI32 0
                  <> leI32 30
          case materializeFlatRecordBatch schema rb body of
            Left e -> expectationFailure e
            Right cols -> V.head cols `shouldBe` ColInt32Maybe (V.fromList [Just 10, Nothing, Just 30])
      , it "Struct column" $ do
          let childX = Field "x" False (AInt 32 True) V.empty Nothing V.empty
              childY = Field "y" False (AInt 32 True) V.empty Nothing V.empty
              schema =
                Schema
                  { arrowFields =
                      V.singleton
                        Field
                          { fieldName = "point"
                          , fieldNullable = False
                          , fieldType = AStruct
                          , fieldChildren = V.fromList [childX, childY]
                          , fieldDictionary = Nothing
                          , fieldMetadata = V.empty
                          }
                  , arrowEndianness = Little
                  , arrowMetadata = V.empty
                  , arrowFeatures = V.empty
                  }
              rb =
                RecordBatchDef
                  { rbLength = 2
                  , rbNodes = V.fromList [FieldNode 2 0, FieldNode 2 0, FieldNode 2 0]
                  , rbBuffers = V.fromList [Buffer 0 8, Buffer 8 8]
                  , rbVariadicBufferCounts = V.empty
                  , rbBodyCompression = Nothing
                  }
              body = leI32 1 <> leI32 2 <> leI32 3 <> leI32 4
          case materializeRecordBatch schema rb body of
            Left e -> expectationFailure e
            Right cols ->
              V.head cols
                `shouldBe` ColStruct
                  ( V.fromList
                      [ ("x", ColInt32 (VP.fromList [1, 2]))
                      , ("y", ColInt32 (VP.fromList [3, 4]))
                      ]
                  )
      , it "List column" $ do
          let childField = Field "item" False (AInt 32 True) V.empty Nothing V.empty
              schema =
                Schema
                  { arrowFields =
                      V.singleton
                        Field
                          { fieldName = "vals"
                          , fieldNullable = False
                          , fieldType = AList
                          , fieldChildren = V.singleton childField
                          , fieldDictionary = Nothing
                          , fieldMetadata = V.empty
                          }
                  , arrowEndianness = Little
                  , arrowMetadata = V.empty
                  , arrowFeatures = V.empty
                  }
              rb =
                RecordBatchDef
                  { rbLength = 2
                  , rbNodes = V.fromList [FieldNode 2 0, FieldNode 5 0]
                  , rbBuffers = V.fromList [Buffer 0 12, Buffer 16 20]
                  , rbVariadicBufferCounts = V.empty
                  , rbBodyCompression = Nothing
                  }
              offsets = leI32 0 <> leI32 2 <> leI32 5
              body =
                offsets
                  <> BS.replicate 4 0
                  <> leI32 1
                  <> leI32 2
                  <> leI32 3
                  <> leI32 4
                  <> leI32 5
          case materializeRecordBatch schema rb body of
            Left e -> expectationFailure e
            Right cols ->
              V.head cols
                `shouldBe` ColList
                  (VP.fromList [0, 2, 5])
                  (ColInt32 (VP.fromList [1, 2, 3, 4, 5]))
      ]


writeRoundtrips :: Spec
writeRoundtrips =
  describe
    "Write round-trips"
    $ sequence_
      [ it "Int32 encode-decode" $ do
          let schema =
                Schema
                  { arrowFields =
                      V.singleton
                        Field
                          { fieldName = "x"
                          , fieldNullable = False
                          , fieldType = AInt 32 True
                          , fieldChildren = V.empty
                          , fieldDictionary = Nothing
                          , fieldMetadata = V.empty
                          }
                  , arrowEndianness = Little
                  , arrowMetadata = V.empty
                  , arrowFeatures = V.empty
                  }
              vals = VP.fromList [1, 2, 3, 4, 5] :: VP.Vector Int32
              cols = V.singleton (ColInt32 vals)
              streamBs = writeArrowStream schema (V.singleton cols)
          case readIPCMessage streamBs 0 of
            Left e -> expectationFailure e
            Right (SchemaMessage s, _, next) -> do
              s `shouldBe` schema
              case readIPCMessage streamBs next of
                Left e -> expectationFailure e
                Right (RecordBatch rb, body, _) ->
                  case materializeRecordBatchFB schema rb body of
                    Left e2 -> expectationFailure e2
                    Right result -> V.head result `shouldBe` ColInt32 vals
                Right _ -> expectationFailure "Expected RecordBatch message"
            Right _ -> expectationFailure "Expected Schema message"
      , it "Arrow stream round-trip" $ do
          let schema =
                Schema
                  { arrowFields =
                      V.fromList
                        [ Field "a" False (AInt 32 True) V.empty Nothing V.empty
                        , Field "b" False ABool V.empty Nothing V.empty
                        ]
                  , arrowEndianness = Little
                  , arrowMetadata = V.empty
                  , arrowFeatures = V.empty
                  }
              batch1 =
                V.fromList
                  [ ColInt32 (VP.fromList [10, 20, 30])
                  , ColBool (V.fromList [True, False, True])
                  ]
              streamBs = writeArrowStream schema (V.singleton batch1)
          case readArrowStream streamBs of
            Left e -> expectationFailure e
            Right as -> do
              asSchema as `shouldBe` schema
              V.length (asBatches as) `shouldBe` 1
              let (rb, body) = V.head (asBatches as)
              case materializeRecordBatchFB schema rb body of
                Left e2 -> expectationFailure e2
                Right result -> do
                  result V.! 0 `shouldBe` ColInt32 (VP.fromList [10, 20, 30])
                  result V.! 1 `shouldBe` ColBool (V.fromList [True, False, True])
      , it "Arrow file round-trip" $ do
          let schema =
                Schema
                  { arrowFields =
                      V.singleton
                        Field
                          { fieldName = "x"
                          , fieldNullable = False
                          , fieldType = AInt 32 True
                          , fieldChildren = V.empty
                          , fieldDictionary = Nothing
                          , fieldMetadata = V.empty
                          }
                  , arrowEndianness = Little
                  , arrowMetadata = V.empty
                  , arrowFeatures = V.empty
                  }
              batch1 = V.singleton (ColInt32 (VP.fromList [100, 200, 300]))
              fileBs = writeArrowFile schema (V.singleton batch1)
          case readArrowFileColumns fileBs of
            Left e -> expectationFailure e
            Right (readSchema, readBatches) -> do
              readSchema `shouldBe` schema
              V.length readBatches `shouldBe` 1
              V.head (V.head readBatches) `shouldBe` ColInt32 (VP.fromList [100, 200, 300])
      ]


leI32 :: Int32 -> BS.ByteString
leI32 = BL.toStrict . B.toLazyByteString . B.int32LE


emptyBatchDef :: RecordBatchDef
emptyBatchDef = RecordBatchDef 0 V.empty V.empty V.empty Nothing
