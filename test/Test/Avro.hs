module Test.Avro (avroTests) where

import Avro.Container (readContainer)
import Avro.Decode (decodeAvro, decodeAvroAt)
import Avro.Encode (encodeAvro)
import Avro.Fingerprint (avroFingerprint, avroFingerprintMD5, parsingCanonicalForm)
import Avro.JSON (avroFromJSON, avroSchemaFromJSON, avroSchemaToJSON, avroToJSON)
import Avro.Protocol
import Avro.Resolution (FieldResolution (..), ResolvedSchema (..), resolveSchema, resolveValue)
import Avro.Schema
import Avro.Value qualified as AV
import Avro.Wire (avroEncodeInt, avroEncodeLong)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()
import Wireform.Builder qualified as B


avroTests :: Spec
avroTests =
  describe
    "Avro Encode/Decode"
    $ sequence_
      [ fingerprintTests
      , aliasResolutionTests
      , protocolTests
      , zigzagComplianceTests
      , byteEncodingComplianceTests
      , nullUnionComplianceTests
      , arrayEncodingComplianceTests
      , mapEncodingComplianceTests
      , sizedBlockTests
      , deconflictComplianceTests
      , propertyRoundtripComplianceTests
      , avroSpecConformanceVectors
      , describe
          "Primitive roundtrips (property)"
          $ sequence_
            [ it "null roundtrip" $ property $ do
                let ty = AvroPrimitive AvroNull
                roundtrip ty AV.Null
            , it "bool roundtrip" $ property $ do
                b <- forAll Gen.bool
                roundtrip (AvroPrimitive AvroBool) (AV.Bool b)
            , it "int roundtrip" $ property $ do
                n <- forAll $ Gen.int32 Range.linearBounded
                roundtrip (AvroPrimitive AvroInt) (AV.Int n)
            , it "long roundtrip" $ property $ do
                n <- forAll $ Gen.int64 Range.linearBounded
                roundtrip (AvroPrimitive AvroLong) (AV.Long n)
            , it "float roundtrip" $ property $ do
                f <- forAll $ Gen.float (Range.linearFrac (-1e6) 1e6)
                roundtrip (AvroPrimitive AvroFloat) (AV.Float f)
            , it "double roundtrip" $ property $ do
                d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
                roundtrip (AvroPrimitive AvroDouble) (AV.Double d)
            , it "bytes roundtrip" $ property $ do
                bs <- forAll $ Gen.bytes (Range.linear 0 256)
                roundtrip (AvroPrimitive AvroBytes) (AV.Bytes bs)
            , it "string roundtrip" $ property $ do
                t <- forAll $ Gen.text (Range.linear 0 128) Gen.unicode
                roundtrip (AvroPrimitive AvroString) (AV.String t)
            ]
      , describe
          "Edge cases (unit)"
          $ sequence_
            [ it "null encodes to empty" $ do
                let bs = encodeAvro (AvroPrimitive AvroNull) AV.Null
                bs `shouldBe` BS.empty
            , it "empty string" $ do
                let ty = AvroPrimitive AvroString
                    val = AV.String ""
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            , it "empty bytes" $ do
                let ty = AvroPrimitive AvroBytes
                    val = AV.Bytes ""
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            , it "empty array" $ do
                let ty = AvroArray (AvroPrimitive AvroInt)
                    val = AV.Array V.empty
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            , it "empty map" $ do
                let ty = AvroMap (AvroPrimitive AvroString)
                    val = AV.Map V.empty
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            , it "union index 0 (null)" $ do
                let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
                    val = AV.Union 0 AV.Null
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            , it "union index 1 (string)" $ do
                let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
                    val = AV.Union 1 (AV.String "hello")
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            , it "bool true" $ do
                let ty = AvroPrimitive AvroBool
                decodeAvro ty (encodeAvro ty (AV.Bool True)) `shouldBe` Right (AV.Bool True)
            , it "bool false" $ do
                let ty = AvroPrimitive AvroBool
                decodeAvro ty (encodeAvro ty (AV.Bool False)) `shouldBe` Right (AV.Bool False)
            , it "int 0" $ do
                let ty = AvroPrimitive AvroInt
                decodeAvro ty (encodeAvro ty (AV.Int 0)) `shouldBe` Right (AV.Int 0)
            , it "int min" $ do
                let ty = AvroPrimitive AvroInt
                decodeAvro ty (encodeAvro ty (AV.Int minBound)) `shouldBe` Right (AV.Int minBound)
            , it "int max" $ do
                let ty = AvroPrimitive AvroInt
                decodeAvro ty (encodeAvro ty (AV.Int maxBound)) `shouldBe` Right (AV.Int maxBound)
            , it "long min" $ do
                let ty = AvroPrimitive AvroLong
                decodeAvro ty (encodeAvro ty (AV.Long minBound)) `shouldBe` Right (AV.Long minBound)
            , it "long max" $ do
                let ty = AvroPrimitive AvroLong
                decodeAvro ty (encodeAvro ty (AV.Long maxBound)) `shouldBe` Right (AV.Long maxBound)
            ]
      , describe
          "Record roundtrip"
          $ sequence_
            [ it "mixed-field record" $ property $ do
                n <- forAll $ Gen.int32 Range.linearBounded
                t <- forAll $ Gen.text (Range.linear 0 64) Gen.unicode
                b <- forAll Gen.bool
                d <- forAll $ Gen.double (Range.linearFrac (-1e6) 1e6)
                let ty =
                      mkRecordType
                        "TestRecord"
                        [ ("intField", AvroPrimitive AvroInt)
                        , ("stringField", AvroPrimitive AvroString)
                        , ("boolField", AvroPrimitive AvroBool)
                        , ("doubleField", AvroPrimitive AvroDouble)
                        ]
                    val = AV.Record (V.fromList [AV.Int n, AV.String t, AV.Bool b, AV.Double d])
                roundtrip ty val
            ]
      , describe
          "Array roundtrip"
          $ sequence_
            [ it "array of ints" $ property $ do
                ns <- forAll $ Gen.list (Range.linear 0 50) (Gen.int32 Range.linearBounded)
                let ty = AvroArray (AvroPrimitive AvroInt)
                    val = AV.Array (V.fromList (map AV.Int ns))
                roundtrip ty val
            , it "array of records" $ property $ do
                items <- forAll $ Gen.list (Range.linear 0 20) $ do
                  i <- Gen.int32 Range.linearBounded
                  s <- Gen.text (Range.linear 0 32) Gen.unicode
                  pure (i, s)
                let recTy =
                      mkRecordType
                        "Item"
                        [ ("id", AvroPrimitive AvroInt)
                        , ("name", AvroPrimitive AvroString)
                        ]
                    ty = AvroArray recTy
                    val = AV.Array (V.fromList [AV.Record (V.fromList [AV.Int i, AV.String s]) | (i, s) <- items])
                roundtrip ty val
            ]
      , describe
          "Map roundtrip"
          $ sequence_
            [ it "map of longs" $ property $ do
                entries <- forAll $ Gen.list (Range.linear 0 30) $ do
                  k <- Gen.text (Range.linear 1 32) Gen.alphaNum
                  v <- Gen.int64 Range.linearBounded
                  pure (k, v)
                let ty = AvroMap (AvroPrimitive AvroLong)
                    val = AV.Map (V.fromList [(k, AV.Long v) | (k, v) <- entries])
                roundtrip ty val
            ]
      , describe
          "Union roundtrip"
          $ sequence_
            [ it "null|string union" $ property $ do
                useNull <- forAll Gen.bool
                let branches = V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString]
                    ty = AvroUnion branches
                val <-
                  if useNull
                    then pure (AV.Union 0 AV.Null)
                    else do
                      t <- forAll $ Gen.text (Range.linear 0 64) Gen.unicode
                      pure (AV.Union 1 (AV.String t))
                roundtrip ty val
            , it "int|long|string union" $ property $ do
                branch <- forAll $ Gen.int (Range.linear 0 2)
                let branches =
                      V.fromList
                        [ AvroPrimitive AvroInt
                        , AvroPrimitive AvroLong
                        , AvroPrimitive AvroString
                        ]
                    ty = AvroUnion branches
                val <- case branch of
                  0 -> AV.Union 0 . AV.Int <$> forAll (Gen.int32 Range.linearBounded)
                  1 -> AV.Union 1 . AV.Long <$> forAll (Gen.int64 Range.linearBounded)
                  _ -> AV.Union 2 . AV.String <$> forAll (Gen.text (Range.linear 0 32) Gen.unicode)
                roundtrip ty val
            ]
      , describe
          "Fixed roundtrip"
          $ sequence_
            [ it "fixed bytes" $ property $ do
                sz <- forAll $ Gen.int (Range.linear 0 64)
                bs <- forAll $ Gen.bytes (Range.singleton sz)
                let ty = AvroFixed "TestFixed" Nothing sz V.empty
                    val = AV.Fixed bs
                roundtrip ty val
            , it "fixed empty" $ do
                let ty = AvroFixed "Empty" Nothing 0 V.empty
                    val = AV.Fixed ""
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            ]
      , describe
          "Enum roundtrip"
          $ sequence_
            [ it "enum ordinal" $ property $ do
                idx <- forAll $ Gen.int (Range.linear 0 9)
                let syms = V.fromList (map (T.pack . ("S" ++) . show) [0 .. 9 :: Int])
                    ty = AvroEnum "TestEnum" Nothing Nothing V.empty syms Nothing
                    val = AV.Enum idx
                roundtrip ty val
            ]
      , describe
          "JSON roundtrip — primitives"
          $ sequence_
            [ it "null JSON roundtrip" $ do
                let ty = AvroPrimitive AvroNull
                    val = AV.Null
                avroFromJSON ty (avroToJSON ty val) `shouldBe` Right val
            , it "bool JSON roundtrip" $ do
                let ty = AvroPrimitive AvroBool
                avroFromJSON ty (avroToJSON ty (AV.Bool True)) `shouldBe` Right (AV.Bool True)
                avroFromJSON ty (avroToJSON ty (AV.Bool False)) `shouldBe` Right (AV.Bool False)
            , it "int JSON roundtrip" $ do
                let ty = AvroPrimitive AvroInt
                    val = AV.Int 42
                avroFromJSON ty (avroToJSON ty val) `shouldBe` Right val
            , it "long JSON roundtrip" $ do
                let ty = AvroPrimitive AvroLong
                    val = AV.Long 123456789
                avroFromJSON ty (avroToJSON ty val) `shouldBe` Right val
            , it "float JSON roundtrip" $ do
                let ty = AvroPrimitive AvroFloat
                    val = AV.Float 3.14
                    Right (AV.Float result) = avroFromJSON ty (avroToJSON ty val)
                abs (result - 3.14) < 0.001 `shouldBe` True
            , it "double JSON roundtrip" $ do
                let ty = AvroPrimitive AvroDouble
                    val = AV.Double 2.71828
                avroFromJSON ty (avroToJSON ty val) `shouldBe` Right val
            , it "bytes JSON roundtrip" $ do
                let ty = AvroPrimitive AvroBytes
                    val = AV.Bytes (BS.pack [0, 1, 127, 255])
                avroFromJSON ty (avroToJSON ty val) `shouldBe` Right val
            , it "string JSON roundtrip" $ do
                let ty = AvroPrimitive AvroString
                    val = AV.String "hello world"
                avroFromJSON ty (avroToJSON ty val) `shouldBe` Right val
            , it "float NaN JSON" $ do
                let ty = AvroPrimitive AvroFloat
                avroToJSON ty (AV.Float (0 / 0)) `shouldBe` Aeson.String "NaN"
            , it "double Infinity JSON" $ do
                let ty = AvroPrimitive AvroDouble
                avroToJSON ty (AV.Double (1 / 0)) `shouldBe` Aeson.String "Infinity"
                avroToJSON ty (AV.Double (negate (1 / 0))) `shouldBe` Aeson.String "-Infinity"
            ]
      , describe
          "JSON record encode/decode"
          $ sequence_
            [ it "simple record" $ do
                let ty =
                      mkRecordType
                        "Person"
                        [ ("name", AvroPrimitive AvroString)
                        , ("age", AvroPrimitive AvroInt)
                        ]
                    val = AV.Record (V.fromList [AV.String "Alice", AV.Int 30])
                    json = avroToJSON ty val
                avroFromJSON ty json `shouldBe` Right val
            , it "record JSON structure" $ do
                let ty =
                      mkRecordType
                        "Pair"
                        [ ("first", AvroPrimitive AvroInt)
                        , ("second", AvroPrimitive AvroString)
                        ]
                    val = AV.Record (V.fromList [AV.Int 1, AV.String "x"])
                    json = avroToJSON ty val
                case json of
                  Aeson.Object _ -> pure ()
                  _ -> expectationFailure "expected JSON object"
                avroFromJSON ty json `shouldBe` Right val
            ]
      , describe
          "JSON union encode/decode"
          $ sequence_
            [ it "null branch in union" $ do
                let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
                    val = AV.Union 0 AV.Null
                avroToJSON ty val `shouldBe` Aeson.Null
                avroFromJSON ty Aeson.Null `shouldBe` Right val
            , it "string branch in union" $ do
                let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
                    val = AV.Union 1 (AV.String "hello")
                    json = avroToJSON ty val
                avroFromJSON ty json `shouldBe` Right val
            ]
      , describe
          "Schema to/from JSON roundtrip"
          $ sequence_
            [ it "primitive schemas" $ do
                let prims =
                      [ AvroPrimitive AvroNull
                      , AvroPrimitive AvroBool
                      , AvroPrimitive AvroInt
                      , AvroPrimitive AvroLong
                      , AvroPrimitive AvroFloat
                      , AvroPrimitive AvroDouble
                      , AvroPrimitive AvroBytes
                      , AvroPrimitive AvroString
                      ]
                mapM_ (\ty -> avroSchemaFromJSON (avroSchemaToJSON ty) `shouldBe` Right ty) prims
            , it "record schema roundtrip" $ do
                let ty =
                      mkRecordType
                        "MyRec"
                        [ ("x", AvroPrimitive AvroInt)
                        , ("y", AvroPrimitive AvroString)
                        ]
                    Right parsed = avroSchemaFromJSON (avroSchemaToJSON ty)
                avroRecordName parsed `shouldBe` "MyRec"
                V.length (avroRecordFields parsed) `shouldBe` 2
            , it "enum schema roundtrip" $ do
                let ty =
                      AvroEnum
                        "Color"
                        Nothing
                        Nothing
                        V.empty
                        (V.fromList ["RED", "GREEN", "BLUE"])
                        Nothing
                    Right parsed = avroSchemaFromJSON (avroSchemaToJSON ty)
                avroEnumName parsed `shouldBe` "Color"
                avroEnumSymbols parsed `shouldBe` V.fromList ["RED", "GREEN", "BLUE"]
            , it "array schema roundtrip" $ do
                let ty = AvroArray (AvroPrimitive AvroInt)
                avroSchemaFromJSON (avroSchemaToJSON ty) `shouldBe` Right ty
            , it "map schema roundtrip" $ do
                let ty = AvroMap (AvroPrimitive AvroString)
                avroSchemaFromJSON (avroSchemaToJSON ty) `shouldBe` Right ty
            , it "union schema roundtrip" $ do
                let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroInt])
                avroSchemaFromJSON (avroSchemaToJSON ty) `shouldBe` Right ty
            , it "fixed schema roundtrip" $ do
                let ty = AvroFixed "Hash" Nothing 16 V.empty
                    Right parsed = avroSchemaFromJSON (avroSchemaToJSON ty)
                avroFixedName parsed `shouldBe` "Hash"
                avroFixedSize parsed `shouldBe` 16
            ]
      , describe
          "Resolution — compatible schemas"
          $ sequence_
            [ it "add field with default" $ do
                let writerTy =
                      mkRecordType
                        "Rec"
                        [("a", AvroPrimitive AvroInt)]
                    readerTy =
                      mkRecordTypeWithDefaults
                        "Rec"
                        [ ("a", AvroPrimitive AvroInt, Nothing)
                        , ("b", AvroPrimitive AvroString, Just AvroString)
                        ]
                    Right res = resolveSchema writerTy readerTy
                    writerVal = AV.Record (V.fromList [AV.Int 42])
                resolveValue res writerVal `shouldBe` Right (AV.Record (V.fromList [AV.Int 42, AV.String ""]))
            , it "remove field" $ do
                let writerTy =
                      mkRecordType
                        "Rec"
                        [ ("a", AvroPrimitive AvroInt)
                        , ("b", AvroPrimitive AvroString)
                        ]
                    readerTy =
                      mkRecordType
                        "Rec"
                        [("a", AvroPrimitive AvroInt)]
                    Right res = resolveSchema writerTy readerTy
                    writerVal = AV.Record (V.fromList [AV.Int 99, AV.String "dropped"])
                resolveValue res writerVal `shouldBe` Right (AV.Record (V.fromList [AV.Int 99]))
            , it "promote int -> long" $ do
                let writerTy = AvroPrimitive AvroInt
                    readerTy = AvroPrimitive AvroLong
                    Right res = resolveSchema writerTy readerTy
                resolveValue res (AV.Int 42) `shouldBe` Right (AV.Long 42)
            , it "promote int -> float" $ do
                let writerTy = AvroPrimitive AvroInt
                    readerTy = AvroPrimitive AvroFloat
                    Right res = resolveSchema writerTy readerTy
                resolveValue res (AV.Int 7) `shouldBe` Right (AV.Float 7.0)
            , it "promote int -> double" $ do
                let writerTy = AvroPrimitive AvroInt
                    readerTy = AvroPrimitive AvroDouble
                    Right res = resolveSchema writerTy readerTy
                resolveValue res (AV.Int 7) `shouldBe` Right (AV.Double 7.0)
            , it "promote long -> float" $ do
                let writerTy = AvroPrimitive AvroLong
                    readerTy = AvroPrimitive AvroFloat
                    Right res = resolveSchema writerTy readerTy
                resolveValue res (AV.Long 100) `shouldBe` Right (AV.Float 100.0)
            , it "promote long -> double" $ do
                let writerTy = AvroPrimitive AvroLong
                    readerTy = AvroPrimitive AvroDouble
                    Right res = resolveSchema writerTy readerTy
                resolveValue res (AV.Long 100) `shouldBe` Right (AV.Double 100.0)
            , it "promote float -> double" $ do
                let writerTy = AvroPrimitive AvroFloat
                    readerTy = AvroPrimitive AvroDouble
                    Right res = resolveSchema writerTy readerTy
                    Right (AV.Double d) = resolveValue res (AV.Float 1.5)
                abs (d - 1.5) < 0.001 `shouldBe` True
            , it "same schema resolves trivially" $ do
                let ty = AvroPrimitive AvroInt
                resolveSchema ty ty `shouldBe` Right ResolvedSame
            , it "array resolution" $ do
                let writerTy = AvroArray (AvroPrimitive AvroInt)
                    readerTy = AvroArray (AvroPrimitive AvroLong)
                    Right res = resolveSchema writerTy readerTy
                resolveValue res (AV.Array (V.fromList [AV.Int 1, AV.Int 2]))
                  `shouldBe` Right (AV.Array (V.fromList [AV.Long 1, AV.Long 2]))
            , it "map resolution" $ do
                let writerTy = AvroMap (AvroPrimitive AvroInt)
                    readerTy = AvroMap (AvroPrimitive AvroLong)
                    Right res = resolveSchema writerTy readerTy
                resolveValue res (AV.Map (V.fromList [("k", AV.Int 5)]))
                  `shouldBe` Right (AV.Map (V.fromList [("k", AV.Long 5)]))
            ]
      , describe
          "Resolution — incompatible schemas"
          $ sequence_
            [ it "type mismatch int vs string" $ do
                let res = resolveSchema (AvroPrimitive AvroInt) (AvroPrimitive AvroString)
                case res of
                  Left _ -> pure ()
                  Right _ -> expectationFailure "expected incompatibility error"
            , it "missing required field" $ do
                let writerTy =
                      mkRecordType
                        "Rec"
                        [("a", AvroPrimitive AvroInt)]
                    readerTy =
                      mkRecordType
                        "Rec"
                        [ ("a", AvroPrimitive AvroInt)
                        , ("b", AvroPrimitive AvroString)
                        ]
                case resolveSchema writerTy readerTy of
                  Left _ -> pure ()
                  Right _ -> expectationFailure "expected missing field error"
            , it "fixed size mismatch" $ do
                let w = AvroFixed "Hash" Nothing 16 V.empty
                    r = AvroFixed "Hash" Nothing 32 V.empty
                case resolveSchema w r of
                  Left _ -> pure ()
                  Right _ -> expectationFailure "expected fixed size mismatch error"
            , it "enum symbol mismatch" $ do
                let w = AvroEnum "E" Nothing Nothing V.empty (V.fromList ["A", "B", "C"]) Nothing
                    r = AvroEnum "E" Nothing Nothing V.empty (V.fromList ["A", "B"]) Nothing
                case resolveSchema w r of
                  Left _ -> pure ()
                  Right _ -> expectationFailure "expected enum symbol mismatch"
            , it "string vs int incompatible" $ do
                let res = resolveSchema (AvroPrimitive AvroString) (AvroPrimitive AvroInt)
                case res of
                  Left _ -> pure ()
                  Right _ -> expectationFailure "expected incompatibility error"
            ]
      ]


--------------------------------------------------------------------------------
-- Avro Protocol tests
--------------------------------------------------------------------------------

protocolTests :: Spec
protocolTests =
  describe
    "Avro Protocol"
    $ sequence_
      [ protocolJsonRoundtrip
      , protocolFingerprintTest
      , handshakeTests
      , minimalProtocol
      ]


protocolJsonRoundtrip :: Spec
protocolJsonRoundtrip = it "Protocol JSON roundtrip" $ do
  let proto =
        AvroProtocol
          { protoName = "HelloService"
          , protoNamespace = Just "com.example"
          , protoDoc = Just "A simple service"
          , protoTypes =
              [ mkRecordType
                  "Greeting"
                  [("message", AvroPrimitive AvroString)]
              , AvroEnum
                  { avroEnumName = "Tone"
                  , avroEnumNamespace = Nothing
                  , avroEnumDoc = Nothing
                  , avroEnumAliases = V.empty
                  , avroEnumSymbols = V.fromList ["FRIENDLY", "FORMAL"]
                  , avroEnumDefault = Nothing
                  }
              ]
          , protoMessages =
              [
                ( "hello"
                , AvroMessage
                    { msgRequest =
                        [ AvroParam "name" (AvroPrimitive AvroString)
                        , AvroParam "tone" (AvroPrimitive (AvroSchemaRef "Tone"))
                        ]
                    , msgResponse = AvroPrimitive (AvroSchemaRef "Greeting")
                    , msgErrors = Nothing
                    , msgOneWay = False
                    }
                )
              ,
                ( "ping"
                , AvroMessage
                    { msgRequest = []
                    , msgResponse = AvroPrimitive AvroNull
                    , msgErrors = Nothing
                    , msgOneWay = True
                    }
                )
              ]
          }
      json = protocolToJSON proto
  case protocolFromJSON json of
    Left err -> expectationFailure $ "Failed to parse protocol JSON: " ++ err
    Right parsed -> do
      protoName parsed `shouldBe` "HelloService"
      protoNamespace parsed `shouldBe` Just "com.example"
      protoDoc parsed `shouldBe` Just "A simple service"
      length (protoTypes parsed) `shouldBe` 2
      length (protoMessages parsed) `shouldBe` 2


protocolFingerprintTest :: Spec
protocolFingerprintTest = it "Protocol fingerprint is 16 bytes (MD5)" $ do
  let proto =
        AvroProtocol
          { protoName = "TestService"
          , protoNamespace = Nothing
          , protoDoc = Nothing
          , protoTypes = []
          , protoMessages =
              [
                ( "echo"
                , AvroMessage
                    { msgRequest = [AvroParam "msg" (AvroPrimitive AvroString)]
                    , msgResponse = AvroPrimitive AvroString
                    , msgErrors = Nothing
                    , msgOneWay = False
                    }
                )
              ]
          }
      fp = avroProtocolFingerprint proto
  BS.length fp `shouldBe` 16
  let fp2 = avroProtocolFingerprint proto
  fp `shouldBe` fp2


handshakeTests :: Spec
handshakeTests =
  describe
    "Handshake types"
    $ sequence_
      [ it "HandshakeRequest roundtrip" $ do
          let req =
                HandshakeRequest
                  { hsReqClientHash = BS.replicate 16 0xAA
                  , hsReqClientProtocol = Just "{\"protocol\":\"Test\"}"
                  , hsReqServerHash = BS.replicate 16 0xBB
                  , hsReqMeta = Nothing
                  }
              json = handshakeRequestToJSON req
          case handshakeRequestFromJSON json of
            Left err -> expectationFailure $ "Failed to parse handshake request: " ++ err
            Right parsed -> do
              hsReqClientHash parsed `shouldBe` hsReqClientHash req
              hsReqClientProtocol parsed `shouldBe` hsReqClientProtocol req
              hsReqServerHash parsed `shouldBe` hsReqServerHash req
      , it "HandshakeResponse roundtrip" $ do
          let resp =
                HandshakeResponse
                  { hsRespMatch = MatchBoth
                  , hsRespServerProtocol = Nothing
                  , hsRespServerHash = Nothing
                  , hsRespMeta = Nothing
                  }
              json = handshakeResponseToJSON resp
          case handshakeResponseFromJSON json of
            Left err -> expectationFailure $ "Failed to parse handshake response: " ++ err
            Right parsed -> do
              hsRespMatch parsed `shouldBe` MatchBoth
              hsRespServerProtocol parsed `shouldBe` Nothing
      , it "HandshakeResponse CLIENT match" $ do
          let resp =
                HandshakeResponse
                  { hsRespMatch = MatchClient
                  , hsRespServerProtocol = Just "{\"protocol\":\"Test\"}"
                  , hsRespServerHash = Just (BS.replicate 16 0xCC)
                  , hsRespMeta = Nothing
                  }
              json = handshakeResponseToJSON resp
          case handshakeResponseFromJSON json of
            Left err -> expectationFailure $ "Failed to parse handshake response: " ++ err
            Right parsed -> do
              hsRespMatch parsed `shouldBe` MatchClient
              hsRespServerProtocol parsed `shouldBe` Just "{\"protocol\":\"Test\"}"
      , it "HandshakeResponse NONE match" $ do
          let resp =
                HandshakeResponse
                  { hsRespMatch = MatchNone
                  , hsRespServerProtocol = Just "{}"
                  , hsRespServerHash = Just (BS.replicate 16 0x00)
                  , hsRespMeta = Just [("key", BS.pack [1, 2, 3])]
                  }
              json = handshakeResponseToJSON resp
          case handshakeResponseFromJSON json of
            Left err -> expectationFailure $ "Failed to parse handshake response: " ++ err
            Right parsed ->
              hsRespMatch parsed `shouldBe` MatchNone
      ]


minimalProtocol :: Spec
minimalProtocol = it "Minimal protocol (no types, no messages)" $ do
  let proto =
        AvroProtocol
          { protoName = "Empty"
          , protoNamespace = Nothing
          , protoDoc = Nothing
          , protoTypes = []
          , protoMessages = []
          }
      json = protocolToJSON proto
  case protocolFromJSON json of
    Left err -> expectationFailure $ "Failed to parse minimal protocol: " ++ err
    Right parsed -> do
      protoName parsed `shouldBe` "Empty"
      null (protoTypes parsed) `shouldBe` True
      null (protoMessages parsed) `shouldBe` True
  let fp1 = avroProtocolFingerprint proto
      fp2 = avroProtocolFingerprint proto
  fp1 `shouldBe` fp2


--------------------------------------------------------------------------------
-- Avro spec compliance tests (ported from haskell-works/avro)
--------------------------------------------------------------------------------

buildBytes :: B.Builder -> BS.ByteString
buildBytes = BL.toStrict . B.toLazyByteString


zigzagComplianceTests :: Spec
zigzagComplianceTests =
  describe
    "ZigZag encoding compliance"
    $ sequence_
      [ it "zigzag(0) = 0x00" $
          buildBytes (avroEncodeInt 0) `shouldBe` BS.pack [0x00]
      , it "zigzag(-1) = 0x01" $
          buildBytes (avroEncodeInt (-1)) `shouldBe` BS.pack [0x01]
      , it "zigzag(1) = 0x02" $
          buildBytes (avroEncodeInt 1) `shouldBe` BS.pack [0x02]
      , it "zigzag(-2) = 0x03" $
          buildBytes (avroEncodeInt (-2)) `shouldBe` BS.pack [0x03]
      , it "zigzag(2147483647) = 4294967294 as ULEB128" $
          buildBytes (avroEncodeInt 2147483647)
            `shouldBe` BS.pack [0xFE, 0xFF, 0xFF, 0xFF, 0x0F]
      , it "zigzag(-2147483648) = 4294967295 as ULEB128" $
          buildBytes (avroEncodeInt (-2147483648))
            `shouldBe` BS.pack [0xFF, 0xFF, 0xFF, 0xFF, 0x0F]
      ]


byteEncodingComplianceTests :: Spec
byteEncodingComplianceTests =
  describe
    "Byte encoding compliance"
    $ sequence_
      [ it "bool true = [0x01]" $
          encodeAvro (AvroPrimitive AvroBool) (AV.Bool True) `shouldBe` BS.pack [0x01]
      , it "bool false = [0x00]" $
          encodeAvro (AvroPrimitive AvroBool) (AV.Bool False) `shouldBe` BS.pack [0x00]
      , it "int 0 = [0x00]" $
          encodeAvro (AvroPrimitive AvroInt) (AV.Int 0) `shouldBe` BS.pack [0x00]
      , it "int -1 = [0x01]" $
          encodeAvro (AvroPrimitive AvroInt) (AV.Int (-1)) `shouldBe` BS.pack [0x01]
      , it "int 1 = [0x02]" $
          encodeAvro (AvroPrimitive AvroInt) (AV.Int 1) `shouldBe` BS.pack [0x02]
      , it "int 64 = [0x80, 0x01]" $
          encodeAvro (AvroPrimitive AvroInt) (AV.Int 64) `shouldBe` BS.pack [0x80, 0x01]
      , it "long 90071992547409917 = [0xfa,..,0x02]" $
          encodeAvro (AvroPrimitive AvroLong) (AV.Long 90071992547409917)
            `shouldBe` BS.pack [0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x02]
      , it "double 0.89 = LE IEEE 754" $
          encodeAvro (AvroPrimitive AvroDouble) (AV.Double 0.89)
            `shouldBe` BS.pack [123, 20, 174, 71, 225, 122, 236, 63]
      , it "double -2.0 = LE IEEE 754" $
          encodeAvro (AvroPrimitive AvroDouble) (AV.Double (-2.0))
            `shouldBe` BS.pack [0, 0, 0, 0, 0, 0, 0, 192]
      , it "double 1.0 = LE IEEE 754" $
          encodeAvro (AvroPrimitive AvroDouble) (AV.Double 1.0)
            `shouldBe` BS.pack [0, 0, 0, 0, 0, 0, 240, 63]
      , it "string \"foo\" = [0x06, 0x66, 0x6f, 0x6f]" $
          encodeAvro (AvroPrimitive AvroString) (AV.String "foo")
            `shouldBe` BS.pack [0x06, 0x66, 0x6f, 0x6f]
      , it "string \"This is an unit test\"" $ do
          let encoded = encodeAvro (AvroPrimitive AvroString) (AV.String "This is an unit test")
              expected = BS.pack (0x28 : BS.unpack (TE.encodeUtf8 "This is an unit test"))
          encoded `shouldBe` expected
      , it "null = []" $
          encodeAvro (AvroPrimitive AvroNull) AV.Null `shouldBe` BS.empty
      , it "float 0.0 = [0, 0, 0, 0]" $
          encodeAvro (AvroPrimitive AvroFloat) (AV.Float 0.0)
            `shouldBe` BS.pack [0, 0, 0, 0]
      ]


nullUnionComplianceTests :: Spec
nullUnionComplianceTests =
  describe
    "Null in union compliance"
    $ sequence_
      [ it "null first in [null, string] = [0x00]" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              encoded = encodeAvro ty (AV.Union 0 AV.Null)
          encoded `shouldBe` BS.pack [0x00]
      , it "null third in [string, bool, null] = [0x04]" $ do
          let ty =
                AvroUnion
                  ( V.fromList
                      [ AvroPrimitive AvroString
                      , AvroPrimitive AvroBool
                      , AvroPrimitive AvroNull
                      ]
                  )
              encoded = encodeAvro ty (AV.Union 2 AV.Null)
          encoded `shouldBe` BS.pack [0x04]
      ]


arrayEncodingComplianceTests :: Spec
arrayEncodingComplianceTests =
  describe
    "Array encoding compliance"
    $ sequence_
      [ it "empty array = [0x00]" $ do
          let ty = AvroArray (AvroPrimitive AvroInt)
              encoded = encodeAvro ty (AV.Array V.empty)
          encoded `shouldBe` BS.pack [0x00]
      , it "array [1,2,3] of int" $ do
          let ty = AvroArray (AvroPrimitive AvroInt)
              encoded = encodeAvro ty (AV.Array (V.fromList [AV.Int 1, AV.Int 2, AV.Int 3]))
          encoded `shouldBe` BS.pack [0x06, 0x02, 0x04, 0x06, 0x00]
      ]


mapEncodingComplianceTests :: Spec
mapEncodingComplianceTests =
  describe
    "Map encoding compliance"
    $ sequence_
      [ it "empty map = [0x00]" $ do
          let ty = AvroMap (AvroPrimitive AvroInt)
              encoded = encodeAvro ty (AV.Map V.empty)
          encoded `shouldBe` BS.pack [0x00]
      , it "map {\"a\": 1}" $ do
          let ty = AvroMap (AvroPrimitive AvroInt)
              encoded = encodeAvro ty (AV.Map (V.fromList [("a", AV.Int 1)]))
          encoded `shouldBe` BS.pack [0x02, 0x02, 0x61, 0x02, 0x00]
      ]


sizedBlockTests :: Spec
sizedBlockTests = describe "Negative-count blocks" $ do
  let intTy = AvroPrimitive AvroInt
  it "checks array block sizes and resumes at the following value" $ do
    let ty = AvroArray intTy
        wire size = BS.pack [1, size, 0x36, 0, 0x0e]
    decodeAvroAt ty (wire 2) 0 `shouldBe` Right (AV.Array (V.singleton (AV.Int 27)), 4)
    decodeAvroAt intTy (wire 2) 4 `shouldBe` Right (AV.Int 7, 5)
    mapM_ (\size -> isLeft (decodeAvroAt ty (wire size) 0) `shouldBe` True) [0, 4, 1, 0x7e]
  it "checks map block sizes and resumes at the following value" $ do
    let ty = AvroMap intTy
        wire size = BS.pack [1, size, 2, 0x61, 0x36, 0, 0x0e]
    decodeAvroAt ty (wire 6) 0 `shouldBe` Right (AV.Map (V.singleton ("a", AV.Int 27)), 6)
    decodeAvroAt intTy (wire 6) 6 `shouldBe` Right (AV.Int 7, 7)
    mapM_ (\size -> isLeft (decodeAvroAt ty (wire size) 0) `shouldBe` True) [2, 8, 1, 0x7e]
  it "allows zero-byte array blocks and continues across mixed block types" $ do
    let ty = AvroArray (AvroPrimitive AvroNull)
        wire = BS.pack [3, 0, 2, 0]
    decodeAvro ty wire `shouldBe` Right (AV.Array (V.replicate 3 AV.Null))
  it "rejects the count whose absolute value does not fit a long" $ do
    let wire = BS.pack (replicate 9 0xff ++ [1, 0, 0])
    isLeft (decodeAvro (AvroArray intTy) wire) `shouldBe` True
    isLeft (decodeAvro (AvroMap intTy) wire) `shouldBe` True
  it "checks sized map blocks in an object container header" $ do
    let entry = "avro.schema"
        metadata = B.toStrictByteString (avroEncodeLong 11 <> B.byteString entry <> avroEncodeLong 5 <> B.byteString "\"int\"")
        file size =
          "Obj\1"
            <> B.toStrictByteString (avroEncodeLong (-1) <> avroEncodeLong size)
            <> metadata
            <> BS.replicate 17 0
        size = fromIntegral (BS.length metadata)
    readContainer (file size) `shouldBe` Right (intTy, V.empty)
    isLeft (readContainer (file (size - 1))) `shouldBe` True
    isLeft (readContainer (file (size + 1))) `shouldBe` True


deconflictComplianceTests :: Spec
deconflictComplianceTests =
  describe
    "Schema resolution (deconflict) compliance"
    $ sequence_
      [ it "reader adds optional field with null default" $ do
          let innerWriter =
                mkRecordType
                  "Inner"
                  [("id", AvroPrimitive AvroInt)]
              innerReader =
                mkRecordTypeWithDefaults
                  "Inner"
                  [ ("id", AvroPrimitive AvroInt, Nothing)
                  ,
                    ( "smell"
                    , AvroUnion
                        ( V.fromList
                            [ AvroPrimitive AvroNull
                            , AvroPrimitive AvroString
                            ]
                        )
                    , Just AvroNull
                    )
                  ]
              writerTy =
                mkRecordType
                  "Outer"
                  [ ("name", AvroPrimitive AvroString)
                  , ("inner", innerWriter)
                  , ("other", innerWriter)
                  ]
              readerTy =
                mkRecordType
                  "Outer"
                  [ ("name", AvroPrimitive AvroString)
                  , ("inner", innerReader)
                  , ("other", innerReader)
                  ]
              writerVal =
                AV.Record
                  ( V.fromList
                      [ AV.String "test"
                      , AV.Record (V.fromList [AV.Int 42])
                      , AV.Record (V.fromList [AV.Int 99])
                      ]
                  )
              writerBs = encodeAvro writerTy writerVal

          case resolveSchema writerTy readerTy of
            Left err -> expectationFailure $ "resolveSchema failed: " ++ err
            Right res -> do
              case decodeAvro writerTy writerBs of
                Left err -> expectationFailure $ "decode failed: " ++ err
                Right decoded -> do
                  case resolveValue res decoded of
                    Left err -> expectationFailure $ "resolveValue failed: " ++ err
                    Right resolved -> do
                      case resolved of
                        AV.Record fields | V.length fields == 3 -> do
                          let AV.String nm = fields V.! 0
                              AV.Record innerFields = fields V.! 1
                              AV.Record otherFields = fields V.! 2
                          nm `shouldBe` "test"
                          V.length innerFields `shouldBe` 2
                          (innerFields V.! 0) `shouldBe` AV.Int 42
                          (innerFields V.! 1) `shouldBe` AV.Null
                          V.length otherFields `shouldBe` 2
                          (otherFields V.! 0) `shouldBe` AV.Int 99
                          (otherFields V.! 1) `shouldBe` AV.Null
                        other -> expectationFailure $ "unexpected resolved value: " ++ show other
      ]


propertyRoundtripComplianceTests :: Spec
propertyRoundtripComplianceTests =
  describe
    "Property-based roundtrip compliance"
    $ sequence_
      [ it "int32 full range roundtrip" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          let ty = AvroPrimitive AvroInt
              val = AV.Int n
          decodeAvro ty (encodeAvro ty val) === Right val
      , it "int64 full range roundtrip" $ property $ do
          n <- forAll $ Gen.int64 Range.linearBounded
          let ty = AvroPrimitive AvroLong
              val = AV.Long n
          decodeAvro ty (encodeAvro ty val) === Right val
      , it "double roundtrip" $ property $ do
          d <- forAll $ Gen.double (Range.linearFrac (-1e15) 1e15)
          let ty = AvroPrimitive AvroDouble
              val = AV.Double d
          decodeAvro ty (encodeAvro ty val) === Right val
      , it "text roundtrip (unicode)" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 512) Gen.unicode
          let ty = AvroPrimitive AvroString
              val = AV.String t
          decodeAvro ty (encodeAvro ty val) === Right val
      , it "text roundtrip (empty)" $ property $ do
          let ty = AvroPrimitive AvroString
              val = AV.String ""
          decodeAvro ty (encodeAvro ty val) === Right val
      , it "text roundtrip (long strings)" $ property $ do
          t <- forAll $ Gen.text (Range.linear 256 2048) Gen.unicode
          let ty = AvroPrimitive AvroString
              val = AV.String t
          decodeAvro ty (encodeAvro ty val) === Right val
      , it "bytes roundtrip (random binary)" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 1024)
          let ty = AvroPrimitive AvroBytes
              val = AV.Bytes bs
          decodeAvro ty (encodeAvro ty val) === Right val
      ]


fingerprintTests :: Spec
fingerprintTests =
  describe
    "Schema fingerprinting"
    $ sequence_
      [ it "CRC-64-AVRO fingerprint is 8 bytes" $ do
          let ty = AvroPrimitive AvroInt
          BS.length (avroFingerprint ty) `shouldBe` 8
      , it "MD5 fingerprint is 16 bytes" $ do
          let ty = AvroPrimitive AvroInt
          BS.length (avroFingerprintMD5 ty) `shouldBe` 16
      , it "same schema produces same fingerprint" $ do
          let ty =
                mkRecordType
                  "User"
                  [ ("name", AvroPrimitive AvroString)
                  , ("age", AvroPrimitive AvroInt)
                  ]
          avroFingerprint ty `shouldBe` avroFingerprint ty
      , it "different schemas produce different fingerprints" $ do
          let ty1 = AvroPrimitive AvroInt
              ty2 = AvroPrimitive AvroLong
          (avroFingerprint ty1 /= avroFingerprint ty2) `shouldBe` True
      , it "parsing canonical form strips doc and aliases" $ do
          let ty =
                AvroRecord
                  { avroRecordName = "TestRec"
                  , avroRecordNamespace = Just "com.example"
                  , avroRecordDoc = Just "A doc string that should be stripped"
                  , avroRecordAliases = V.fromList ["OldName"]
                  , avroRecordFields =
                      V.fromList
                        [ AvroField
                            { avroFieldName = "x"
                            , avroFieldType = AvroPrimitive AvroInt
                            , avroFieldDefault = Nothing
                            , avroFieldOrder = Just Ascending
                            , avroFieldAliases = V.fromList ["old_x"]
                            , avroFieldDoc = Just "field doc"
                            , avroFieldProps = Map.empty
                            }
                        ]
                  , avroRecordProps = Map.empty
                  }
              pcf = parsingCanonicalForm ty
          (not $ BS.isInfixOf "doc string" pcf) `shouldBe` True
      , it "PCF for primitive is quoted string" $ do
          parsingCanonicalForm (AvroPrimitive AvroNull) `shouldBe` "\"null\""
          parsingCanonicalForm (AvroPrimitive AvroInt) `shouldBe` "\"int\""
          parsingCanonicalForm (AvroPrimitive AvroString) `shouldBe` "\"string\""
      ]


aliasResolutionTests :: Spec
aliasResolutionTests =
  describe
    "Alias-aware resolution"
    $ sequence_
      [ it "reader field alias matches writer field name" $ do
          let writerTy =
                mkRecordType
                  "Rec"
                  [("old_name", AvroPrimitive AvroInt)]
              readerTy =
                AvroRecord
                  { avroRecordName = "Rec"
                  , avroRecordNamespace = Nothing
                  , avroRecordDoc = Nothing
                  , avroRecordAliases = V.empty
                  , avroRecordProps = Map.empty
                  , avroRecordFields =
                      V.fromList
                        [ AvroField
                            { avroFieldName = "new_name"
                            , avroFieldType = AvroPrimitive AvroInt
                            , avroFieldDefault = Nothing
                            , avroFieldOrder = Nothing
                            , avroFieldAliases = V.fromList ["old_name"]
                            , avroFieldDoc = Nothing
                            , avroFieldProps = Map.empty
                            }
                        ]
                  }
              writerVal = AV.Record (V.fromList [AV.Int 42])
          case resolveSchema writerTy readerTy of
            Left err -> expectationFailure $ "resolveSchema failed: " ++ err
            Right res -> resolveValue res writerVal `shouldBe` Right (AV.Record (V.fromList [AV.Int 42]))
      , it "writer field alias matches reader field name" $ do
          let writerTy =
                AvroRecord
                  { avroRecordName = "Rec"
                  , avroRecordNamespace = Nothing
                  , avroRecordDoc = Nothing
                  , avroRecordAliases = V.empty
                  , avroRecordProps = Map.empty
                  , avroRecordFields =
                      V.fromList
                        [ AvroField
                            { avroFieldName = "old_name"
                            , avroFieldType = AvroPrimitive AvroInt
                            , avroFieldDefault = Nothing
                            , avroFieldOrder = Nothing
                            , avroFieldAliases = V.fromList ["new_name"]
                            , avroFieldDoc = Nothing
                            , avroFieldProps = Map.empty
                            }
                        ]
                  }
              readerTy =
                mkRecordType
                  "Rec"
                  [("new_name", AvroPrimitive AvroInt)]
              writerVal = AV.Record (V.fromList [AV.Int 99])
          case resolveSchema writerTy readerTy of
            Left err -> expectationFailure $ "resolveSchema failed: " ++ err
            Right res -> resolveValue res writerVal `shouldBe` Right (AV.Record (V.fromList [AV.Int 99]))
      , it "exact name match still works" $ do
          let writerTy = mkRecordType "Rec" [("x", AvroPrimitive AvroInt)]
              readerTy = mkRecordType "Rec" [("x", AvroPrimitive AvroInt)]
          resolveSchema writerTy readerTy `shouldBe` Right ResolvedSame
      ]


-- Helpers

roundtrip :: (MonadTest m) => AvroType -> AV.Value -> m ()
roundtrip ty val =
  decodeAvro ty (encodeAvro ty val) === Right val


mkRecordType :: Text -> [(Text, AvroType)] -> AvroType
mkRecordType name fields =
  AvroRecord
    { avroRecordName = name
    , avroRecordNamespace = Nothing
    , avroRecordDoc = Nothing
    , avroRecordAliases = V.empty
    , avroRecordProps = Map.empty
    , avroRecordFields =
        V.fromList
          [ AvroField
              { avroFieldName = fname
              , avroFieldType = ftype
              , avroFieldDefault = Nothing
              , avroFieldOrder = Nothing
              , avroFieldAliases = V.empty
              , avroFieldDoc = Nothing
              , avroFieldProps = Map.empty
              }
          | (fname, ftype) <- fields
          ]
    }


mkRecordTypeWithDefaults :: Text -> [(Text, AvroType, Maybe AvroSchema)] -> AvroType
mkRecordTypeWithDefaults name fields =
  AvroRecord
    { avroRecordName = name
    , avroRecordNamespace = Nothing
    , avroRecordDoc = Nothing
    , avroRecordAliases = V.empty
    , avroRecordProps = Map.empty
    , avroRecordFields =
        V.fromList
          [ AvroField
              { avroFieldName = fname
              , avroFieldType = ftype
              , avroFieldDefault = dflt
              , avroFieldOrder = Nothing
              , avroFieldAliases = V.empty
              , avroFieldDoc = Nothing
              , avroFieldProps = Map.empty
              }
          | (fname, ftype, dflt) <- fields
          ]
    }


--------------------------------------------------------------------------------
-- Avro spec conformance vectors (from the Apache Avro specification)
-- Exact byte-level tests for all primitive types.
--------------------------------------------------------------------------------

avroSpecConformanceVectors :: Spec
avroSpecConformanceVectors =
  describe
    "Avro spec conformance vectors"
    $ sequence_
      [ describe
          "null"
          $ sequence_
            [ it "null encodes to 0 bytes" $
                encodeAvro (AvroPrimitive AvroNull) AV.Null `shouldBe` BS.empty
            , it "null decodes from 0 bytes" $
                decodeAvro (AvroPrimitive AvroNull) BS.empty `shouldBe` Right AV.Null
            ]
      , describe
          "boolean"
          $ sequence_
            [ it "true = [0x01]" $
                encodeAvro (AvroPrimitive AvroBool) (AV.Bool True) `shouldBe` BS.pack [0x01]
            , it "false = [0x00]" $
                encodeAvro (AvroPrimitive AvroBool) (AV.Bool False) `shouldBe` BS.pack [0x00]
            , it "decode true" $
                decodeAvro (AvroPrimitive AvroBool) (BS.pack [0x01]) `shouldBe` Right (AV.Bool True)
            , it "decode false" $
                decodeAvro (AvroPrimitive AvroBool) (BS.pack [0x00]) `shouldBe` Right (AV.Bool False)
            ]
      , describe
          "int (zigzag)"
          $ sequence_
            [ it "0 = [0x00]" $
                encodeAvro (AvroPrimitive AvroInt) (AV.Int 0) `shouldBe` BS.pack [0x00]
            , it "-1 = [0x01]" $
                encodeAvro (AvroPrimitive AvroInt) (AV.Int (-1)) `shouldBe` BS.pack [0x01]
            , it "1 = [0x02]" $
                encodeAvro (AvroPrimitive AvroInt) (AV.Int 1) `shouldBe` BS.pack [0x02]
            , it "-2 = [0x03]" $
                encodeAvro (AvroPrimitive AvroInt) (AV.Int (-2)) `shouldBe` BS.pack [0x03]
            , it "2 = [0x04]" $
                encodeAvro (AvroPrimitive AvroInt) (AV.Int 2) `shouldBe` BS.pack [0x04]
            , it "-64 = [0x7f]" $
                encodeAvro (AvroPrimitive AvroInt) (AV.Int (-64)) `shouldBe` BS.pack [0x7f]
            , it "64 = [0x80, 0x01]" $
                encodeAvro (AvroPrimitive AvroInt) (AV.Int 64) `shouldBe` BS.pack [0x80, 0x01]
            , it "max int32 = 5 bytes" $ do
                let bs = encodeAvro (AvroPrimitive AvroInt) (AV.Int maxBound)
                BS.length bs `shouldBe` 5
                decodeAvro (AvroPrimitive AvroInt) bs `shouldBe` Right (AV.Int maxBound)
            , it "min int32 = 5 bytes" $ do
                let bs = encodeAvro (AvroPrimitive AvroInt) (AV.Int minBound)
                BS.length bs `shouldBe` 5
                decodeAvro (AvroPrimitive AvroInt) bs `shouldBe` Right (AV.Int minBound)
            ]
      , describe
          "long (zigzag)"
          $ sequence_
            [ it "long 0 = [0x00]" $
                encodeAvro (AvroPrimitive AvroLong) (AV.Long 0) `shouldBe` BS.pack [0x00]
            , it "long -1 = [0x01]" $
                encodeAvro (AvroPrimitive AvroLong) (AV.Long (-1)) `shouldBe` BS.pack [0x01]
            , it "long 1 = [0x02]" $
                encodeAvro (AvroPrimitive AvroLong) (AV.Long 1) `shouldBe` BS.pack [0x02]
            , it "long max roundtrip" $ do
                let ty = AvroPrimitive AvroLong
                    val = AV.Long maxBound
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            , it "long min roundtrip" $ do
                let ty = AvroPrimitive AvroLong
                    val = AV.Long minBound
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            , it "long 90071992547409917" $
                encodeAvro (AvroPrimitive AvroLong) (AV.Long 90071992547409917)
                  `shouldBe` BS.pack [0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x02]
            ]
      , describe
          "float (IEEE 754 LE)"
          $ sequence_
            [ it "float 0.0 = [0,0,0,0]" $
                encodeAvro (AvroPrimitive AvroFloat) (AV.Float 0.0)
                  `shouldBe` BS.pack [0, 0, 0, 0]
            , it "float 1.0 = [0x00,0x00,0x80,0x3f]" $
                encodeAvro (AvroPrimitive AvroFloat) (AV.Float 1.0)
                  `shouldBe` BS.pack [0x00, 0x00, 0x80, 0x3f]
            , it "float roundtrip -3.14" $ do
                let ty = AvroPrimitive AvroFloat
                    val = AV.Float (-3.14)
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            ]
      , describe
          "double (IEEE 754 LE)"
          $ sequence_
            [ it "double 0.0 = 8 zero bytes" $
                encodeAvro (AvroPrimitive AvroDouble) (AV.Double 0.0)
                  `shouldBe` BS.pack [0, 0, 0, 0, 0, 0, 0, 0]
            , it "double 1.0" $
                encodeAvro (AvroPrimitive AvroDouble) (AV.Double 1.0)
                  `shouldBe` BS.pack [0, 0, 0, 0, 0, 0, 240, 63]
            , it "double -2.0" $
                encodeAvro (AvroPrimitive AvroDouble) (AV.Double (-2.0))
                  `shouldBe` BS.pack [0, 0, 0, 0, 0, 0, 0, 192]
            , it "double 0.89" $
                encodeAvro (AvroPrimitive AvroDouble) (AV.Double 0.89)
                  `shouldBe` BS.pack [123, 20, 174, 71, 225, 122, 236, 63]
            ]
      , describe
          "string"
          $ sequence_
            [ it "\"foo\" = [0x06, 0x66, 0x6f, 0x6f]" $
                encodeAvro (AvroPrimitive AvroString) (AV.String "foo")
                  `shouldBe` BS.pack [0x06, 0x66, 0x6f, 0x6f]
            , it "empty string = [0x00]" $
                encodeAvro (AvroPrimitive AvroString) (AV.String "")
                  `shouldBe` BS.pack [0x00]
            , it "\"a\" = [0x02, 0x61]" $
                encodeAvro (AvroPrimitive AvroString) (AV.String "a")
                  `shouldBe` BS.pack [0x02, 0x61]
            ]
      , describe
          "bytes"
          $ sequence_
            [ it "[0xDE, 0xAD] = [0x04, 0xDE, 0xAD]" $
                encodeAvro (AvroPrimitive AvroBytes) (AV.Bytes (BS.pack [0xDE, 0xAD]))
                  `shouldBe` BS.pack [0x04, 0xDE, 0xAD]
            , it "empty bytes = [0x00]" $
                encodeAvro (AvroPrimitive AvroBytes) (AV.Bytes BS.empty)
                  `shouldBe` BS.pack [0x00]
            ]
      , describe
          "array block encoding"
          $ sequence_
            [ it "empty array = [0x00]" $
                encodeAvro (AvroArray (AvroPrimitive AvroInt)) (AV.Array V.empty)
                  `shouldBe` BS.pack [0x00]
            , it "[1,2,3] = [count=6, zz(1)=2, zz(2)=4, zz(3)=6, 0]" $
                encodeAvro
                  (AvroArray (AvroPrimitive AvroInt))
                  (AV.Array (V.fromList [AV.Int 1, AV.Int 2, AV.Int 3]))
                  `shouldBe` BS.pack [0x06, 0x02, 0x04, 0x06, 0x00]
            , it "single element [42]" $ do
                let ty = AvroArray (AvroPrimitive AvroInt)
                    val = AV.Array (V.fromList [AV.Int 42])
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            , it "many elements [0..99]" $ do
                let ty = AvroArray (AvroPrimitive AvroInt)
                    val = AV.Array (V.fromList [AV.Int i | i <- [0 .. 99]])
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            ]
      , describe
          "map block encoding"
          $ sequence_
            [ it "empty map = [0x00]" $
                encodeAvro (AvroMap (AvroPrimitive AvroInt)) (AV.Map V.empty)
                  `shouldBe` BS.pack [0x00]
            , it "{\"a\": 1} = [0x02, 0x02, 0x61, 0x02, 0x00]" $
                encodeAvro
                  (AvroMap (AvroPrimitive AvroInt))
                  (AV.Map (V.fromList [("a", AV.Int 1)]))
                  `shouldBe` BS.pack [0x02, 0x02, 0x61, 0x02, 0x00]
            , it "single entry roundtrip" $ do
                let ty = AvroMap (AvroPrimitive AvroString)
                    val = AV.Map (V.fromList [("key", AV.String "value")])
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            ]
      , describe
          "union index encoding"
          $ sequence_
            [ it "null first in [null, string] = [0x00]" $ do
                let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
                encodeAvro ty (AV.Union 0 AV.Null) `shouldBe` BS.pack [0x00]
            , it "string second in [null, string] starts with [0x02]" $ do
                let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
                    bs = encodeAvro ty (AV.Union 1 (AV.String "hi"))
                BS.index bs 0 `shouldBe` 0x02
            , it "null third in [string, bool, null] = [0x04]" $ do
                let ty =
                      AvroUnion
                        ( V.fromList
                            [ AvroPrimitive AvroString
                            , AvroPrimitive AvroBool
                            , AvroPrimitive AvroNull
                            ]
                        )
                encodeAvro ty (AV.Union 2 AV.Null) `shouldBe` BS.pack [0x04]
            ]
      , describe
          "fixed encoding"
          $ sequence_
            [ it "fixed 4 bytes" $ do
                let ty = AvroFixed "F4" Nothing 4 V.empty
                    val = AV.Fixed (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])
                encodeAvro ty val `shouldBe` BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            , it "fixed 0 bytes" $ do
                let ty = AvroFixed "F0" Nothing 0 V.empty
                    val = AV.Fixed BS.empty
                encodeAvro ty val `shouldBe` BS.empty
                decodeAvro ty (encodeAvro ty val) `shouldBe` Right val
            ]
      , describe
          "enum encoding"
          $ sequence_
            [ it "enum ordinal 0 = [0x00]" $ do
                let syms = V.fromList ["A", "B", "C"]
                    ty = AvroEnum "E" Nothing Nothing V.empty syms Nothing
                encodeAvro ty (AV.Enum 0) `shouldBe` BS.pack [0x00]
            , it "enum ordinal 1 = [0x02]" $ do
                let syms = V.fromList ["A", "B", "C"]
                    ty = AvroEnum "E" Nothing Nothing V.empty syms Nothing
                encodeAvro ty (AV.Enum 1) `shouldBe` BS.pack [0x02]
            ]
      , describe
          "nested record"
          $ sequence_
            [ it "3-level nested record roundtrip" $ do
                let innerTy = mkRecordType "Inner" [("val", AvroPrimitive AvroInt)]
                    midTy = mkRecordType "Mid" [("inner", innerTy), ("label", AvroPrimitive AvroString)]
                    outerTy = mkRecordType "Outer" [("mid", midTy), ("flag", AvroPrimitive AvroBool)]
                    innerVal = AV.Record (V.fromList [AV.Int 42])
                    midVal = AV.Record (V.fromList [innerVal, AV.String "hello"])
                    outerVal = AV.Record (V.fromList [midVal, AV.Bool True])
                decodeAvro outerTy (encodeAvro outerTy outerVal) `shouldBe` Right outerVal
            ]
      ]
