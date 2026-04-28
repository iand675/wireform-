module Test.Avro (avroTests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (chr)
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Avro.Schema
import qualified Avro.Value as AV
import Avro.Encode (encodeAvro)
import Avro.Decode (decodeAvro)
import Avro.Wire (avroEncodeInt, avroEncodeLong)
import Avro.JSON (avroToJSON, avroFromJSON, avroSchemaToJSON, avroSchemaFromJSON)
import Avro.Protocol
import Avro.Resolution (ResolvedSchema(..), FieldResolution(..), resolveSchema, resolveValue)
import Avro.Fingerprint (avroFingerprint, avroFingerprintMD5, parsingCanonicalForm)

avroTests :: TestTree
avroTests = testGroup "Avro Encode/Decode"
  [ fingerprintTests
  , aliasResolutionTests
  , protocolTests
  , zigzagComplianceTests
  , byteEncodingComplianceTests
  , nullUnionComplianceTests
  , arrayEncodingComplianceTests
  , mapEncodingComplianceTests
  , deconflictComplianceTests
  , propertyRoundtripComplianceTests
  , avroSpecConformanceVectors
  , testGroup "Primitive roundtrips (property)"
      [ testProperty "null roundtrip" $ property $ do
          let ty = AvroPrimitive AvroNull
          roundtrip ty AV.Null

      , testProperty "bool roundtrip" $ property $ do
          b <- forAll Gen.bool
          roundtrip (AvroPrimitive AvroBool) (AV.Bool b)

      , testProperty "int roundtrip" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          roundtrip (AvroPrimitive AvroInt) (AV.Int n)

      , testProperty "long roundtrip" $ property $ do
          n <- forAll $ Gen.int64 Range.linearBounded
          roundtrip (AvroPrimitive AvroLong) (AV.Long n)

      , testProperty "float roundtrip" $ property $ do
          f <- forAll $ Gen.float (Range.linearFrac (-1e6) 1e6)
          roundtrip (AvroPrimitive AvroFloat) (AV.Float f)

      , testProperty "double roundtrip" $ property $ do
          d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
          roundtrip (AvroPrimitive AvroDouble) (AV.Double d)

      , testProperty "bytes roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 256)
          roundtrip (AvroPrimitive AvroBytes) (AV.Bytes bs)

      , testProperty "string roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 128) Gen.unicode
          roundtrip (AvroPrimitive AvroString) (AV.String t)
      ]

  , testGroup "Edge cases (unit)"
      [ testCase "null encodes to empty" $ do
          let bs = encodeAvro (AvroPrimitive AvroNull) AV.Null
          bs @?= BS.empty

      , testCase "empty string" $ do
          let ty = AvroPrimitive AvroString
              val = AV.String ""
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "empty bytes" $ do
          let ty = AvroPrimitive AvroBytes
              val = AV.Bytes ""
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "empty array" $ do
          let ty = AvroArray (AvroPrimitive AvroInt)
              val = AV.Array V.empty
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "empty map" $ do
          let ty = AvroMap (AvroPrimitive AvroString)
              val = AV.Map V.empty
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "union index 0 (null)" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              val = AV.Union 0 AV.Null
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "union index 1 (string)" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              val = AV.Union 1 (AV.String "hello")
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "bool true" $ do
          let ty = AvroPrimitive AvroBool
          decodeAvro ty (encodeAvro ty (AV.Bool True)) @?= Right (AV.Bool True)

      , testCase "bool false" $ do
          let ty = AvroPrimitive AvroBool
          decodeAvro ty (encodeAvro ty (AV.Bool False)) @?= Right (AV.Bool False)

      , testCase "int 0" $ do
          let ty = AvroPrimitive AvroInt
          decodeAvro ty (encodeAvro ty (AV.Int 0)) @?= Right (AV.Int 0)

      , testCase "int min" $ do
          let ty = AvroPrimitive AvroInt
          decodeAvro ty (encodeAvro ty (AV.Int minBound)) @?= Right (AV.Int minBound)

      , testCase "int max" $ do
          let ty = AvroPrimitive AvroInt
          decodeAvro ty (encodeAvro ty (AV.Int maxBound)) @?= Right (AV.Int maxBound)

      , testCase "long min" $ do
          let ty = AvroPrimitive AvroLong
          decodeAvro ty (encodeAvro ty (AV.Long minBound)) @?= Right (AV.Long minBound)

      , testCase "long max" $ do
          let ty = AvroPrimitive AvroLong
          decodeAvro ty (encodeAvro ty (AV.Long maxBound)) @?= Right (AV.Long maxBound)
      ]

  , testGroup "Record roundtrip"
      [ testProperty "mixed-field record" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          t <- forAll $ Gen.text (Range.linear 0 64) Gen.unicode
          b <- forAll Gen.bool
          d <- forAll $ Gen.double (Range.linearFrac (-1e6) 1e6)
          let ty = mkRecordType "TestRecord"
                     [ ("intField",    AvroPrimitive AvroInt)
                     , ("stringField", AvroPrimitive AvroString)
                     , ("boolField",   AvroPrimitive AvroBool)
                     , ("doubleField", AvroPrimitive AvroDouble)
                     ]
              val = AV.Record (V.fromList [AV.Int n, AV.String t, AV.Bool b, AV.Double d])
          roundtrip ty val
      ]

  , testGroup "Array roundtrip"
      [ testProperty "array of ints" $ property $ do
          ns <- forAll $ Gen.list (Range.linear 0 50) (Gen.int32 Range.linearBounded)
          let ty = AvroArray (AvroPrimitive AvroInt)
              val = AV.Array (V.fromList (map AV.Int ns))
          roundtrip ty val

      , testProperty "array of records" $ property $ do
          items <- forAll $ Gen.list (Range.linear 0 20) $ do
            i <- Gen.int32 Range.linearBounded
            s <- Gen.text (Range.linear 0 32) Gen.unicode
            pure (i, s)
          let recTy = mkRecordType "Item"
                        [ ("id",   AvroPrimitive AvroInt)
                        , ("name", AvroPrimitive AvroString)
                        ]
              ty = AvroArray recTy
              val = AV.Array (V.fromList [AV.Record (V.fromList [AV.Int i, AV.String s]) | (i, s) <- items])
          roundtrip ty val
      ]

  , testGroup "Map roundtrip"
      [ testProperty "map of longs" $ property $ do
          entries <- forAll $ Gen.list (Range.linear 0 30) $ do
            k <- Gen.text (Range.linear 1 32) Gen.alphaNum
            v <- Gen.int64 Range.linearBounded
            pure (k, v)
          let ty = AvroMap (AvroPrimitive AvroLong)
              val = AV.Map (V.fromList [(k, AV.Long v) | (k, v) <- entries])
          roundtrip ty val
      ]

  , testGroup "Union roundtrip"
      [ testProperty "null|string union" $ property $ do
          useNull <- forAll Gen.bool
          let branches = V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString]
              ty = AvroUnion branches
          val <- if useNull
                 then pure (AV.Union 0 AV.Null)
                 else do
                   t <- forAll $ Gen.text (Range.linear 0 64) Gen.unicode
                   pure (AV.Union 1 (AV.String t))
          roundtrip ty val

      , testProperty "int|long|string union" $ property $ do
          branch <- forAll $ Gen.int (Range.linear 0 2)
          let branches = V.fromList
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

  , testGroup "Fixed roundtrip"
      [ testProperty "fixed bytes" $ property $ do
          sz <- forAll $ Gen.int (Range.linear 0 64)
          bs <- forAll $ Gen.bytes (Range.singleton sz)
          let ty = AvroFixed "TestFixed" Nothing sz V.empty
              val = AV.Fixed bs
          roundtrip ty val

      , testCase "fixed empty" $ do
          let ty = AvroFixed "Empty" Nothing 0 V.empty
              val = AV.Fixed ""
          decodeAvro ty (encodeAvro ty val) @?= Right val
      ]

  , testGroup "Enum roundtrip"
      [ testProperty "enum ordinal" $ property $ do
          idx <- forAll $ Gen.int (Range.linear 0 9)
          let syms = V.fromList (map (T.pack . ("S" ++) . show) [0..9 :: Int])
              ty = AvroEnum "TestEnum" Nothing Nothing V.empty syms Nothing
              val = AV.Enum idx
          roundtrip ty val
      ]

  , testGroup "JSON roundtrip — primitives"
      [ testCase "null JSON roundtrip" $ do
          let ty = AvroPrimitive AvroNull
              val = AV.Null
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "bool JSON roundtrip" $ do
          let ty = AvroPrimitive AvroBool
          avroFromJSON ty (avroToJSON ty (AV.Bool True)) @?= Right (AV.Bool True)
          avroFromJSON ty (avroToJSON ty (AV.Bool False)) @?= Right (AV.Bool False)

      , testCase "int JSON roundtrip" $ do
          let ty = AvroPrimitive AvroInt
              val = AV.Int 42
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "long JSON roundtrip" $ do
          let ty = AvroPrimitive AvroLong
              val = AV.Long 123456789
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "float JSON roundtrip" $ do
          let ty = AvroPrimitive AvroFloat
              val = AV.Float 3.14
              Right (AV.Float result) = avroFromJSON ty (avroToJSON ty val)
          abs (result - 3.14) < 0.001 @?= True

      , testCase "double JSON roundtrip" $ do
          let ty = AvroPrimitive AvroDouble
              val = AV.Double 2.71828
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "bytes JSON roundtrip" $ do
          let ty = AvroPrimitive AvroBytes
              val = AV.Bytes (BS.pack [0, 1, 127, 255])
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "string JSON roundtrip" $ do
          let ty = AvroPrimitive AvroString
              val = AV.String "hello world"
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "float NaN JSON" $ do
          let ty = AvroPrimitive AvroFloat
          avroToJSON ty (AV.Float (0/0)) @?= Aeson.String "NaN"

      , testCase "double Infinity JSON" $ do
          let ty = AvroPrimitive AvroDouble
          avroToJSON ty (AV.Double (1/0)) @?= Aeson.String "Infinity"
          avroToJSON ty (AV.Double (negate (1/0))) @?= Aeson.String "-Infinity"

      , testCase "bytes JSON rejects code points above 0x00FF" $ do
          -- Avro JSON spec: bytes/fixed values map each character to one
          -- byte (code points 0..255). Higher code points must not
          -- silently truncate.
          let ty = AvroPrimitive AvroBytes
          case avroFromJSON ty (Aeson.String (T.pack "\x100")) of
            Left _ -> pure ()
            Right v -> assertFailure $
              "expected rejection of high code point, got: " ++ show v

      , testCase "fixed JSON rejects code points above 0x00FF" $ do
          let ty = AvroFixed
                { avroFixedName      = "F"
                , avroFixedNamespace = Nothing
                , avroFixedAliases   = V.empty
                , avroFixedSize      = 1
                }
          case avroFromJSON ty (Aeson.String (T.pack "\x100")) of
            Left _ -> pure ()
            Right v -> assertFailure $
              "expected rejection of high code point, got: " ++ show v

      , testCase "bytes JSON accepts full 0x00..0xFF range" $ do
          let ty = AvroPrimitive AvroBytes
              s  = T.pack [chr 0x00, chr 0xFF, chr 0x80]
          avroFromJSON ty (Aeson.String s)
            @?= Right (AV.Bytes (BS.pack [0x00, 0xFF, 0x80]))
      ]

  , testGroup "JSON record encode/decode"
      [ testCase "simple record" $ do
          let ty = mkRecordType "Person"
                     [ ("name", AvroPrimitive AvroString)
                     , ("age",  AvroPrimitive AvroInt)
                     ]
              val = AV.Record (V.fromList [AV.String "Alice", AV.Int 30])
              json = avroToJSON ty val
          avroFromJSON ty json @?= Right val

      , testCase "record JSON structure" $ do
          let ty = mkRecordType "Pair"
                     [ ("first",  AvroPrimitive AvroInt)
                     , ("second", AvroPrimitive AvroString)
                     ]
              val = AV.Record (V.fromList [AV.Int 1, AV.String "x"])
              json = avroToJSON ty val
          case json of
            Aeson.Object _ -> pure ()
            _              -> assertFailure "expected JSON object"
          avroFromJSON ty json @?= Right val
      ]

  , testGroup "JSON union encode/decode"
      [ testCase "null branch in union" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              val = AV.Union 0 AV.Null
          avroToJSON ty val @?= Aeson.Null
          avroFromJSON ty Aeson.Null @?= Right val

      , testCase "string branch in union" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              val = AV.Union 1 (AV.String "hello")
              json = avroToJSON ty val
          avroFromJSON ty json @?= Right val
      ]

  , testGroup "Schema to/from JSON roundtrip"
      [ testCase "primitive schemas" $ do
          let prims = [ AvroPrimitive AvroNull, AvroPrimitive AvroBool
                       , AvroPrimitive AvroInt, AvroPrimitive AvroLong
                       , AvroPrimitive AvroFloat, AvroPrimitive AvroDouble
                       , AvroPrimitive AvroBytes, AvroPrimitive AvroString ]
          mapM_ (\ty -> avroSchemaFromJSON (avroSchemaToJSON ty) @?= Right ty) prims

      , testCase "record schema roundtrip" $ do
          let ty = mkRecordType "MyRec"
                     [ ("x", AvroPrimitive AvroInt)
                     , ("y", AvroPrimitive AvroString)
                     ]
              Right parsed = avroSchemaFromJSON (avroSchemaToJSON ty)
          avroRecordName parsed @?= "MyRec"
          V.length (avroRecordFields parsed) @?= 2

      , testCase "enum schema roundtrip" $ do
          let ty = AvroEnum "Color" Nothing Nothing V.empty
                     (V.fromList ["RED","GREEN","BLUE"]) Nothing
              Right parsed = avroSchemaFromJSON (avroSchemaToJSON ty)
          avroEnumName parsed @?= "Color"
          avroEnumSymbols parsed @?= V.fromList ["RED","GREEN","BLUE"]

      , testCase "array schema roundtrip" $ do
          let ty = AvroArray (AvroPrimitive AvroInt)
          avroSchemaFromJSON (avroSchemaToJSON ty) @?= Right ty

      , testCase "map schema roundtrip" $ do
          let ty = AvroMap (AvroPrimitive AvroString)
          avroSchemaFromJSON (avroSchemaToJSON ty) @?= Right ty

      , testCase "union schema roundtrip" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroInt])
          avroSchemaFromJSON (avroSchemaToJSON ty) @?= Right ty

      , testCase "fixed schema roundtrip" $ do
          let ty = AvroFixed "Hash" Nothing 16 V.empty
              Right parsed = avroSchemaFromJSON (avroSchemaToJSON ty)
          avroFixedName parsed @?= "Hash"
          avroFixedSize parsed @?= 16
      ]

  , testGroup "Resolution — compatible schemas"
      [ testCase "add field with default" $ do
          let writerTy = mkRecordType "Rec"
                           [ ("a", AvroPrimitive AvroInt) ]
              readerTy = mkRecordTypeWithDefaults "Rec"
                           [ ("a", AvroPrimitive AvroInt, Nothing)
                           , ("b", AvroPrimitive AvroString, Just (Aeson.String "hello"))
                           ]
              Right res = resolveSchema writerTy readerTy
              writerVal = AV.Record (V.fromList [AV.Int 42])
          -- The default JSON literal "hello" is now actually
          -- preserved through resolution (was lossily replaced with
          -- the empty string by the old defaultSchemaToJSON path).
          resolveValue res writerVal @?= Right (AV.Record (V.fromList [AV.Int 42, AV.String "hello"]))

      , testCase "add field with non-trivial JSON default literal" $ do
          -- Avro 1.11 §schema-record: the default JSON value is
          -- preserved verbatim and interpreted under the field's
          -- reader schema. The previous resolver always substituted
          -- the type's zero (\"\" for string, 0 for int) regardless
          -- of the actual default literal.
          let writerTy = mkRecordType "Rec" [ ("a", AvroPrimitive AvroInt) ]
              readerTy = mkRecordTypeWithDefaults "Rec"
                           [ ("a", AvroPrimitive AvroInt, Nothing)
                           , ("count", AvroPrimitive AvroInt,
                                Just (Aeson.Number 17))
                           , ("note", AvroPrimitive AvroString,
                                Just (Aeson.String "n/a"))
                           ]
              Right res = resolveSchema writerTy readerTy
              writerVal = AV.Record (V.fromList [AV.Int 1])
          resolveValue res writerVal
            @?= Right (AV.Record (V.fromList
                  [ AV.Int 1
                  , AV.Int 17
                  , AV.String "n/a"
                  ]))

      , testCase "default mismatched against type is rejected" $ do
          let writerTy = mkRecordType "Rec" [ ("a", AvroPrimitive AvroInt) ]
              readerTy = mkRecordTypeWithDefaults "Rec"
                           [ ("a", AvroPrimitive AvroInt, Nothing)
                           , ("b", AvroPrimitive AvroInt,
                                Just (Aeson.String "not-an-int"))
                           ]
          case resolveSchema writerTy readerTy of
            Left _  -> pure ()
            Right _ -> assertFailure
              "expected resolveSchema to reject malformed default"

      , testCase "remove field" $ do
          let writerTy = mkRecordType "Rec"
                           [ ("a", AvroPrimitive AvroInt)
                           , ("b", AvroPrimitive AvroString)
                           ]
              readerTy = mkRecordType "Rec"
                           [ ("a", AvroPrimitive AvroInt) ]
              Right res = resolveSchema writerTy readerTy
              writerVal = AV.Record (V.fromList [AV.Int 99, AV.String "dropped"])
          resolveValue res writerVal @?= Right (AV.Record (V.fromList [AV.Int 99]))

      , testCase "promote int -> long" $ do
          let writerTy = AvroPrimitive AvroInt
              readerTy = AvroPrimitive AvroLong
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AV.Int 42) @?= Right (AV.Long 42)

      , testCase "promote int -> float" $ do
          let writerTy = AvroPrimitive AvroInt
              readerTy = AvroPrimitive AvroFloat
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AV.Int 7) @?= Right (AV.Float 7.0)

      , testCase "promote int -> double" $ do
          let writerTy = AvroPrimitive AvroInt
              readerTy = AvroPrimitive AvroDouble
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AV.Int 7) @?= Right (AV.Double 7.0)

      , testCase "promote long -> float" $ do
          let writerTy = AvroPrimitive AvroLong
              readerTy = AvroPrimitive AvroFloat
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AV.Long 100) @?= Right (AV.Float 100.0)

      , testCase "promote long -> double" $ do
          let writerTy = AvroPrimitive AvroLong
              readerTy = AvroPrimitive AvroDouble
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AV.Long 100) @?= Right (AV.Double 100.0)

      , testCase "promote float -> double" $ do
          let writerTy = AvroPrimitive AvroFloat
              readerTy = AvroPrimitive AvroDouble
              Right res = resolveSchema writerTy readerTy
              Right (AV.Double d) = resolveValue res (AV.Float 1.5)
          abs (d - 1.5) < 0.001 @?= True

      , testCase "same schema resolves trivially" $ do
          let ty = AvroPrimitive AvroInt
          resolveSchema ty ty @?= Right ResolvedSame

      , testCase "array resolution" $ do
          let writerTy = AvroArray (AvroPrimitive AvroInt)
              readerTy = AvroArray (AvroPrimitive AvroLong)
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AV.Array (V.fromList [AV.Int 1, AV.Int 2]))
            @?= Right (AV.Array (V.fromList [AV.Long 1, AV.Long 2]))

      , testCase "map resolution" $ do
          let writerTy = AvroMap (AvroPrimitive AvroInt)
              readerTy = AvroMap (AvroPrimitive AvroLong)
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AV.Map (V.fromList [("k", AV.Int 5)]))
            @?= Right (AV.Map (V.fromList [("k", AV.Long 5)]))
      ]

  , testGroup "Resolution — incompatible schemas"
      [ testCase "type mismatch int vs string" $ do
          let res = resolveSchema (AvroPrimitive AvroInt) (AvroPrimitive AvroString)
          case res of
            Left _ -> pure ()
            Right _ -> assertFailure "expected incompatibility error"

      , testCase "missing required field" $ do
          let writerTy = mkRecordType "Rec"
                           [ ("a", AvroPrimitive AvroInt) ]
              readerTy = mkRecordType "Rec"
                           [ ("a", AvroPrimitive AvroInt)
                           , ("b", AvroPrimitive AvroString)
                           ]
          case resolveSchema writerTy readerTy of
            Left _ -> pure ()
            Right _ -> assertFailure "expected missing field error"

      , testCase "fixed size mismatch" $ do
          let w = AvroFixed "Hash" Nothing 16 V.empty
              r = AvroFixed "Hash" Nothing 32 V.empty
          case resolveSchema w r of
            Left _ -> pure ()
            Right _ -> assertFailure "expected fixed size mismatch error"

      , testCase "enum symbol mismatch" $ do
          let w = AvroEnum "E" Nothing Nothing V.empty (V.fromList ["A","B","C"]) Nothing
              r = AvroEnum "E" Nothing Nothing V.empty (V.fromList ["A","B"]) Nothing
          case resolveSchema w r of
            Left _ -> pure ()
            Right _ -> assertFailure "expected enum symbol mismatch"

      , testCase "string vs int incompatible" $ do
          let res = resolveSchema (AvroPrimitive AvroString) (AvroPrimitive AvroInt)
          case res of
            Left _ -> pure ()
            Right _ -> assertFailure "expected incompatibility error"
      ]
  ]

--------------------------------------------------------------------------------
-- Avro Protocol tests
--------------------------------------------------------------------------------

protocolTests :: TestTree
protocolTests = testGroup "Avro Protocol"
  [ protocolJsonRoundtrip
  , protocolFingerprintTest
  , handshakeTests
  , minimalProtocol
  ]

protocolJsonRoundtrip :: TestTree
protocolJsonRoundtrip = testCase "Protocol JSON roundtrip" $ do
  let proto = AvroProtocol
        { protoName      = "HelloService"
        , protoNamespace = Just "com.example"
        , protoDoc       = Just "A simple service"
        , protoTypes     =
            [ mkRecordType "Greeting"
                [ ("message", AvroPrimitive AvroString) ]
            , AvroEnum
                { avroEnumName      = "Tone"
                , avroEnumNamespace = Nothing
                , avroEnumDoc       = Nothing
                , avroEnumAliases   = V.empty
                , avroEnumSymbols   = V.fromList ["FRIENDLY", "FORMAL"]
                , avroEnumDefault   = Nothing
                }
            ]
        , protoMessages  =
            [ ("hello", AvroMessage
                { msgRequest  =
                    [ AvroParam "name" (AvroPrimitive AvroString)
                    , AvroParam "tone" (AvroPrimitive (AvroSchemaRef "Tone"))
                    ]
                , msgResponse = AvroPrimitive (AvroSchemaRef "Greeting")
                , msgErrors   = Nothing
                , msgOneWay   = False
                })
            , ("ping", AvroMessage
                { msgRequest  = []
                , msgResponse = AvroPrimitive AvroNull
                , msgErrors   = Nothing
                , msgOneWay   = True
                })
            ]
        }
      json = protocolToJSON proto
  case protocolFromJSON json of
    Left err -> assertFailure $ "Failed to parse protocol JSON: " ++ err
    Right parsed -> do
      protoName parsed @?= "HelloService"
      protoNamespace parsed @?= Just "com.example"
      protoDoc parsed @?= Just "A simple service"
      length (protoTypes parsed) @?= 2
      length (protoMessages parsed) @?= 2

protocolFingerprintTest :: TestTree
protocolFingerprintTest = testCase "Protocol fingerprint is 16 bytes (MD5)" $ do
  let proto = AvroProtocol
        { protoName      = "TestService"
        , protoNamespace = Nothing
        , protoDoc       = Nothing
        , protoTypes     = []
        , protoMessages  =
            [ ("echo", AvroMessage
                { msgRequest  = [AvroParam "msg" (AvroPrimitive AvroString)]
                , msgResponse = AvroPrimitive AvroString
                , msgErrors   = Nothing
                , msgOneWay   = False
                })
            ]
        }
      fp = avroProtocolFingerprint proto
  BS.length fp @?= 16
  let fp2 = avroProtocolFingerprint proto
  fp @?= fp2

handshakeTests :: TestTree
handshakeTests = testGroup "Handshake types"
  [ testCase "HandshakeRequest roundtrip" $ do
      let req = HandshakeRequest
            { hsReqClientHash     = BS.replicate 16 0xAA
            , hsReqClientProtocol = Just "{\"protocol\":\"Test\"}"
            , hsReqServerHash     = BS.replicate 16 0xBB
            , hsReqMeta           = Nothing
            }
          json = handshakeRequestToJSON req
      case handshakeRequestFromJSON json of
        Left err -> assertFailure $ "Failed to parse handshake request: " ++ err
        Right parsed -> do
          hsReqClientHash parsed @?= hsReqClientHash req
          hsReqClientProtocol parsed @?= hsReqClientProtocol req
          hsReqServerHash parsed @?= hsReqServerHash req

  , testCase "HandshakeResponse roundtrip" $ do
      let resp = HandshakeResponse
            { hsRespMatch          = MatchBoth
            , hsRespServerProtocol = Nothing
            , hsRespServerHash     = Nothing
            , hsRespMeta           = Nothing
            }
          json = handshakeResponseToJSON resp
      case handshakeResponseFromJSON json of
        Left err -> assertFailure $ "Failed to parse handshake response: " ++ err
        Right parsed -> do
          hsRespMatch parsed @?= MatchBoth
          hsRespServerProtocol parsed @?= Nothing

  , testCase "HandshakeResponse CLIENT match" $ do
      let resp = HandshakeResponse
            { hsRespMatch          = MatchClient
            , hsRespServerProtocol = Just "{\"protocol\":\"Test\"}"
            , hsRespServerHash     = Just (BS.replicate 16 0xCC)
            , hsRespMeta           = Nothing
            }
          json = handshakeResponseToJSON resp
      case handshakeResponseFromJSON json of
        Left err -> assertFailure $ "Failed to parse handshake response: " ++ err
        Right parsed -> do
          hsRespMatch parsed @?= MatchClient
          hsRespServerProtocol parsed @?= Just "{\"protocol\":\"Test\"}"

  , testCase "HandshakeResponse NONE match" $ do
      let resp = HandshakeResponse
            { hsRespMatch          = MatchNone
            , hsRespServerProtocol = Just "{}"
            , hsRespServerHash     = Just (BS.replicate 16 0x00)
            , hsRespMeta           = Just [("key", BS.pack [1,2,3])]
            }
          json = handshakeResponseToJSON resp
      case handshakeResponseFromJSON json of
        Left err -> assertFailure $ "Failed to parse handshake response: " ++ err
        Right parsed ->
          hsRespMatch parsed @?= MatchNone
  ]

minimalProtocol :: TestTree
minimalProtocol = testCase "Minimal protocol (no types, no messages)" $ do
  let proto = AvroProtocol
        { protoName      = "Empty"
        , protoNamespace = Nothing
        , protoDoc       = Nothing
        , protoTypes     = []
        , protoMessages  = []
        }
      json = protocolToJSON proto
  case protocolFromJSON json of
    Left err -> assertFailure $ "Failed to parse minimal protocol: " ++ err
    Right parsed -> do
      protoName parsed @?= "Empty"
      null (protoTypes parsed) @?= True
      null (protoMessages parsed) @?= True
  let fp1 = avroProtocolFingerprint proto
      fp2 = avroProtocolFingerprint proto
  fp1 @?= fp2

--------------------------------------------------------------------------------
-- Avro spec compliance tests (ported from haskell-works/avro)
--------------------------------------------------------------------------------

buildBytes :: B.Builder -> BS.ByteString
buildBytes = BL.toStrict . B.toLazyByteString

zigzagComplianceTests :: TestTree
zigzagComplianceTests = testGroup "ZigZag encoding compliance"
  [ testCase "zigzag(0) = 0x00" $
      buildBytes (avroEncodeInt 0) @?= BS.pack [0x00]
  , testCase "zigzag(-1) = 0x01" $
      buildBytes (avroEncodeInt (-1)) @?= BS.pack [0x01]
  , testCase "zigzag(1) = 0x02" $
      buildBytes (avroEncodeInt 1) @?= BS.pack [0x02]
  , testCase "zigzag(-2) = 0x03" $
      buildBytes (avroEncodeInt (-2)) @?= BS.pack [0x03]
  , testCase "zigzag(2147483647) = 4294967294 as ULEB128" $
      buildBytes (avroEncodeInt 2147483647)
        @?= BS.pack [0xFE, 0xFF, 0xFF, 0xFF, 0x0F]
  , testCase "zigzag(-2147483648) = 4294967295 as ULEB128" $
      buildBytes (avroEncodeInt (-2147483648))
        @?= BS.pack [0xFF, 0xFF, 0xFF, 0xFF, 0x0F]
  ]

byteEncodingComplianceTests :: TestTree
byteEncodingComplianceTests = testGroup "Byte encoding compliance"
  [ testCase "bool true = [0x01]" $
      encodeAvro (AvroPrimitive AvroBool) (AV.Bool True) @?= BS.pack [0x01]
  , testCase "bool false = [0x00]" $
      encodeAvro (AvroPrimitive AvroBool) (AV.Bool False) @?= BS.pack [0x00]
  , testCase "int 0 = [0x00]" $
      encodeAvro (AvroPrimitive AvroInt) (AV.Int 0) @?= BS.pack [0x00]
  , testCase "int -1 = [0x01]" $
      encodeAvro (AvroPrimitive AvroInt) (AV.Int (-1)) @?= BS.pack [0x01]
  , testCase "int 1 = [0x02]" $
      encodeAvro (AvroPrimitive AvroInt) (AV.Int 1) @?= BS.pack [0x02]
  , testCase "int 64 = [0x80, 0x01]" $
      encodeAvro (AvroPrimitive AvroInt) (AV.Int 64) @?= BS.pack [0x80, 0x01]
  , testCase "long 90071992547409917 = [0xfa,..,0x02]" $
      encodeAvro (AvroPrimitive AvroLong) (AV.Long 90071992547409917)
        @?= BS.pack [0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x02]
  , testCase "double 0.89 = LE IEEE 754" $
      encodeAvro (AvroPrimitive AvroDouble) (AV.Double 0.89)
        @?= BS.pack [123, 20, 174, 71, 225, 122, 236, 63]
  , testCase "double -2.0 = LE IEEE 754" $
      encodeAvro (AvroPrimitive AvroDouble) (AV.Double (-2.0))
        @?= BS.pack [0, 0, 0, 0, 0, 0, 0, 192]
  , testCase "double 1.0 = LE IEEE 754" $
      encodeAvro (AvroPrimitive AvroDouble) (AV.Double 1.0)
        @?= BS.pack [0, 0, 0, 0, 0, 0, 240, 63]
  , testCase "string \"foo\" = [0x06, 0x66, 0x6f, 0x6f]" $
      encodeAvro (AvroPrimitive AvroString) (AV.String "foo")
        @?= BS.pack [0x06, 0x66, 0x6f, 0x6f]
  , testCase "string \"This is an unit test\"" $ do
      let encoded = encodeAvro (AvroPrimitive AvroString) (AV.String "This is an unit test")
          expected = BS.pack (0x28 : BS.unpack (TE.encodeUtf8 "This is an unit test"))
      encoded @?= expected
  , testCase "null = []" $
      encodeAvro (AvroPrimitive AvroNull) AV.Null @?= BS.empty
  , testCase "float 0.0 = [0, 0, 0, 0]" $
      encodeAvro (AvroPrimitive AvroFloat) (AV.Float 0.0)
        @?= BS.pack [0, 0, 0, 0]
  ]

nullUnionComplianceTests :: TestTree
nullUnionComplianceTests = testGroup "Null in union compliance"
  [ testCase "null first in [null, string] = [0x00]" $ do
      let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
          encoded = encodeAvro ty (AV.Union 0 AV.Null)
      encoded @?= BS.pack [0x00]
  , testCase "null third in [string, bool, null] = [0x04]" $ do
      let ty = AvroUnion (V.fromList
                 [ AvroPrimitive AvroString
                 , AvroPrimitive AvroBool
                 , AvroPrimitive AvroNull
                 ])
          encoded = encodeAvro ty (AV.Union 2 AV.Null)
      encoded @?= BS.pack [0x04]
  ]

arrayEncodingComplianceTests :: TestTree
arrayEncodingComplianceTests = testGroup "Array encoding compliance"
  [ testCase "empty array = [0x00]" $ do
      let ty = AvroArray (AvroPrimitive AvroInt)
          encoded = encodeAvro ty (AV.Array V.empty)
      encoded @?= BS.pack [0x00]
  , testCase "array [1,2,3] of int" $ do
      let ty = AvroArray (AvroPrimitive AvroInt)
          encoded = encodeAvro ty (AV.Array (V.fromList [AV.Int 1, AV.Int 2, AV.Int 3]))
      encoded @?= BS.pack [0x06, 0x02, 0x04, 0x06, 0x00]
  ]

mapEncodingComplianceTests :: TestTree
mapEncodingComplianceTests = testGroup "Map encoding compliance"
  [ testCase "empty map = [0x00]" $ do
      let ty = AvroMap (AvroPrimitive AvroInt)
          encoded = encodeAvro ty (AV.Map V.empty)
      encoded @?= BS.pack [0x00]
  , testCase "map {\"a\": 1}" $ do
      let ty = AvroMap (AvroPrimitive AvroInt)
          encoded = encodeAvro ty (AV.Map (V.fromList [("a", AV.Int 1)]))
      encoded @?= BS.pack [0x02, 0x02, 0x61, 0x02, 0x00]
  ]

deconflictComplianceTests :: TestTree
deconflictComplianceTests = testGroup "Schema resolution (deconflict) compliance"
  [ testCase "reader adds optional field with null default" $ do
      let innerWriter = mkRecordType "Inner"
                          [ ("id", AvroPrimitive AvroInt) ]
          innerReader = mkRecordTypeWithDefaults "Inner"
                          [ ("id", AvroPrimitive AvroInt, Nothing)
                          , ("smell", AvroUnion (V.fromList
                              [ AvroPrimitive AvroNull
                              , AvroPrimitive AvroString
                              ]), Just Aeson.Null)
                          ]
          writerTy = mkRecordType "Outer"
                       [ ("name",  AvroPrimitive AvroString)
                       , ("inner", innerWriter)
                       , ("other", innerWriter)
                       ]
          readerTy = mkRecordType "Outer"
                       [ ("name",  AvroPrimitive AvroString)
                       , ("inner", innerReader)
                       , ("other", innerReader)
                       ]
          writerVal = AV.Record (V.fromList
            [ AV.String "test"
            , AV.Record (V.fromList [AV.Int 42])
            , AV.Record (V.fromList [AV.Int 99])
            ])
          writerBs = encodeAvro writerTy writerVal

      case resolveSchema writerTy readerTy of
        Left err -> assertFailure $ "resolveSchema failed: " ++ err
        Right res -> do
          case decodeAvro writerTy writerBs of
            Left err -> assertFailure $ "decode failed: " ++ err
            Right decoded -> do
              case resolveValue res decoded of
                Left err -> assertFailure $ "resolveValue failed: " ++ err
                Right resolved -> do
                  case resolved of
                    AV.Record fields | V.length fields == 3 -> do
                      let AV.String nm = fields V.! 0
                          AV.Record innerFields = fields V.! 1
                          AV.Record otherFields = fields V.! 2
                      nm @?= "test"
                      V.length innerFields @?= 2
                      (innerFields V.! 0) @?= AV.Int 42
                      -- Avro 1.11 §schema-record: a union default is
                      -- interpreted as the first branch; here that
                      -- branch is null, so the resolved value is
                      -- @Union 0 Null@, not bare @Null@.
                      (innerFields V.! 1) @?= AV.Union 0 AV.Null
                      V.length otherFields @?= 2
                      (otherFields V.! 0) @?= AV.Int 99
                      (otherFields V.! 1) @?= AV.Union 0 AV.Null
                    other -> assertFailure $ "unexpected resolved value: " ++ show other
  ]

propertyRoundtripComplianceTests :: TestTree
propertyRoundtripComplianceTests = testGroup "Property-based roundtrip compliance"
  [ testProperty "int32 full range roundtrip" $ property $ do
      n <- forAll $ Gen.int32 Range.linearBounded
      let ty = AvroPrimitive AvroInt
          val = AV.Int n
      decodeAvro ty (encodeAvro ty val) === Right val

  , testProperty "int64 full range roundtrip" $ property $ do
      n <- forAll $ Gen.int64 Range.linearBounded
      let ty = AvroPrimitive AvroLong
          val = AV.Long n
      decodeAvro ty (encodeAvro ty val) === Right val

  , testProperty "double roundtrip" $ property $ do
      d <- forAll $ Gen.double (Range.linearFrac (-1e15) 1e15)
      let ty = AvroPrimitive AvroDouble
          val = AV.Double d
      decodeAvro ty (encodeAvro ty val) === Right val

  , testProperty "text roundtrip (unicode)" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 512) Gen.unicode
      let ty = AvroPrimitive AvroString
          val = AV.String t
      decodeAvro ty (encodeAvro ty val) === Right val

  , testProperty "text roundtrip (empty)" $ property $ do
      let ty = AvroPrimitive AvroString
          val = AV.String ""
      decodeAvro ty (encodeAvro ty val) === Right val

  , testProperty "text roundtrip (long strings)" $ property $ do
      t <- forAll $ Gen.text (Range.linear 256 2048) Gen.unicode
      let ty = AvroPrimitive AvroString
          val = AV.String t
      decodeAvro ty (encodeAvro ty val) === Right val

  , testProperty "bytes roundtrip (random binary)" $ property $ do
      bs <- forAll $ Gen.bytes (Range.linear 0 1024)
      let ty = AvroPrimitive AvroBytes
          val = AV.Bytes bs
      decodeAvro ty (encodeAvro ty val) === Right val
  ]

fingerprintTests :: TestTree
fingerprintTests = testGroup "Schema fingerprinting"
  [ testCase "CRC-64-AVRO fingerprint is 8 bytes" $ do
      let ty = AvroPrimitive AvroInt
      BS.length (avroFingerprint ty) @?= 8

  , testCase "MD5 fingerprint is 16 bytes" $ do
      let ty = AvroPrimitive AvroInt
      BS.length (avroFingerprintMD5 ty) @?= 16

  , testCase "same schema produces same fingerprint" $ do
      let ty = mkRecordType "User"
                 [ ("name", AvroPrimitive AvroString)
                 , ("age", AvroPrimitive AvroInt)
                 ]
      avroFingerprint ty @?= avroFingerprint ty

  , testCase "different schemas produce different fingerprints" $ do
      let ty1 = AvroPrimitive AvroInt
          ty2 = AvroPrimitive AvroLong
      assertBool "fingerprints should differ" (avroFingerprint ty1 /= avroFingerprint ty2)

  , testCase "parsing canonical form strips doc and aliases" $ do
      let ty = AvroRecord
            { avroRecordName = "TestRec"
            , avroRecordNamespace = Just "com.example"
            , avroRecordDoc = Just "A doc string that should be stripped"
            , avroRecordAliases = V.fromList ["OldName"]
            , avroRecordFields = V.fromList
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
      assertBool "PCF should not contain doc" (not $ BS.isInfixOf "doc string" pcf)

  , testCase "PCF for primitive is quoted string" $ do
      parsingCanonicalForm (AvroPrimitive AvroNull) @?= "\"null\""
      parsingCanonicalForm (AvroPrimitive AvroInt) @?= "\"int\""
      parsingCanonicalForm (AvroPrimitive AvroString) @?= "\"string\""

  , testCase "PCF folds namespace into fully-qualified record name" $ do
      -- Avro 1.11 PCF rule: "Replace short names with fullnames using
      -- applicable namespaces". Otherwise two records with the same
      -- short name but different namespaces would have identical
      -- fingerprints, defeating registry lookups.
      let ty = AvroRecord
            { avroRecordName = "Foo"
            , avroRecordNamespace = Just "com.example"
            , avroRecordDoc = Nothing
            , avroRecordAliases = V.empty
            , avroRecordFields = V.empty
            , avroRecordProps = Map.empty
            }
          pcf = parsingCanonicalForm ty
      assertBool "PCF contains fully-qualified name"
        (BS.isInfixOf "\"com.example.Foo\"" pcf)

  , testCase "PCF differs for same record name with different namespaces" $ do
      let mk ns = AvroRecord
            { avroRecordName = "Foo"
            , avroRecordNamespace = Just ns
            , avroRecordDoc = Nothing
            , avroRecordAliases = V.empty
            , avroRecordFields = V.empty
            , avroRecordProps = Map.empty
            }
      assertBool "fingerprints differ when namespaces differ"
        (avroFingerprint (mk "a.b") /= avroFingerprint (mk "x.y"))
  ]

aliasResolutionTests :: TestTree
aliasResolutionTests = testGroup "Alias-aware resolution"
  [ testCase "reader field alias matches writer field name" $ do
      let writerTy = mkRecordType "Rec"
                       [ ("old_name", AvroPrimitive AvroInt) ]
          readerTy = AvroRecord
            { avroRecordName = "Rec"
            , avroRecordNamespace = Nothing
            , avroRecordDoc = Nothing
            , avroRecordAliases = V.empty
            , avroRecordProps = Map.empty
            , avroRecordFields = V.fromList
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
        Left err -> assertFailure $ "resolveSchema failed: " ++ err
        Right res -> resolveValue res writerVal @?= Right (AV.Record (V.fromList [AV.Int 42]))

  , testCase "writer field alias matches reader field name" $ do
      let writerTy = AvroRecord
            { avroRecordName = "Rec"
            , avroRecordNamespace = Nothing
            , avroRecordDoc = Nothing
            , avroRecordAliases = V.empty
            , avroRecordProps = Map.empty
            , avroRecordFields = V.fromList
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
          readerTy = mkRecordType "Rec"
                       [ ("new_name", AvroPrimitive AvroInt) ]
          writerVal = AV.Record (V.fromList [AV.Int 99])
      case resolveSchema writerTy readerTy of
        Left err -> assertFailure $ "resolveSchema failed: " ++ err
        Right res -> resolveValue res writerVal @?= Right (AV.Record (V.fromList [AV.Int 99]))

  , testCase "exact name match still works" $ do
      let writerTy = mkRecordType "Rec" [("x", AvroPrimitive AvroInt)]
          readerTy = mkRecordType "Rec" [("x", AvroPrimitive AvroInt)]
      resolveSchema writerTy readerTy @?= Right ResolvedSame
  ]

-- Helpers

roundtrip :: (MonadTest m) => AvroType -> AV.Value -> m ()
roundtrip ty val =
  decodeAvro ty (encodeAvro ty val) === Right val

mkRecordType :: Text -> [(Text, AvroType)] -> AvroType
mkRecordType name fields = AvroRecord
  { avroRecordName      = name
  , avroRecordNamespace = Nothing
  , avroRecordDoc       = Nothing
  , avroRecordAliases   = V.empty
  , avroRecordProps     = Map.empty
  , avroRecordFields    = V.fromList
      [ AvroField
          { avroFieldName    = fname
          , avroFieldType    = ftype
          , avroFieldDefault = Nothing
          , avroFieldOrder   = Nothing
          , avroFieldAliases = V.empty
          , avroFieldDoc     = Nothing
          , avroFieldProps   = Map.empty
          }
      | (fname, ftype) <- fields
      ]
  }

mkRecordTypeWithDefaults :: Text -> [(Text, AvroType, Maybe Aeson.Value)] -> AvroType
mkRecordTypeWithDefaults name fields = AvroRecord
  { avroRecordName      = name
  , avroRecordNamespace = Nothing
  , avroRecordDoc       = Nothing
  , avroRecordAliases   = V.empty
  , avroRecordProps     = Map.empty
  , avroRecordFields    = V.fromList
      [ AvroField
          { avroFieldName    = fname
          , avroFieldType    = ftype
          , avroFieldDefault = dflt
          , avroFieldOrder   = Nothing
          , avroFieldAliases = V.empty
          , avroFieldDoc     = Nothing
          , avroFieldProps   = Map.empty
          }
      | (fname, ftype, dflt) <- fields
      ]
  }

--------------------------------------------------------------------------------
-- Avro spec conformance vectors (from the Apache Avro specification)
-- Exact byte-level tests for all primitive types.
--------------------------------------------------------------------------------

avroSpecConformanceVectors :: TestTree
avroSpecConformanceVectors = testGroup "Avro spec conformance vectors"
  [ testGroup "null"
      [ testCase "null encodes to 0 bytes" $
          encodeAvro (AvroPrimitive AvroNull) AV.Null @?= BS.empty
      , testCase "null decodes from 0 bytes" $
          decodeAvro (AvroPrimitive AvroNull) BS.empty @?= Right AV.Null
      ]

  , testGroup "boolean"
      [ testCase "true = [0x01]" $
          encodeAvro (AvroPrimitive AvroBool) (AV.Bool True) @?= BS.pack [0x01]
      , testCase "false = [0x00]" $
          encodeAvro (AvroPrimitive AvroBool) (AV.Bool False) @?= BS.pack [0x00]
      , testCase "decode true" $
          decodeAvro (AvroPrimitive AvroBool) (BS.pack [0x01]) @?= Right (AV.Bool True)
      , testCase "decode false" $
          decodeAvro (AvroPrimitive AvroBool) (BS.pack [0x00]) @?= Right (AV.Bool False)
      ]

  , testGroup "int (zigzag)"
      [ testCase "0 = [0x00]" $
          encodeAvro (AvroPrimitive AvroInt) (AV.Int 0) @?= BS.pack [0x00]
      , testCase "-1 = [0x01]" $
          encodeAvro (AvroPrimitive AvroInt) (AV.Int (-1)) @?= BS.pack [0x01]
      , testCase "1 = [0x02]" $
          encodeAvro (AvroPrimitive AvroInt) (AV.Int 1) @?= BS.pack [0x02]
      , testCase "-2 = [0x03]" $
          encodeAvro (AvroPrimitive AvroInt) (AV.Int (-2)) @?= BS.pack [0x03]
      , testCase "2 = [0x04]" $
          encodeAvro (AvroPrimitive AvroInt) (AV.Int 2) @?= BS.pack [0x04]
      , testCase "-64 = [0x7f]" $
          encodeAvro (AvroPrimitive AvroInt) (AV.Int (-64)) @?= BS.pack [0x7f]
      , testCase "64 = [0x80, 0x01]" $
          encodeAvro (AvroPrimitive AvroInt) (AV.Int 64) @?= BS.pack [0x80, 0x01]
      , testCase "max int32 = 5 bytes" $ do
          let bs = encodeAvro (AvroPrimitive AvroInt) (AV.Int maxBound)
          BS.length bs @?= 5
          decodeAvro (AvroPrimitive AvroInt) bs @?= Right (AV.Int maxBound)
      , testCase "min int32 = 5 bytes" $ do
          let bs = encodeAvro (AvroPrimitive AvroInt) (AV.Int minBound)
          BS.length bs @?= 5
          decodeAvro (AvroPrimitive AvroInt) bs @?= Right (AV.Int minBound)
      ]

  , testGroup "long (zigzag)"
      [ testCase "long 0 = [0x00]" $
          encodeAvro (AvroPrimitive AvroLong) (AV.Long 0) @?= BS.pack [0x00]
      , testCase "long -1 = [0x01]" $
          encodeAvro (AvroPrimitive AvroLong) (AV.Long (-1)) @?= BS.pack [0x01]
      , testCase "long 1 = [0x02]" $
          encodeAvro (AvroPrimitive AvroLong) (AV.Long 1) @?= BS.pack [0x02]
      , testCase "long max roundtrip" $ do
          let ty = AvroPrimitive AvroLong
              val = AV.Long maxBound
          decodeAvro ty (encodeAvro ty val) @?= Right val
      , testCase "long min roundtrip" $ do
          let ty = AvroPrimitive AvroLong
              val = AV.Long minBound
          decodeAvro ty (encodeAvro ty val) @?= Right val
      , testCase "long 90071992547409917" $
          encodeAvro (AvroPrimitive AvroLong) (AV.Long 90071992547409917)
            @?= BS.pack [0xfa, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x02]
      ]

  , testGroup "float (IEEE 754 LE)"
      [ testCase "float 0.0 = [0,0,0,0]" $
          encodeAvro (AvroPrimitive AvroFloat) (AV.Float 0.0)
            @?= BS.pack [0, 0, 0, 0]
      , testCase "float 1.0 = [0x00,0x00,0x80,0x3f]" $
          encodeAvro (AvroPrimitive AvroFloat) (AV.Float 1.0)
            @?= BS.pack [0x00, 0x00, 0x80, 0x3f]
      , testCase "float roundtrip -3.14" $ do
          let ty = AvroPrimitive AvroFloat
              val = AV.Float (-3.14)
          decodeAvro ty (encodeAvro ty val) @?= Right val
      ]

  , testGroup "double (IEEE 754 LE)"
      [ testCase "double 0.0 = 8 zero bytes" $
          encodeAvro (AvroPrimitive AvroDouble) (AV.Double 0.0)
            @?= BS.pack [0,0,0,0,0,0,0,0]
      , testCase "double 1.0" $
          encodeAvro (AvroPrimitive AvroDouble) (AV.Double 1.0)
            @?= BS.pack [0, 0, 0, 0, 0, 0, 240, 63]
      , testCase "double -2.0" $
          encodeAvro (AvroPrimitive AvroDouble) (AV.Double (-2.0))
            @?= BS.pack [0, 0, 0, 0, 0, 0, 0, 192]
      , testCase "double 0.89" $
          encodeAvro (AvroPrimitive AvroDouble) (AV.Double 0.89)
            @?= BS.pack [123, 20, 174, 71, 225, 122, 236, 63]
      ]

  , testGroup "string"
      [ testCase "\"foo\" = [0x06, 0x66, 0x6f, 0x6f]" $
          encodeAvro (AvroPrimitive AvroString) (AV.String "foo")
            @?= BS.pack [0x06, 0x66, 0x6f, 0x6f]
      , testCase "empty string = [0x00]" $
          encodeAvro (AvroPrimitive AvroString) (AV.String "")
            @?= BS.pack [0x00]
      , testCase "\"a\" = [0x02, 0x61]" $
          encodeAvro (AvroPrimitive AvroString) (AV.String "a")
            @?= BS.pack [0x02, 0x61]
      ]

  , testGroup "bytes"
      [ testCase "[0xDE, 0xAD] = [0x04, 0xDE, 0xAD]" $
          encodeAvro (AvroPrimitive AvroBytes) (AV.Bytes (BS.pack [0xDE, 0xAD]))
            @?= BS.pack [0x04, 0xDE, 0xAD]
      , testCase "empty bytes = [0x00]" $
          encodeAvro (AvroPrimitive AvroBytes) (AV.Bytes BS.empty)
            @?= BS.pack [0x00]
      ]

  , testGroup "array block encoding"
      [ testCase "empty array = [0x00]" $
          encodeAvro (AvroArray (AvroPrimitive AvroInt)) (AV.Array V.empty)
            @?= BS.pack [0x00]
      , testCase "[1,2,3] = [count=6, zz(1)=2, zz(2)=4, zz(3)=6, 0]" $
          encodeAvro (AvroArray (AvroPrimitive AvroInt))
            (AV.Array (V.fromList [AV.Int 1, AV.Int 2, AV.Int 3]))
            @?= BS.pack [0x06, 0x02, 0x04, 0x06, 0x00]
      , testCase "single element [42]" $ do
          let ty = AvroArray (AvroPrimitive AvroInt)
              val = AV.Array (V.fromList [AV.Int 42])
          decodeAvro ty (encodeAvro ty val) @?= Right val
      , testCase "many elements [0..99]" $ do
          let ty = AvroArray (AvroPrimitive AvroInt)
              val = AV.Array (V.fromList [AV.Int i | i <- [0..99]])
          decodeAvro ty (encodeAvro ty val) @?= Right val
      ]

  , testGroup "map block encoding"
      [ testCase "empty map = [0x00]" $
          encodeAvro (AvroMap (AvroPrimitive AvroInt)) (AV.Map V.empty)
            @?= BS.pack [0x00]
      , testCase "{\"a\": 1} = [0x02, 0x02, 0x61, 0x02, 0x00]" $
          encodeAvro (AvroMap (AvroPrimitive AvroInt))
            (AV.Map (V.fromList [("a", AV.Int 1)]))
            @?= BS.pack [0x02, 0x02, 0x61, 0x02, 0x00]
      , testCase "single entry roundtrip" $ do
          let ty = AvroMap (AvroPrimitive AvroString)
              val = AV.Map (V.fromList [("key", AV.String "value")])
          decodeAvro ty (encodeAvro ty val) @?= Right val
      ]

  , testGroup "union index encoding"
      [ testCase "null first in [null, string] = [0x00]" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
          encodeAvro ty (AV.Union 0 AV.Null) @?= BS.pack [0x00]
      , testCase "string second in [null, string] starts with [0x02]" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              bs = encodeAvro ty (AV.Union 1 (AV.String "hi"))
          BS.index bs 0 @?= 0x02
      , testCase "null third in [string, bool, null] = [0x04]" $ do
          let ty = AvroUnion (V.fromList
                     [ AvroPrimitive AvroString
                     , AvroPrimitive AvroBool
                     , AvroPrimitive AvroNull
                     ])
          encodeAvro ty (AV.Union 2 AV.Null) @?= BS.pack [0x04]
      ]

  , testGroup "fixed encoding"
      [ testCase "fixed 4 bytes" $ do
          let ty = AvroFixed "F4" Nothing 4 V.empty
              val = AV.Fixed (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])
          encodeAvro ty val @?= BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
          decodeAvro ty (encodeAvro ty val) @?= Right val
      , testCase "fixed 0 bytes" $ do
          let ty = AvroFixed "F0" Nothing 0 V.empty
              val = AV.Fixed BS.empty
          encodeAvro ty val @?= BS.empty
          decodeAvro ty (encodeAvro ty val) @?= Right val
      ]

  , testGroup "enum encoding"
      [ testCase "enum ordinal 0 = [0x00]" $ do
          let syms = V.fromList ["A", "B", "C"]
              ty = AvroEnum "E" Nothing Nothing V.empty syms Nothing
          encodeAvro ty (AV.Enum 0) @?= BS.pack [0x00]
      , testCase "enum ordinal 1 = [0x02]" $ do
          let syms = V.fromList ["A", "B", "C"]
              ty = AvroEnum "E" Nothing Nothing V.empty syms Nothing
          encodeAvro ty (AV.Enum 1) @?= BS.pack [0x02]
      ]

  , testGroup "nested record"
      [ testCase "3-level nested record roundtrip" $ do
          let innerTy = mkRecordType "Inner" [("val", AvroPrimitive AvroInt)]
              midTy = mkRecordType "Mid" [("inner", innerTy), ("label", AvroPrimitive AvroString)]
              outerTy = mkRecordType "Outer" [("mid", midTy), ("flag", AvroPrimitive AvroBool)]
              innerVal = AV.Record (V.fromList [AV.Int 42])
              midVal = AV.Record (V.fromList [innerVal, AV.String "hello"])
              outerVal = AV.Record (V.fromList [midVal, AV.Bool True])
          decodeAvro outerTy (encodeAvro outerTy outerVal) @?= Right outerVal
      ]
  ]
