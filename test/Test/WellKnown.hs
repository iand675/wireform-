module Test.WellKnown (wellKnownTests) where

import qualified Data.ByteString as BS
import Data.Hashable (hash, hashWithSalt)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as AesonKM
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Proto.Encode
import Proto.Decode
import qualified Data.Aeson as Aeson
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Any
import Proto.Google.Protobuf.Any.Util
import Proto.Google.Protobuf.Empty
import Proto.Google.Protobuf.Wrappers
import Proto.Google.Protobuf.FieldMask
import Proto.Google.Protobuf.SourceContext
import Proto.Google.Protobuf.Struct
import Proto.Message (IsMessage(..))

wellKnownTests :: TestTree
wellKnownTests = testGroup "Well-Known Types"
  [ testGroup "Timestamp"
      [ testProperty "roundtrip" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear (-1000000000) 1000000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let msg = defaultTimestamp { timestampSeconds = s, timestampNanos = n }
              encoded = encodeMessage msg
          decodeMessage encoded === Right msg

      , testCase "default is zero-length" $ do
          let encoded = encodeMessage defaultTimestamp
          BS.length encoded @?= 0

      , testCase "sized encoding matches" $ do
          let msg = defaultTimestamp { timestampSeconds = 1234567890, timestampNanos = 123456789 }
          BS.length (encodeMessage msg) @?= messageSize msg

      , testCase "JSON canonical RFC 3339" $ do
          let msg = defaultTimestamp { timestampSeconds = 1708000000, timestampNanos = 0 }
          Aeson.toJSON msg @?= Aeson.String "2024-02-15T12:26:40Z"

      , testCase "JSON with nanos" $ do
          let msg = defaultTimestamp { timestampSeconds = 0, timestampNanos = 123456789 }
          Aeson.toJSON msg @?= Aeson.String "1970-01-01T00:00:00.123456789Z"

      , testCase "JSON nanos trailing zeros trimmed" $ do
          let msg = defaultTimestamp { timestampSeconds = 0, timestampNanos = 100000000 }
          Aeson.toJSON msg @?= Aeson.String "1970-01-01T00:00:00.1Z"

      , testCase "JSON parses pre-epoch (negative) timestamps" $ do
          -- 1969-12-31T23:59:00Z = -60 seconds.
          case Aeson.fromJSON (Aeson.String "1969-12-31T23:59:00Z") :: Aeson.Result Timestamp of
            Aeson.Success ts -> do
              timestampSeconds ts @?= -60
              timestampNanos   ts @?= 0
            Aeson.Error e -> assertFailure e

      , testCase "JSON parses RFC 3339 numeric timezone offset" $ do
          -- 12:00:00+05:00 == 07:00:00Z
          case Aeson.fromJSON (Aeson.String "2024-01-01T12:00:00+05:00") :: Aeson.Result Timestamp of
            Aeson.Success ts -> do
              -- 2024-01-01T07:00:00Z
              case Aeson.fromJSON (Aeson.String "2024-01-01T07:00:00Z") :: Aeson.Result Timestamp of
                Aeson.Success refTs -> timestampSeconds ts @?= timestampSeconds refTs
                _ -> assertFailure "ref parse failed"
            Aeson.Error e -> assertFailure e

      , testCase "JSON parses RFC 3339 negative timezone offset" $ do
          -- 02:00:00-03:00 == 05:00:00Z
          case Aeson.fromJSON (Aeson.String "2024-01-01T02:00:00-03:00") :: Aeson.Result Timestamp of
            Aeson.Success ts -> do
              case Aeson.fromJSON (Aeson.String "2024-01-01T05:00:00Z") :: Aeson.Result Timestamp of
                Aeson.Success refTs -> timestampSeconds ts @?= timestampSeconds refTs
                _ -> assertFailure "ref parse failed"
            Aeson.Error e -> assertFailure e
      ]

  , testGroup "Duration"
      [ testProperty "roundtrip" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear (-315576000000) 315576000000)
          n <- forAll $ Gen.int32 (Range.linear (-999999999) 999999999)
          let msg = defaultDuration { durationSeconds = s, durationNanos = n }
              encoded = encodeMessage msg
          decodeMessage encoded === Right msg

      , testCase "JSON canonical seconds" $ do
          let msg = defaultDuration { durationSeconds = 3600, durationNanos = 0 }
          Aeson.toJSON msg @?= Aeson.String "3600s"

      , testCase "JSON with nanos" $ do
          let msg = defaultDuration { durationSeconds = 1, durationNanos = 500000000 }
          Aeson.toJSON msg @?= Aeson.String "1.5s"

      , testCase "JSON negative" $ do
          let msg = defaultDuration { durationSeconds = -1, durationNanos = -500000000 }
          Aeson.toJSON msg @?= Aeson.String "-1.5s"
      ]

  , testGroup "Any"
      [ testProperty "raw Any roundtrip" $ property $ do
          tu <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
          v <- forAll $ Gen.bytes (Range.linear 0 200)
          let msg = defaultAny { anyTypeUrl = tu, anyValue = v }
              encoded = encodeMessage msg
          decodeMessage encoded === Right msg

      , testProperty "packAny Timestamp roundtrip" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 2000000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let ts = defaultTimestamp { timestampSeconds = s, timestampNanos = n }
              packed = packAny ts
          anyTypeUrl packed === "type.googleapis.com/google.protobuf.Timestamp"
          case unpackAny packed of
            Just (Right decoded) -> decoded === ts
            Just (Left err) -> do annotate (show err); failure
            Nothing -> do annotate "type mismatch"; failure

      , testProperty "packAny Duration roundtrip" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 1000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let dur = defaultDuration { durationSeconds = s, durationNanos = n }
              packed = packAny dur
          case unpackAny packed of
            Just (Right decoded) -> decoded === dur
            _ -> failure

      , testCase "packAny Empty" $ do
          let packed = packAny defaultEmpty
          anyTypeUrl packed @?= "type.googleapis.com/google.protobuf.Empty"
          case unpackAny packed :: Maybe (Either DecodeError Empty) of
            Just (Right _) -> pure ()
            other -> assertFailure ("Expected Just (Right Empty), got " <> show other)

      , testCase "unpackAny type mismatch returns Nothing" $ do
          let packed = packAny (defaultTimestamp { timestampSeconds = 100 })
          case unpackAny packed :: Maybe (Either DecodeError Duration) of
            Nothing -> pure ()
            Just _  -> assertFailure "Should not match Duration"

      , testCase "isMessageType" $ do
          let packed = packAny (defaultDuration { durationSeconds = 60 })
          isMessageType (Proxy :: Proxy Duration) packed @?= True
          isMessageType (Proxy :: Proxy Timestamp) packed @?= False

      , testCase "typeNameFromUrl strips prefix" $ do
          typeNameFromUrl "type.googleapis.com/google.protobuf.Timestamp"
            @?= "google.protobuf.Timestamp"
          typeNameFromUrl "mycompany.com/types/example.Foo"
            @?= "example.Foo"
          typeNameFromUrl "google.protobuf.Timestamp"
            @?= "google.protobuf.Timestamp"

      , testCase "packAnyWithPrefix custom prefix" $ do
          let packed = packAnyWithPrefix "myhost/" (defaultTimestamp { timestampSeconds = 1 })
          anyTypeUrl packed @?= "myhost/google.protobuf.Timestamp"
          case unpackAny packed of
            Just (Right ts) -> ts @?= defaultTimestamp { timestampSeconds = 1 }
            _ -> assertFailure "Should unpack with any prefix"

      , testCase "Any wire roundtrip preserves content" $ do
          let ts = defaultTimestamp { timestampSeconds = 1234567890, timestampNanos = 500000000 }
              packed = packAny ts
              encodedAny = encodeMessage packed
          case decodeMessage encodedAny of
            Left err -> assertFailure (show err)
            Right decodedAny -> case unpackAny decodedAny of
              Just (Right (decoded :: Timestamp)) -> decoded @?= ts
              _ -> assertFailure "Should unpack after wire roundtrip"

      , testCase "TypeRegistry dynamic dispatch" $ do
          let registry = registerType (Proxy :: Proxy Timestamp)
                       . registerType (Proxy :: Proxy Duration)
                       . registerType (Proxy :: Proxy Empty)
                       $ emptyRegistry
          case unpackAnyDynamic registry (packAny (defaultTimestamp { timestampSeconds = 42 })) of
            Just (Right (DynamicMessage _)) -> pure ()
            _ -> assertFailure "Should unpack Timestamp dynamically"
          case unpackAnyDynamic registry (packAny (defaultDuration { durationSeconds = 60 })) of
            Just (Right (DynamicMessage _)) -> pure ()
            _ -> assertFailure "Should unpack Duration dynamically"

      , testCase "TypeRegistry unknown type returns Nothing" $ do
          let registry = registerType (Proxy :: Proxy Timestamp) emptyRegistry
              unknownAny = defaultAny { anyTypeUrl = "type.googleapis.com/unknown.Type" }
          case unpackAnyDynamic registry unknownAny of
            Nothing -> pure ()
            Just _  -> assertFailure "Should return Nothing for unknown type"
      ]

  , testGroup "Empty"
      [ testCase "empty roundtrip" $ do
          let encoded = encodeMessage defaultEmpty
          BS.length encoded @?= 0
          case decodeMessage encoded :: Either DecodeError Empty of
            Right _ -> pure ()
            Left e  -> assertFailure (show e)
      ]

  , testGroup "Wrappers"
      [ testProperty "Int64Value roundtrip" $ property $ do
          v <- forAll $ Gen.int64 Range.linearBounded
          let msg = defaultInt64Value { int64ValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "UInt64Value roundtrip" $ property $ do
          v <- forAll $ Gen.word64 (Range.linear 0 maxBound)
          let msg = defaultUInt64Value { uInt64ValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "Int32Value roundtrip" $ property $ do
          v <- forAll $ Gen.int32 Range.linearBounded
          let msg = defaultInt32Value { int32ValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "BoolValue roundtrip" $ property $ do
          v <- forAll Gen.bool
          let msg = defaultBoolValue { boolValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "StringValue roundtrip" $ property $ do
          v <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
          let msg = defaultStringValue { stringValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "BytesValue roundtrip" $ property $ do
          v <- forAll $ Gen.bytes (Range.linear 0 200)
          let msg = defaultBytesValue { bytesValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "DoubleValue roundtrip" $ property $ do
          v <- forAll $ Gen.double (Range.linearFrac (-1e100) 1e100)
          let msg = defaultDoubleValue { doubleValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "FloatValue roundtrip" $ property $ do
          v <- forAll $ Gen.float (Range.linearFrac (-1e30) 1e30)
          let msg = defaultFloatValue { floatValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      -- proto3 JSON canonical mapping for wrappers: the wrapper message
      -- maps to the JSON value of its inner field (number / string /
      -- bool / base64), NOT to a {"value": ...} object.
      , testCase "Int32Value JSON is bare number" $
          Aeson.toJSON (defaultInt32Value { int32ValueValue = 42 })
            @?= Aeson.toJSON (42 :: Int)

      , testCase "Int32Value JSON parses bare number" $
          case Aeson.fromJSON (Aeson.Number 42) :: Aeson.Result Int32Value of
            Aeson.Success v -> int32ValueValue v @?= 42
            Aeson.Error e   -> assertFailure e

      , testCase "Int64Value JSON is string-encoded number" $
          Aeson.toJSON (defaultInt64Value { int64ValueValue = 9007199254740993 })
            @?= Aeson.String "9007199254740993"

      , testCase "Int64Value JSON parses string and number" $ do
          case Aeson.fromJSON (Aeson.String "12345") :: Aeson.Result Int64Value of
            Aeson.Success v -> int64ValueValue v @?= 12345
            Aeson.Error e   -> assertFailure e
          case Aeson.fromJSON (Aeson.Number 12345) :: Aeson.Result Int64Value of
            Aeson.Success v -> int64ValueValue v @?= 12345
            Aeson.Error e   -> assertFailure e

      , testCase "BoolValue JSON is bare bool" $
          Aeson.toJSON (defaultBoolValue { boolValueValue = True })
            @?= Aeson.Bool True

      , testCase "StringValue JSON is bare string" $
          Aeson.toJSON (defaultStringValue { stringValueValue = "hello" })
            @?= Aeson.String "hello"

      , testCase "BytesValue JSON is base64 string" $ do
          let msg = defaultBytesValue { bytesValueValue = BS.pack [0x00, 0xff, 0x10] }
          Aeson.toJSON msg @?= Aeson.String "AP8Q"
      ]

  , testGroup "Struct / Value JSON canonical form"
      -- proto3 JSON: Struct == JSON object, Value == raw JSON value of
      -- its discriminated kind, ListValue == JSON array.
      [ testCase "Empty serialises as {}" $
          Aeson.toJSON defaultEmpty @?= Aeson.Object mempty

      , testCase "Value(Number) serialises as bare JSON number" $ do
          let v = defaultValue
                    { valueKind = Just (Value'Kind'NumberValue 3.14) }
          Aeson.toJSON v @?= Aeson.toJSON (3.14 :: Double)

      , testCase "Value(Bool) serialises as bare JSON bool" $ do
          let v = defaultValue
                    { valueKind = Just (Value'Kind'BoolValue True) }
          Aeson.toJSON v @?= Aeson.Bool True

      , testCase "Value(String) serialises as bare JSON string" $ do
          let v = defaultValue
                    { valueKind = Just (Value'Kind'StringValue "hi") }
          Aeson.toJSON v @?= Aeson.String "hi"

      , testCase "Value(Null) serialises as JSON null" $ do
          let v = defaultValue
                    { valueKind = Just
                                    (Value'Kind'NullValue
                                       NullValue'NullValue) }
          Aeson.toJSON v @?= Aeson.Null

      , testCase "Value parses bare JSON number into NumberValue" $
          case Aeson.fromJSON (Aeson.Number 7) :: Aeson.Result Value of
            Aeson.Success v ->
              valueKind v @?= Just (Value'Kind'NumberValue 7.0)
            Aeson.Error e -> assertFailure e

      , testCase "Struct serialises as JSON object of inner Values" $ do
          let s = defaultStruct
                    { structFields =
                        Map.fromList
                          [ ( "n"
                            , defaultValue
                                { valueKind =
                                    Just (Value'Kind'NumberValue 1.0) }
                            )
                          , ( "s"
                            , defaultValue
                                { valueKind =
                                    Just (Value'Kind'StringValue "x") }
                            )
                          ]
                    }
          Aeson.toJSON s
            @?= Aeson.Object (AesonKM.fromList
                  [ ("n", Aeson.toJSON (1.0 :: Double))
                  , ("s", Aeson.String "x")
                  ])
      ]

  , testGroup "FieldMask"
      [ testCase "roundtrip" $ do
          let msg = defaultFieldMask { fieldMaskPaths = V.fromList ["foo.bar", "baz"] }
              encoded = encodeMessage msg
          decodeMessage encoded @?= Right msg
      ]

  , testGroup "SourceContext"
      [ testProperty "roundtrip" $ property $ do
          fn <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
          let msg = defaultSourceContext { sourceContextFileName = fn }
          decodeMessage (encodeMessage msg) === Right msg
      ]

  , testGroup "Struct"
      [ testCase "empty struct roundtrip" $ do
          let msg = defaultStruct
          decodeMessage (encodeMessage msg) @?= Right msg

      , testCase "value null roundtrip" $ do
          let v = defaultValue { valueKind = Just (Value'Kind'NullValue NullValue'NullValue) }
          decodeMessage (encodeMessage v) @?= Right v

      , testCase "value number roundtrip" $ do
          let v = defaultValue { valueKind = Just (Value'Kind'NumberValue 3.14) }
          decodeMessage (encodeMessage v) @?= Right v

      , testCase "value string roundtrip" $ do
          let v = defaultValue { valueKind = Just (Value'Kind'StringValue "hello") }
          decodeMessage (encodeMessage v) @?= Right v

      , testCase "value bool roundtrip" $ do
          let v = defaultValue { valueKind = Just (Value'Kind'BoolValue True) }
          decodeMessage (encodeMessage v) @?= Right v

      , testCase "list value roundtrip" $ do
          let lv = defaultListValue { listValueValues = V.fromList
                [ defaultValue { valueKind = Just (Value'Kind'NumberValue 1) }
                , defaultValue { valueKind = Just (Value'Kind'StringValue "two") }
                , defaultValue { valueKind = Just (Value'Kind'BoolValue False) }
                ] }
          decodeMessage (encodeMessage lv) @?= Right lv
      ]

  , testGroup "Exact-size encoding"
      [ testProperty "encodeMessageSized matches encodeMessage for Timestamp" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 1000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999)
          let msg = defaultTimestamp { timestampSeconds = s, timestampNanos = n }
          encodeMessageSized msg === encodeMessage msg

      , testProperty "encodeMessageSized matches encodeMessage for Duration" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 1000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999)
          let msg = defaultDuration { durationSeconds = s, durationNanos = n }
          encodeMessageSized msg === encodeMessage msg
      ]

  , testGroup "Hashable instances"
      [ testProperty "Timestamp: equal values have equal hashes" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 1000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999)
          let msg = defaultTimestamp { timestampSeconds = s, timestampNanos = n }
          hash msg === hash msg

      , testCase "Timestamp: different values have different hashes" $ do
          let m1 = defaultTimestamp { timestampSeconds = 100 }
              m2 = defaultTimestamp { timestampSeconds = 200 }
          assertBool "hashes should differ" (hash m1 /= hash m2)

      , testCase "Duration: hashWithSalt works" $ do
          let msg = defaultDuration { durationSeconds = 42, durationNanos = 123 }
          hashWithSalt 0 msg `seq` pure ()

      , testCase "Empty: hashable" $ do
          hash defaultEmpty `seq` pure ()

      , testProperty "FieldMask: hashable with vector field" $ property $ do
          ps <- forAll $ Gen.list (Range.linear 0 5) (Gen.text (Range.linear 1 10) Gen.alphaNum)
          let msg = defaultFieldMask { fieldMaskPaths = V.fromList ps }
          hash msg `seq` pure ()

      , testProperty "Struct: hashable with map field" $ property $ do
          hash defaultStruct `seq` pure ()
      ]
  ]
