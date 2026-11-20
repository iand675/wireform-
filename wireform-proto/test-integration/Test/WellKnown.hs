module Test.WellKnown (wellKnownTests) where

import Control.Exception (ErrorCall, evaluate)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Hashable (hash, hashWithSalt)
import Data.Proxy (Proxy (..))
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proto.Decode
import Proto.Encode
import Proto.Google.Protobuf.Any
import Proto.Google.Protobuf.Any.Util
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Empty
import Proto.Google.Protobuf.FieldMask
import Proto.Google.Protobuf.SourceContext
import Proto.Google.Protobuf.Struct
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Wrappers
import Proto.Internal.JSON.WellKnown (durationFromJSON, durationToJSON, timestampFromJSON, timestampToJSON)
import Proto.Registry (TypeRegistry, emptyRegistry, lookupCodec, lookupDecoder, registerMessage)
import Test.Syd
import Test.Syd.Hedgehog ()


wellKnownTests :: Spec
wellKnownTests =
  describe
    "Well-Known Types"
    $ sequence_
      [ describe
          "Timestamp"
          $ sequence_
            [ it "roundtrip" $ property $ do
                s <- forAll $ Gen.int64 (Range.linear (-1000000000) 1000000000)
                n <- forAll $ Gen.int32 (Range.linear 0 999999999)
                let msg = defaultTimestamp {timestampSeconds = s, timestampNanos = n}
                    encoded = encodeMessage msg
                decodeMessage encoded === Right msg
            , it "default is zero-length" $ do
                let encoded = encodeMessage defaultTimestamp
                BS.length encoded `shouldBe` 0
            , it "sized encoding matches" $ do
                let msg = defaultTimestamp {timestampSeconds = 1234567890, timestampNanos = 123456789}
                BS.length (encodeMessage msg) `shouldBe` messageSize msg
            , it "JSON canonical RFC 3339" $ do
                let msg = defaultTimestamp {timestampSeconds = 1708000000, timestampNanos = 0}
                Aeson.toJSON msg `shouldBe` Aeson.String "2024-02-15T12:26:40Z"
            , it "JSON with nanos" $ do
                let msg = defaultTimestamp {timestampSeconds = 0, timestampNanos = 123456789}
                Aeson.toJSON msg `shouldBe` Aeson.String "1970-01-01T00:00:00.123456789Z"
            , it "JSON nanos use canonical fractional precision" $ do
                let msg = defaultTimestamp {timestampSeconds = 0, timestampNanos = 100000000}
                Aeson.toJSON msg `shouldBe` Aeson.String "1970-01-01T00:00:00.100Z"
            , it "ProtoJSON roundtrips pre-epoch instants and normalizes offsets" $ do
                let msg = defaultTimestamp {timestampSeconds = -1, timestampNanos = 100000000}
                    canonical = Aeson.String "1969-12-31T23:59:59.100Z"
                    offset = Aeson.String "1970-01-01T00:59:59.1+01:00"
                timestampToJSON msg `shouldBe` canonical
                timestampFromJSON offset `shouldBe` Right msg
                Aeson.toJSON msg `shouldBe` canonical
                Aeson.fromJSON offset `shouldBe` Aeson.Success msg
            , it "ProtoJSON rejects invalid calendar dates, times, fractions and offsets" $
                mapM_ (\input -> do
                  let value = Aeson.String input
                  isLeft (timestampFromJSON value) `shouldBe` True
                  case Aeson.fromJSON @Timestamp value of
                    Aeson.Error _ -> pure ()
                    Aeson.Success _ -> expectationFailure ("Accepted " <> show input))
                  [ "2023-02-29T00:00:00Z"
                  , "2024-04-31T00:00:00Z"
                  , "2026-11-10T24:00:00Z"
                  , "2026-11-10T00:00:00.1junkZ"
                  , "2026-11-10T00:00:00+00:99"
                  , "0001-01-01T00:00:00+00:01"
                  , "9999-12-31T23:59:59-00:01"
                  ]
            ]
      , describe
          "Duration"
          $ sequence_
            [ it "roundtrip" $ property $ do
                s <- forAll $ Gen.int64 (Range.linear (-315576000000) 315576000000)
                n <- forAll $ Gen.int32 (Range.linear (-999999999) 999999999)
                let msg = defaultDuration {durationSeconds = s, durationNanos = n}
                    encoded = encodeMessage msg
                decodeMessage encoded === Right msg
            , it "JSON canonical seconds" $ do
                let msg = defaultDuration {durationSeconds = 3600, durationNanos = 0}
                Aeson.toJSON msg `shouldBe` Aeson.String "3600s"
            , it "JSON with nanos" $ do
                let msg = defaultDuration {durationSeconds = 1, durationNanos = 500000000}
                Aeson.toJSON msg `shouldBe` Aeson.String "1.500s"
            , it "JSON negative" $ do
                let msg = defaultDuration {durationSeconds = -1, durationNanos = -500000000}
                Aeson.toJSON msg `shouldBe` Aeson.String "-1.500s"
            , it "both Duration JSON paths preserve valid extreme values" $
                mapM_ (\(input, expected) -> do
                  let value = Aeson.String input
                  durationFromJSON value `shouldBe` Right expected
                  Aeson.fromJSON value `shouldBe` Aeson.Success expected
                  Aeson.toJSON expected `shouldBe` value
                  durationToJSON expected `shouldBe` value)
                  [ ("315576000000.999999999s", defaultDuration {durationSeconds = 315576000000, durationNanos = 999999999})
                  , ("-315576000000.999999999s", defaultDuration {durationSeconds = -315576000000, durationNanos = -999999999})
                  , ("-0.250s", defaultDuration {durationNanos = -250000000})
                  ]
            , it "both Duration JSON paths reject malformed and overflowing input" $
                mapM_ (\input -> do
                  let value = Aeson.String input
                  isLeft (durationFromJSON value) `shouldBe` True
                  case Aeson.fromJSON @Duration value of
                    Aeson.Error _ -> pure ()
                    Aeson.Success _ -> expectationFailure ("Accepted " <> show input))
                  ["garbage", "1.25garbages", "18446744073709551616s", "1.s", "1.1234567890s", "315576000001s"]
            , it "both Duration JSON paths reject invalid wire components on serialization" $
                mapM_ (\dur -> do
                  evaluate (durationToJSON dur) `shouldThrow` (\(_ :: ErrorCall) -> True)
                  evaluate (Aeson.toJSON dur) `shouldThrow` (\(_ :: ErrorCall) -> True))
                  [ defaultDuration {durationSeconds = 1, durationNanos = -1}
                  , defaultDuration {durationNanos = 1000000000}
                  , defaultDuration {durationSeconds = 315576000001}
                  ]
            ]
      , describe
          "Any"
          $ sequence_
            [ it "raw Any roundtrip" $ property $ do
                tu <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
                v <- forAll $ Gen.bytes (Range.linear 0 200)
                let msg = defaultAny {anyTypeUrl = tu, anyValue = v}
                    encoded = encodeMessage msg
                decodeMessage encoded === Right msg
            , it "packAny Timestamp roundtrip" $ property $ do
                s <- forAll $ Gen.int64 (Range.linear 0 2000000000)
                n <- forAll $ Gen.int32 (Range.linear 0 999999999)
                let ts = defaultTimestamp {timestampSeconds = s, timestampNanos = n}
                    packed = packAny ts
                anyTypeUrl packed === "type.googleapis.com/google.protobuf.Timestamp"
                case unpackAny packed of
                  Just (Right decoded) -> decoded === ts
                  Just (Left err) -> do annotate (show err); failure
                  Nothing -> do annotate "type mismatch"; failure
            , it "packAny Duration roundtrip" $ property $ do
                s <- forAll $ Gen.int64 (Range.linear 0 1000000)
                n <- forAll $ Gen.int32 (Range.linear 0 999999999)
                let dur = defaultDuration {durationSeconds = s, durationNanos = n}
                    packed = packAny dur
                case unpackAny packed of
                  Just (Right decoded) -> decoded === dur
                  _ -> failure
            , it "packAny Empty" $ do
                let packed = packAny defaultEmpty
                anyTypeUrl packed `shouldBe` "type.googleapis.com/google.protobuf.Empty"
                case unpackAny packed :: Maybe (Either DecodeError Empty) of
                  Just (Right _) -> pure ()
                  other -> expectationFailure ("Expected Just (Right Empty), got " <> show other)
            , it "unpackAny type mismatch returns Nothing" $ do
                let packed = packAny (defaultTimestamp {timestampSeconds = 100})
                case unpackAny packed :: Maybe (Either DecodeError Duration) of
                  Nothing -> pure ()
                  Just _ -> expectationFailure "Should not match Duration"
            , it "isMessageType" $ do
                let packed = packAny (defaultDuration {durationSeconds = 60})
                isMessageType (Proxy :: Proxy Duration) packed `shouldBe` True
                isMessageType (Proxy :: Proxy Timestamp) packed `shouldBe` False
            , it "typeNameFromUrl strips prefix" $ do
                typeNameFromUrl "type.googleapis.com/google.protobuf.Timestamp"
                  `shouldBe` "google.protobuf.Timestamp"
                typeNameFromUrl "mycompany.com/types/example.Foo"
                  `shouldBe` "example.Foo"
                typeNameFromUrl "google.protobuf.Timestamp"
                  `shouldBe` "google.protobuf.Timestamp"
            , it "packAnyWithPrefix custom prefix" $ do
                let packed = packAnyWithPrefix "myhost/" (defaultTimestamp {timestampSeconds = 1})
                anyTypeUrl packed `shouldBe` "myhost/google.protobuf.Timestamp"
                case unpackAny packed of
                  Just (Right ts) -> ts `shouldBe` defaultTimestamp {timestampSeconds = 1}
                  _ -> expectationFailure "Should unpack with any prefix"
            , it "Any wire roundtrip preserves content" $ do
                let ts = defaultTimestamp {timestampSeconds = 1234567890, timestampNanos = 500000000}
                    packed = packAny ts
                    encodedAny = encodeMessage packed
                case decodeMessage encodedAny of
                  Left err -> expectationFailure (show err)
                  Right decodedAny -> case unpackAny decodedAny of
                    Just (Right (decoded :: Timestamp)) -> decoded `shouldBe` ts
                    _ -> expectationFailure "Should unpack after wire roundtrip"
            , it "TypeRegistry codec lookup" $ do
                let registry =
                      registerMessage (Proxy :: Proxy Timestamp)
                        . registerMessage (Proxy :: Proxy Duration)
                        . registerMessage (Proxy :: Proxy Empty)
                        $ emptyRegistry
                case lookupDecoder @Timestamp "google.protobuf.Timestamp" registry of
                  Just decode' -> case decode' (encodeMessage (defaultTimestamp {timestampSeconds = 42})) of
                    Right ts -> timestampSeconds ts `shouldBe` 42
                    Left e -> expectationFailure (show e)
                  Nothing -> expectationFailure "Should find Timestamp decoder"
            , it "TypeRegistry unknown type returns Nothing" $ do
                let registry = registerMessage (Proxy :: Proxy Timestamp) emptyRegistry
                case lookupCodec "unknown.Type" registry of
                  Nothing -> pure ()
                  Just _ -> expectationFailure "Should return Nothing for unknown type"
            ]
      , describe
          "Empty"
          $ sequence_
            [ it "empty roundtrip" $ do
                let encoded = encodeMessage defaultEmpty
                BS.length encoded `shouldBe` 0
                case decodeMessage encoded :: Either DecodeError Empty of
                  Right _ -> pure ()
                  Left e -> expectationFailure (show e)
            ]
      , describe
          "Wrappers"
          $ sequence_
            [ it "Int64Value roundtrip" $ property $ do
                v <- forAll $ Gen.int64 Range.linearBounded
                let msg = defaultInt64Value {int64ValueValue = v}
                decodeMessage (encodeMessage msg) === Right msg
            , it "UInt64Value roundtrip" $ property $ do
                v <- forAll $ Gen.word64 (Range.linear 0 maxBound)
                let msg = defaultUInt64Value {uInt64ValueValue = v}
                decodeMessage (encodeMessage msg) === Right msg
            , it "Int32Value roundtrip" $ property $ do
                v <- forAll $ Gen.int32 Range.linearBounded
                let msg = defaultInt32Value {int32ValueValue = v}
                decodeMessage (encodeMessage msg) === Right msg
            , it "BoolValue roundtrip" $ property $ do
                v <- forAll Gen.bool
                let msg = defaultBoolValue {boolValueValue = v}
                decodeMessage (encodeMessage msg) === Right msg
            , it "StringValue roundtrip" $ property $ do
                v <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
                let msg = defaultStringValue {stringValueValue = v}
                decodeMessage (encodeMessage msg) === Right msg
            , it "BytesValue roundtrip" $ property $ do
                v <- forAll $ Gen.bytes (Range.linear 0 200)
                let msg = defaultBytesValue {bytesValueValue = v}
                decodeMessage (encodeMessage msg) === Right msg
            , it "DoubleValue roundtrip" $ property $ do
                v <- forAll $ Gen.double (Range.linearFrac (-1e100) 1e100)
                let msg = defaultDoubleValue {doubleValueValue = v}
                decodeMessage (encodeMessage msg) === Right msg
            , it "FloatValue roundtrip" $ property $ do
                v <- forAll $ Gen.float (Range.linearFrac (-1e30) 1e30)
                let msg = defaultFloatValue {floatValueValue = v}
                decodeMessage (encodeMessage msg) === Right msg
            ]
      , describe
          "FieldMask"
          $ sequence_
            [ it "roundtrip" $ do
                let msg = defaultFieldMask {fieldMaskPaths = V.fromList ["foo.bar", "baz"]}
                    encoded = encodeMessage msg
                decodeMessage encoded `shouldBe` Right msg
            ]
      , describe
          "SourceContext"
          $ sequence_
            [ it "roundtrip" $ property $ do
                fn <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
                let msg = defaultSourceContext {sourceContextFileName = fn}
                decodeMessage (encodeMessage msg) === Right msg
            ]
      , describe
          "Struct"
          $ sequence_
            [ it "empty struct roundtrip" $ do
                let msg = defaultStruct
                decodeMessage (encodeMessage msg) `shouldBe` Right msg
            , it "value null roundtrip" $ do
                let v = defaultValue {valueKind = Just (Value'Kind'NullValue NullValue'NullValue)}
                decodeMessage (encodeMessage v) `shouldBe` Right v
            , it "value number roundtrip" $ do
                let v = defaultValue {valueKind = Just (Value'Kind'NumberValue 3.14)}
                decodeMessage (encodeMessage v) `shouldBe` Right v
            , it "value string roundtrip" $ do
                let v = defaultValue {valueKind = Just (Value'Kind'StringValue "hello")}
                decodeMessage (encodeMessage v) `shouldBe` Right v
            , it "value bool roundtrip" $ do
                let v = defaultValue {valueKind = Just (Value'Kind'BoolValue True)}
                decodeMessage (encodeMessage v) `shouldBe` Right v
            , it "list value roundtrip" $ do
                let lv =
                      defaultListValue
                        { listValueValues =
                            V.fromList
                              [ defaultValue {valueKind = Just (Value'Kind'NumberValue 1)}
                              , defaultValue {valueKind = Just (Value'Kind'StringValue "two")}
                              , defaultValue {valueKind = Just (Value'Kind'BoolValue False)}
                              ]
                        }
                decodeMessage (encodeMessage lv) `shouldBe` Right lv
            ]
      , describe
          "Exact-size encoding"
          $ sequence_
            [ it "encodeMessageSized matches encodeMessage for Timestamp" $ property $ do
                s <- forAll $ Gen.int64 (Range.linear 0 1000000)
                n <- forAll $ Gen.int32 (Range.linear 0 999999)
                let msg = defaultTimestamp {timestampSeconds = s, timestampNanos = n}
                encodeMessageSized msg === encodeMessage msg
            , it "encodeMessageSized matches encodeMessage for Duration" $ property $ do
                s <- forAll $ Gen.int64 (Range.linear 0 1000000)
                n <- forAll $ Gen.int32 (Range.linear 0 999999)
                let msg = defaultDuration {durationSeconds = s, durationNanos = n}
                encodeMessageSized msg === encodeMessage msg
            ]
      , describe
          "Hashable instances"
          $ sequence_
            [ it "Timestamp: equal values have equal hashes" $ property $ do
                s <- forAll $ Gen.int64 (Range.linear 0 1000000)
                n <- forAll $ Gen.int32 (Range.linear 0 999999)
                let msg = defaultTimestamp {timestampSeconds = s, timestampNanos = n}
                hash msg === hash msg
            , it "Timestamp: different values have different hashes" $ do
                let m1 = defaultTimestamp {timestampSeconds = 100}
                    m2 = defaultTimestamp {timestampSeconds = 200}
                (hash m1 /= hash m2) `shouldBe` True
            , it "Duration: hashWithSalt works" $ do
                let msg = defaultDuration {durationSeconds = 42, durationNanos = 123}
                hashWithSalt 0 msg `seq` pure () :: IO ()
            , it "Empty: hashable" $ do
                hash defaultEmpty `seq` pure () :: IO ()
            , it "FieldMask: hashable with vector field" $ property $ do
                ps <- forAll $ Gen.list (Range.linear 0 5) (Gen.text (Range.linear 1 10) Gen.alphaNum)
                let msg = defaultFieldMask {fieldMaskPaths = V.fromList ps}
                hash msg `seq` pure ()
            , it "Struct: hashable with map field" $ property $ do
                hash defaultStruct `seq` pure ()
            ]
      ]
