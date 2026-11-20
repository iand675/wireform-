module Test.WellKnownUtil (wellKnownUtilTests) where

import Control.Exception (ErrorCall, evaluate)
import Data.Aeson qualified as Aeson
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Duration.Util
import Proto.Google.Protobuf.FieldMask
import Proto.Google.Protobuf.FieldMask.Util
import Proto.Google.Protobuf.Struct
import Proto.Google.Protobuf.Struct.Util
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Timestamp.Util
import Proto.Google.Protobuf.Wrappers
import Proto.Google.Protobuf.Wrappers.Util
import Test.Syd
import Test.Syd.Hedgehog ()


wellKnownUtilTests :: Spec
wellKnownUtilTests =
  describe "Well-Known Type Utilities" $
    sequence_
      [ timestampUtilTests
      , durationUtilTests
      , fieldMaskUtilTests
      , wrappersUtilTests
      , structUtilTests
      ]


-- --------------------------------------------------------------------------
-- Timestamp.Util
-- --------------------------------------------------------------------------

timestampUtilTests :: Spec
timestampUtilTests =
  describe "Timestamp.Util" $
    sequence_
      [ it "POSIXTime roundtrip" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 2000000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let ts = defaultTimestamp {timestampSeconds = s, timestampNanos = n}
              rt = timestampFromPOSIXTime (timestampToPOSIXTime ts)
          timestampSeconds rt === s
          timestampNanos rt === n
      , it "UTCTime roundtrip" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 2000000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let ts = defaultTimestamp {timestampSeconds = s, timestampNanos = n}
              rt = timestampFromUTCTime (timestampToUTCTime ts)
          timestampSeconds rt === s
          timestampNanos rt === n
      , it "epoch is zero" $ do
          let ts = timestampFromPOSIXTime 0
          timestampSeconds ts `shouldBe` 0
          timestampNanos ts `shouldBe` 0
      , it "specific POSIX time" $ do
          let ts = timestampFromPOSIXTime 1708000000.5
          timestampSeconds ts `shouldBe` 1708000000
          timestampNanos ts `shouldBe` 500000000
      , it "normalizes pre-epoch fractions and preserves the latest Timestamp" $ do
          let before = defaultTimestamp {timestampSeconds = -1, timestampNanos = 500000000}
              latest = defaultTimestamp {timestampSeconds = 253402300799, timestampNanos = 999999999}
              earliest = defaultTimestamp {timestampSeconds = -62135596800}
          timestampFromPOSIXTime (-0.5) `shouldBe` before
          timestampFromPOSIXTime (timestampToPOSIXTime latest) `shouldBe` latest
          addDuration defaultTimestamp (defaultDuration {durationNanos = -500000000}) `shouldBe` before
          addDuration earliest (subtractTimestamps latest earliest) `shouldBe` latest
      , it "addDuration" $ do
          let ts = defaultTimestamp {timestampSeconds = 100, timestampNanos = 500000000}
              dur = defaultDuration {durationSeconds = 1, durationNanos = 700000000}
              result = addDuration ts dur
          timestampSeconds result `shouldBe` 102
          timestampNanos result `shouldBe` 200000000
      , it "subtractTimestamps" $ do
          let a = defaultTimestamp {timestampSeconds = 102, timestampNanos = 200000000}
              b = defaultTimestamp {timestampSeconds = 100, timestampNanos = 500000000}
              dur = subtractTimestamps a b
          durationSeconds dur `shouldBe` 1
          durationNanos dur `shouldBe` 700000000
      , it "addDuration then subtract gives identity" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 1000000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999999)
          ds <- forAll $ Gen.int64 (Range.linear 0 1000000)
          dn <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let ts = defaultTimestamp {timestampSeconds = s, timestampNanos = n}
              dur = defaultDuration {durationSeconds = ds, durationNanos = dn}
              result = subtractTimestamps (addDuration ts dur) ts
          durationToNanos result === durationToNanos dur
      , it "isValidTimestamp valid" $ do
          isValidTimestamp (defaultTimestamp {timestampSeconds = 0, timestampNanos = 0}) `shouldBe` True
          isValidTimestamp (defaultTimestamp {timestampSeconds = 1708000000, timestampNanos = 123456789}) `shouldBe` True
      , it "isValidTimestamp invalid nanos" $ do
          isValidTimestamp (defaultTimestamp {timestampSeconds = 0, timestampNanos = -1}) `shouldBe` False
          isValidTimestamp (defaultTimestamp {timestampSeconds = 0, timestampNanos = 1000000000}) `shouldBe` False
      , it "Ord is consistent with compareTimestamp" $ property $ do
          s1 <- forAll $ Gen.int64 (Range.linear 0 2000000000)
          n1 <- forAll $ Gen.int32 (Range.linear 0 999999999)
          s2 <- forAll $ Gen.int64 (Range.linear 0 2000000000)
          n2 <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let ts1 = defaultTimestamp {timestampSeconds = s1, timestampNanos = n1}
              ts2 = defaultTimestamp {timestampSeconds = s2, timestampNanos = n2}
          compare ts1 ts2 === compareTimestamp ts1 ts2
      ]


-- --------------------------------------------------------------------------
-- Duration.Util
-- --------------------------------------------------------------------------

durationUtilTests :: Spec
durationUtilTests =
  describe "Duration.Util" $
    sequence_
      [ it "NominalDiffTime roundtrip" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear (-1000000) 1000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let dur = defaultDuration {durationSeconds = s, durationNanos = n}
              rt = durationFromNominalDiffTime (durationToNominalDiffTime dur)
          durationToNanos rt === durationToNanos dur
      , it "durationFromSeconds" $ do
          let dur = durationFromSeconds 42
          durationSeconds dur `shouldBe` 42
          durationNanos dur `shouldBe` 0
      , it "preserves the maximum Duration through conversion and arithmetic" $ do
          let limit = defaultDuration {durationSeconds = 315576000000, durationNanos = 999999999}
          durationFromNominalDiffTime (durationToNominalDiffTime limit) `shouldBe` limit
          normalizeDuration limit `shouldBe` limit
          addDurations limit defaultDuration `shouldBe` limit
          compare limit (durationFromSeconds 2000000000) `shouldBe` GT
          isValidDuration (durationFromSeconds minBound) `shouldBe` False
          evaluate (durationToNanos limit) `shouldThrow` (\(_ :: ErrorCall) -> True)
      , it "durationFromMillis" $ do
          let dur = durationFromMillis 1500
          durationSeconds dur `shouldBe` 1
          durationNanos dur `shouldBe` 500000000
      , it "durationFromMillis negative" $ do
          let dur = durationFromMillis (-1500)
          durationSeconds dur `shouldBe` (-1)
          durationNanos dur `shouldBe` (-500000000)
      , it "durationFromMicros" $ do
          let dur = durationFromMicros 2500000
          durationSeconds dur `shouldBe` 2
          durationNanos dur `shouldBe` 500000000
      , it "durationFromNanos" $ do
          let dur = durationFromNanos 1500000000
          durationSeconds dur `shouldBe` 1
          durationNanos dur `shouldBe` 500000000
      , it "durationToMillis" $ do
          durationToMillis (defaultDuration {durationSeconds = 1, durationNanos = 500000000}) `shouldBe` 1500
      , it "durationToMicros" $ do
          durationToMicros (defaultDuration {durationSeconds = 1, durationNanos = 500000000}) `shouldBe` 1500000
      , it "durationToNanos" $ do
          durationToNanos (defaultDuration {durationSeconds = 1, durationNanos = 500000000}) `shouldBe` 1500000000
      , it "fromNanos . toNanos is identity" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear (-1000000) 1000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let dur = defaultDuration {durationSeconds = s, durationNanos = n}
          durationToNanos (durationFromNanos (durationToNanos dur)) === durationToNanos dur
      , it "addDurations" $ do
          let a = defaultDuration {durationSeconds = 1, durationNanos = 700000000}
              b = defaultDuration {durationSeconds = 2, durationNanos = 500000000}
          durationToNanos (addDurations a b) `shouldBe` 4200000000
      , it "negateDuration" $ do
          let dur = defaultDuration {durationSeconds = 1, durationNanos = 500000000}
              neg = negateDuration dur
          durationSeconds neg `shouldBe` (-1)
          durationNanos neg `shouldBe` (-500000000)
      , it "absDuration of negative" $ do
          let dur = defaultDuration {durationSeconds = (-3), durationNanos = (-500000000)}
              abs' = absDuration dur
          durationSeconds abs' `shouldBe` 3
          durationNanos abs' `shouldBe` 500000000
      , it "normalizeDuration" $ do
          let dur = defaultDuration {durationSeconds = 0, durationNanos = 2000000000}
              norm = normalizeDuration dur
          durationSeconds norm `shouldBe` 2
          durationNanos norm `shouldBe` 0
      , it "isValidDuration valid" $ do
          isValidDuration (defaultDuration {durationSeconds = 3600, durationNanos = 0}) `shouldBe` True
          isValidDuration (defaultDuration {durationSeconds = (-3600), durationNanos = 0}) `shouldBe` True
      , it "isValidDuration invalid sign mismatch" $ do
          isValidDuration (defaultDuration {durationSeconds = 1, durationNanos = (-500000000)}) `shouldBe` False
      , it "isValidDuration invalid range" $ do
          isValidDuration (defaultDuration {durationSeconds = 315576000001, durationNanos = 0}) `shouldBe` False
      , it "Ord is consistent with compareDuration" $ property $ do
          s1 <- forAll $ Gen.int64 (Range.linear (-1000000) 1000000)
          n1 <- forAll $ Gen.int32 (Range.linear 0 999999999)
          s2 <- forAll $ Gen.int64 (Range.linear (-1000000) 1000000)
          n2 <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let d1 = defaultDuration {durationSeconds = s1, durationNanos = n1}
              d2 = defaultDuration {durationSeconds = s2, durationNanos = n2}
          compare d1 d2 === compareDuration d1 d2
      ]


-- --------------------------------------------------------------------------
-- FieldMask.Util
-- --------------------------------------------------------------------------

fieldMaskUtilTests :: Spec
fieldMaskUtilTests =
  describe "FieldMask.Util" $
    sequence_
      [ it "fromPaths / toPaths" $ do
          let fm = fromPaths ["a", "b.c"]
          toPaths fm `shouldBe` ["a", "b.c"]
      , it "union deduplicates" $ do
          let a = fromPaths ["x", "y"]
              b = fromPaths ["y", "z"]
          toPaths (union a b) `shouldBe` ["x", "y", "z"]
      , it "union removes sub-paths" $ do
          let a = fromPaths ["a.b"]
              b = fromPaths ["a"]
          toPaths (union a b) `shouldBe` ["a"]
      , it "intersection" $ do
          let a = fromPaths ["x", "y"]
              b = fromPaths ["y", "z"]
          toPaths (intersection a b) `shouldBe` ["y"]
      , it "intersection with parent coverage" $ do
          let a = fromPaths ["a"]
              b = fromPaths ["a.b"]
          toPaths (intersection a b) `shouldBe` ["a.b"]
      , it "subtractMask" $ do
          let a = fromPaths ["x", "y", "z"]
              b = fromPaths ["y"]
          toPaths (subtractMask a b) `shouldBe` ["x", "z"]
      , it "normalize sorts and deduplicates" $ do
          let fm = fromPaths ["c", "a", "b", "a"]
          toPaths (normalize fm) `shouldBe` ["a", "b", "c"]
      , it "normalize removes sub-paths" $ do
          let fm = fromPaths ["a.b.c", "a.b", "a", "d"]
          toPaths (normalize fm) `shouldBe` ["a", "d"]
      , it "contains direct" $ do
          contains (fromPaths ["a", "b"]) "a" `shouldBe` True
          contains (fromPaths ["a", "b"]) "c" `shouldBe` False
      , it "contains parent covers child" $ do
          contains (fromPaths ["a"]) "a.b.c" `shouldBe` True
          contains (fromPaths ["a.b"]) "a" `shouldBe` False
      , it "isEmpty" $ do
          isEmpty (fromPaths []) `shouldBe` True
          isEmpty (fromPaths ["a"]) `shouldBe` False
      , it "allFieldMask for Timestamp" $ do
          let fm = allFieldMask (Proxy :: Proxy Timestamp)
              paths = toPaths fm
          ("seconds" `elem` paths) `shouldBe` True
          ("nanos" `elem` paths) `shouldBe` True
      , it "isValid against Timestamp" $ do
          isValid (Proxy :: Proxy Timestamp) (fromPaths ["seconds"]) `shouldBe` True
          isValid (Proxy :: Proxy Timestamp) (fromPaths ["seconds", "nanos"]) `shouldBe` True
          isValid (Proxy :: Proxy Timestamp) (fromPaths ["nonexistent"]) `shouldBe` False
      , it "isValid accepts sub-paths when top-level matches" $ do
          isValid (Proxy :: Proxy Timestamp) (fromPaths ["seconds.foo"]) `shouldBe` True
      , it "canonicalForm" $ do
          canonicalForm (fromPaths ["c", "a.b", "a"]) `shouldBe` "a,c"
      , it "toCamelCase" $ do
          toCamelCase "foo_bar" `shouldBe` "fooBar"
          toCamelCase "foo_bar.baz_qux" `shouldBe` "fooBar.bazQux"
          toCamelCase "simple" `shouldBe` "simple"
      , it "toSnakeCase" $ do
          toSnakeCase "fooBar" `shouldBe` "foo_bar"
          toSnakeCase "fooBar.bazQux" `shouldBe` "foo_bar.baz_qux"
          toSnakeCase "simple" `shouldBe` "simple"
      ]


-- --------------------------------------------------------------------------
-- Wrappers.Util
-- --------------------------------------------------------------------------

wrappersUtilTests :: Spec
wrappersUtilTests =
  describe "Wrappers.Util" $
    sequence_
      [ it "DoubleValue roundtrip" $ property $ do
          v <- forAll $ Gen.double (Range.linearFrac (-1e10) 1e10)
          fromDoubleValue (toDoubleValue v) === v
      , it "FloatValue roundtrip" $ property $ do
          v <- forAll $ Gen.float (Range.linearFrac (-1e5) 1e5)
          fromFloatValue (toFloatValue v) === v
      , it "Int64Value roundtrip" $ property $ do
          v <- forAll $ Gen.int64 Range.linearBounded
          fromInt64Value (toInt64Value v) === v
      , it "UInt64Value roundtrip" $ property $ do
          v <- forAll $ Gen.word64 (Range.linear 0 maxBound)
          fromUInt64Value (toUInt64Value v) === v
      , it "Int32Value roundtrip" $ property $ do
          v <- forAll $ Gen.int32 Range.linearBounded
          fromInt32Value (toInt32Value v) === v
      , it "UInt32Value roundtrip" $ property $ do
          v <- forAll $ Gen.word32 (Range.linear 0 maxBound)
          fromUInt32Value (toUInt32Value v) === v
      , it "BoolValue roundtrip" $ property $ do
          v <- forAll Gen.bool
          fromBoolValue (toBoolValue v) === v
      , it "StringValue roundtrip" $ property $ do
          v <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
          fromStringValue (toStringValue v) === v
      , it "BytesValue roundtrip" $ property $ do
          v <- forAll $ Gen.bytes (Range.linear 0 100)
          fromBytesValue (toBytesValue v) === v
      , it "Maybe conversions" $ do
          doubleValueToMaybe (maybeToDoubleValue (Just 3.14)) `shouldBe` Just 3.14
          doubleValueToMaybe (maybeToDoubleValue Nothing) `shouldBe` Nothing
          int64ValueToMaybe (maybeToInt64Value (Just 42)) `shouldBe` Just 42
          boolValueToMaybe (maybeToBoolValue (Just True)) `shouldBe` Just True
          stringValueToMaybe (maybeToStringValue (Just "hello")) `shouldBe` Just "hello"
          bytesValueToMaybe (maybeToBytesValue (Just "bytes")) `shouldBe` Just "bytes"
      ]


-- --------------------------------------------------------------------------
-- Struct.Util
-- --------------------------------------------------------------------------

structUtilTests :: Spec
structUtilTests =
  describe "Struct.Util" $
    sequence_
      [ it "fromPairs / toMap" $ do
          let s = fromPairs [("x", numberValue 1), ("y", stringValue "hello")]
          Map.size (toMap s) `shouldBe` 2
      , it "nullValue extraction" $ do
          asNull nullValue `shouldBe` Just ()
          asNull (numberValue 1) `shouldBe` Nothing
      , it "numberValue extraction" $ do
          asNumber (numberValue 3.14) `shouldBe` Just 3.14
          asNumber (stringValue "nope") `shouldBe` Nothing
      , it "stringValue extraction" $ do
          asString (stringValue "hello") `shouldBe` Just "hello"
          asString (boolValue True) `shouldBe` Nothing
      , it "boolValue extraction" $ do
          asBool (boolValue True) `shouldBe` Just True
          asBool nullValue `shouldBe` Nothing
      , it "structValue extraction" $ do
          let inner = fromPairs [("k", numberValue 42)]
              v = structValue inner
          case asStruct v of
            Just s -> Map.size (toMap s) `shouldBe` 1
            Nothing -> expectationFailure "expected struct"
      , it "listValue extraction" $ do
          let v = listValue [numberValue 1, numberValue 2, numberValue 3]
          case asList v of
            Just vs -> length vs `shouldBe` 3
            Nothing -> expectationFailure "expected list"
      , it "Aeson roundtrip via Value" $ do
          let original =
                Aeson.object
                  [ "name" Aeson..= ("test" :: Text)
                  , "count" Aeson..= (42 :: Int)
                  , "active" Aeson..= True
                  , "tags" Aeson..= (["a", "b"] :: [Text])
                  , "nothing" Aeson..= Aeson.Null
                  ]
              pbValue = valueFromAeson original
              back = valueToAeson pbValue
          back `shouldBe` original
      , it "Aeson roundtrip via Struct" $ do
          let original =
                Aeson.object
                  [ "x" Aeson..= (1.0 :: Double)
                  , "y" Aeson..= ("hello" :: Text)
                  ]
              s = structFromAeson original
              back = structToAeson s
          back `shouldBe` original
      ]
