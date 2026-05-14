module Test.WellKnownUtil (wellKnownUtilTests) where

import Data.Int (Int32, Int64)
import Data.Proxy (Proxy(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector as V
import qualified Data.Aeson as Aeson
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Proto.Google.Protobuf.WellKnownTypes.Timestamp
import Proto.Google.Protobuf.WellKnownTypes.Timestamp.Util
import Proto.Google.Protobuf.WellKnownTypes.Duration
import Proto.Google.Protobuf.WellKnownTypes.Duration.Util
import Proto.Google.Protobuf.WellKnownTypes.FieldMask
import Proto.Google.Protobuf.WellKnownTypes.FieldMask.Util
import Proto.Google.Protobuf.WellKnownTypes.Wrappers
import Proto.Google.Protobuf.WellKnownTypes.Wrappers.Util
import Proto.Google.Protobuf.WellKnownTypes.Struct
import Proto.Google.Protobuf.WellKnownTypes.Struct.Util

wellKnownUtilTests :: TestTree
wellKnownUtilTests = testGroup "Well-Known Type Utilities"
  [ timestampUtilTests
  , durationUtilTests
  , fieldMaskUtilTests
  , wrappersUtilTests
  , structUtilTests
  ]

-- --------------------------------------------------------------------------
-- Timestamp.Util
-- --------------------------------------------------------------------------

timestampUtilTests :: TestTree
timestampUtilTests = testGroup "Timestamp.Util"
  [ testProperty "POSIXTime roundtrip" $ property $ do
      s <- forAll $ Gen.int64 (Range.linear 0 2000000000)
      n <- forAll $ Gen.int32 (Range.linear 0 999999999)
      let ts = defaultTimestamp { timestampSeconds = s, timestampNanos = n }
          rt = timestampFromPOSIXTime (timestampToPOSIXTime ts)
      timestampSeconds rt === s
      timestampNanos rt === n

  , testProperty "UTCTime roundtrip" $ property $ do
      s <- forAll $ Gen.int64 (Range.linear 0 2000000000)
      n <- forAll $ Gen.int32 (Range.linear 0 999999999)
      let ts = defaultTimestamp { timestampSeconds = s, timestampNanos = n }
          rt = timestampFromUTCTime (timestampToUTCTime ts)
      timestampSeconds rt === s
      timestampNanos rt === n

  , testCase "epoch is zero" $ do
      let ts = timestampFromPOSIXTime 0
      timestampSeconds ts @?= 0
      timestampNanos ts @?= 0

  , testCase "specific POSIX time" $ do
      let ts = timestampFromPOSIXTime 1708000000.5
      timestampSeconds ts @?= 1708000000
      timestampNanos ts @?= 500000000

  , testCase "addDuration" $ do
      let ts = defaultTimestamp { timestampSeconds = 100, timestampNanos = 500000000 }
          dur = defaultDuration { durationSeconds = 1, durationNanos = 700000000 }
          result = addDuration ts dur
      timestampSeconds result @?= 102
      timestampNanos result @?= 200000000

  , testCase "subtractTimestamps" $ do
      let a = defaultTimestamp { timestampSeconds = 102, timestampNanos = 200000000 }
          b = defaultTimestamp { timestampSeconds = 100, timestampNanos = 500000000 }
          dur = subtractTimestamps a b
      durationSeconds dur @?= 1
      durationNanos dur @?= 700000000

  , testProperty "addDuration then subtract gives identity" $ property $ do
      s <- forAll $ Gen.int64 (Range.linear 0 1000000000)
      n <- forAll $ Gen.int32 (Range.linear 0 999999999)
      ds <- forAll $ Gen.int64 (Range.linear 0 1000000)
      dn <- forAll $ Gen.int32 (Range.linear 0 999999999)
      let ts = defaultTimestamp { timestampSeconds = s, timestampNanos = n }
          dur = defaultDuration { durationSeconds = ds, durationNanos = dn }
          result = subtractTimestamps (addDuration ts dur) ts
      durationToNanos result === durationToNanos dur

  , testCase "isValidTimestamp valid" $ do
      isValidTimestamp (defaultTimestamp { timestampSeconds = 0, timestampNanos = 0 }) @?= True
      isValidTimestamp (defaultTimestamp { timestampSeconds = 1708000000, timestampNanos = 123456789 }) @?= True

  , testCase "isValidTimestamp invalid nanos" $ do
      isValidTimestamp (defaultTimestamp { timestampSeconds = 0, timestampNanos = -1 }) @?= False
      isValidTimestamp (defaultTimestamp { timestampSeconds = 0, timestampNanos = 1000000000 }) @?= False

  , testProperty "Ord is consistent with compareTimestamp" $ property $ do
      s1 <- forAll $ Gen.int64 (Range.linear 0 2000000000)
      n1 <- forAll $ Gen.int32 (Range.linear 0 999999999)
      s2 <- forAll $ Gen.int64 (Range.linear 0 2000000000)
      n2 <- forAll $ Gen.int32 (Range.linear 0 999999999)
      let ts1 = defaultTimestamp { timestampSeconds = s1, timestampNanos = n1 }
          ts2 = defaultTimestamp { timestampSeconds = s2, timestampNanos = n2 }
      compare ts1 ts2 === compareTimestamp ts1 ts2
  ]

-- --------------------------------------------------------------------------
-- Duration.Util
-- --------------------------------------------------------------------------

durationUtilTests :: TestTree
durationUtilTests = testGroup "Duration.Util"
  [ testProperty "NominalDiffTime roundtrip" $ property $ do
      s <- forAll $ Gen.int64 (Range.linear (-1000000) 1000000)
      n <- forAll $ Gen.int32 (Range.linear 0 999999999)
      let dur = defaultDuration { durationSeconds = s, durationNanos = n }
          rt = durationFromNominalDiffTime (durationToNominalDiffTime dur)
      durationToNanos rt === durationToNanos dur

  , testCase "durationFromSeconds" $ do
      let dur = durationFromSeconds 42
      durationSeconds dur @?= 42
      durationNanos dur @?= 0

  , testCase "durationFromMillis" $ do
      let dur = durationFromMillis 1500
      durationSeconds dur @?= 1
      durationNanos dur @?= 500000000

  , testCase "durationFromMillis negative" $ do
      let dur = durationFromMillis (-1500)
      durationSeconds dur @?= (-1)
      durationNanos dur @?= (-500000000)

  , testCase "durationFromMicros" $ do
      let dur = durationFromMicros 2500000
      durationSeconds dur @?= 2
      durationNanos dur @?= 500000000

  , testCase "durationFromNanos" $ do
      let dur = durationFromNanos 1500000000
      durationSeconds dur @?= 1
      durationNanos dur @?= 500000000

  , testCase "durationToMillis" $ do
      durationToMillis (defaultDuration { durationSeconds = 1, durationNanos = 500000000 }) @?= 1500

  , testCase "durationToMicros" $ do
      durationToMicros (defaultDuration { durationSeconds = 1, durationNanos = 500000000 }) @?= 1500000

  , testCase "durationToNanos" $ do
      durationToNanos (defaultDuration { durationSeconds = 1, durationNanos = 500000000 }) @?= 1500000000

  , testProperty "fromNanos . toNanos is identity" $ property $ do
      s <- forAll $ Gen.int64 (Range.linear (-1000000) 1000000)
      n <- forAll $ Gen.int32 (Range.linear 0 999999999)
      let dur = defaultDuration { durationSeconds = s, durationNanos = n }
      durationToNanos (durationFromNanos (durationToNanos dur)) === durationToNanos dur

  , testCase "addDurations" $ do
      let a = defaultDuration { durationSeconds = 1, durationNanos = 700000000 }
          b = defaultDuration { durationSeconds = 2, durationNanos = 500000000 }
      durationToNanos (addDurations a b) @?= 4200000000

  , testCase "negateDuration" $ do
      let dur = defaultDuration { durationSeconds = 1, durationNanos = 500000000 }
          neg = negateDuration dur
      durationSeconds neg @?= (-1)
      durationNanos neg @?= (-500000000)

  , testCase "absDuration of negative" $ do
      let dur = defaultDuration { durationSeconds = (-3), durationNanos = (-500000000) }
          abs' = absDuration dur
      durationSeconds abs' @?= 3
      durationNanos abs' @?= 500000000

  , testCase "normalizeDuration" $ do
      let dur = defaultDuration { durationSeconds = 0, durationNanos = 2000000000 }
          norm = normalizeDuration dur
      durationSeconds norm @?= 2
      durationNanos norm @?= 0

  , testCase "isValidDuration valid" $ do
      isValidDuration (defaultDuration { durationSeconds = 3600, durationNanos = 0 }) @?= True
      isValidDuration (defaultDuration { durationSeconds = (-3600), durationNanos = 0 }) @?= True

  , testCase "isValidDuration invalid sign mismatch" $ do
      isValidDuration (defaultDuration { durationSeconds = 1, durationNanos = (-500000000) }) @?= False

  , testCase "isValidDuration invalid range" $ do
      isValidDuration (defaultDuration { durationSeconds = 315576000001, durationNanos = 0 }) @?= False

  , testProperty "Ord is consistent with compareDuration" $ property $ do
      s1 <- forAll $ Gen.int64 (Range.linear (-1000000) 1000000)
      n1 <- forAll $ Gen.int32 (Range.linear 0 999999999)
      s2 <- forAll $ Gen.int64 (Range.linear (-1000000) 1000000)
      n2 <- forAll $ Gen.int32 (Range.linear 0 999999999)
      let d1 = defaultDuration { durationSeconds = s1, durationNanos = n1 }
          d2 = defaultDuration { durationSeconds = s2, durationNanos = n2 }
      compare d1 d2 === compareDuration d1 d2
  ]

-- --------------------------------------------------------------------------
-- FieldMask.Util
-- --------------------------------------------------------------------------

fieldMaskUtilTests :: TestTree
fieldMaskUtilTests = testGroup "FieldMask.Util"
  [ testCase "fromPaths / toPaths" $ do
      let fm = fromPaths ["a", "b.c"]
      toPaths fm @?= ["a", "b.c"]

  , testCase "union deduplicates" $ do
      let a = fromPaths ["x", "y"]
          b = fromPaths ["y", "z"]
      toPaths (union a b) @?= ["x", "y", "z"]

  , testCase "union removes sub-paths" $ do
      let a = fromPaths ["a.b"]
          b = fromPaths ["a"]
      toPaths (union a b) @?= ["a"]

  , testCase "intersection" $ do
      let a = fromPaths ["x", "y"]
          b = fromPaths ["y", "z"]
      toPaths (intersection a b) @?= ["y"]

  , testCase "intersection with parent coverage" $ do
      let a = fromPaths ["a"]
          b = fromPaths ["a.b"]
      toPaths (intersection a b) @?= ["a.b"]

  , testCase "subtractMask" $ do
      let a = fromPaths ["x", "y", "z"]
          b = fromPaths ["y"]
      toPaths (subtractMask a b) @?= ["x", "z"]

  , testCase "normalize sorts and deduplicates" $ do
      let fm = fromPaths ["c", "a", "b", "a"]
      toPaths (normalize fm) @?= ["a", "b", "c"]

  , testCase "normalize removes sub-paths" $ do
      let fm = fromPaths ["a.b.c", "a.b", "a", "d"]
      toPaths (normalize fm) @?= ["a", "d"]

  , testCase "contains direct" $ do
      contains (fromPaths ["a", "b"]) "a" @?= True
      contains (fromPaths ["a", "b"]) "c" @?= False

  , testCase "contains parent covers child" $ do
      contains (fromPaths ["a"]) "a.b.c" @?= True
      contains (fromPaths ["a.b"]) "a" @?= False

  , testCase "isEmpty" $ do
      isEmpty (fromPaths []) @?= True
      isEmpty (fromPaths ["a"]) @?= False

  , testCase "allFieldMask for Timestamp" $ do
      let fm = allFieldMask (Proxy :: Proxy Timestamp)
          paths = toPaths fm
      assertBool "contains seconds" ("seconds" `elem` paths)
      assertBool "contains nanos" ("nanos" `elem` paths)

  , testCase "isValid against Timestamp" $ do
      isValid (Proxy :: Proxy Timestamp) (fromPaths ["seconds"]) @?= True
      isValid (Proxy :: Proxy Timestamp) (fromPaths ["seconds", "nanos"]) @?= True
      isValid (Proxy :: Proxy Timestamp) (fromPaths ["nonexistent"]) @?= False

  , testCase "isValid accepts sub-paths when top-level matches" $ do
      isValid (Proxy :: Proxy Timestamp) (fromPaths ["seconds.foo"]) @?= True

  , testCase "canonicalForm" $ do
      canonicalForm (fromPaths ["c", "a.b", "a"]) @?= "a,c"

  , testCase "toCamelCase" $ do
      toCamelCase "foo_bar" @?= "fooBar"
      toCamelCase "foo_bar.baz_qux" @?= "fooBar.bazQux"
      toCamelCase "simple" @?= "simple"

  , testCase "toSnakeCase" $ do
      toSnakeCase "fooBar" @?= "foo_bar"
      toSnakeCase "fooBar.bazQux" @?= "foo_bar.baz_qux"
      toSnakeCase "simple" @?= "simple"
  ]

-- --------------------------------------------------------------------------
-- Wrappers.Util
-- --------------------------------------------------------------------------

wrappersUtilTests :: TestTree
wrappersUtilTests = testGroup "Wrappers.Util"
  [ testProperty "DoubleValue roundtrip" $ property $ do
      v <- forAll $ Gen.double (Range.linearFrac (-1e10) 1e10)
      fromDoubleValue (toDoubleValue v) === v

  , testProperty "FloatValue roundtrip" $ property $ do
      v <- forAll $ Gen.float (Range.linearFrac (-1e5) 1e5)
      fromFloatValue (toFloatValue v) === v

  , testProperty "Int64Value roundtrip" $ property $ do
      v <- forAll $ Gen.int64 Range.linearBounded
      fromInt64Value (toInt64Value v) === v

  , testProperty "UInt64Value roundtrip" $ property $ do
      v <- forAll $ Gen.word64 (Range.linear 0 maxBound)
      fromUInt64Value (toUInt64Value v) === v

  , testProperty "Int32Value roundtrip" $ property $ do
      v <- forAll $ Gen.int32 Range.linearBounded
      fromInt32Value (toInt32Value v) === v

  , testProperty "UInt32Value roundtrip" $ property $ do
      v <- forAll $ Gen.word32 (Range.linear 0 maxBound)
      fromUInt32Value (toUInt32Value v) === v

  , testProperty "BoolValue roundtrip" $ property $ do
      v <- forAll Gen.bool
      fromBoolValue (toBoolValue v) === v

  , testProperty "StringValue roundtrip" $ property $ do
      v <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
      fromStringValue (toStringValue v) === v

  , testProperty "BytesValue roundtrip" $ property $ do
      v <- forAll $ Gen.bytes (Range.linear 0 100)
      fromBytesValue (toBytesValue v) === v

  , testCase "Maybe conversions" $ do
      doubleValueToMaybe (maybeToDoubleValue (Just 3.14)) @?= Just 3.14
      doubleValueToMaybe (maybeToDoubleValue Nothing) @?= Nothing
      int64ValueToMaybe (maybeToInt64Value (Just 42)) @?= Just 42
      boolValueToMaybe (maybeToBoolValue (Just True)) @?= Just True
      stringValueToMaybe (maybeToStringValue (Just "hello")) @?= Just "hello"
      bytesValueToMaybe (maybeToBytesValue (Just "bytes")) @?= Just "bytes"
  ]

-- --------------------------------------------------------------------------
-- Struct.Util
-- --------------------------------------------------------------------------

structUtilTests :: TestTree
structUtilTests = testGroup "Struct.Util"
  [ testCase "fromPairs / toMap" $ do
      let s = fromPairs [("x", numberValue 1), ("y", stringValue "hello")]
      Map.size (toMap s) @?= 2

  , testCase "nullValue extraction" $ do
      asNull nullValue @?= Just ()
      asNull (numberValue 1) @?= Nothing

  , testCase "numberValue extraction" $ do
      asNumber (numberValue 3.14) @?= Just 3.14
      asNumber (stringValue "nope") @?= Nothing

  , testCase "stringValue extraction" $ do
      asString (stringValue "hello") @?= Just "hello"
      asString (boolValue True) @?= Nothing

  , testCase "boolValue extraction" $ do
      asBool (boolValue True) @?= Just True
      asBool nullValue @?= Nothing

  , testCase "structValue extraction" $ do
      let inner = fromPairs [("k", numberValue 42)]
          v = structValue inner
      case asStruct v of
        Just s -> Map.size (toMap s) @?= 1
        Nothing -> assertFailure "expected struct"

  , testCase "listValue extraction" $ do
      let v = listValue [numberValue 1, numberValue 2, numberValue 3]
      case asList v of
        Just vs -> length vs @?= 3
        Nothing -> assertFailure "expected list"

  , testCase "Aeson roundtrip via Value" $ do
      let original = Aeson.object
            [ "name" Aeson..= ("test" :: Text)
            , "count" Aeson..= (42 :: Int)
            , "active" Aeson..= True
            , "tags" Aeson..= (["a", "b"] :: [Text])
            , "nothing" Aeson..= Aeson.Null
            ]
          pbValue = valueFromAeson original
          back = valueToAeson pbValue
      back @?= original

  , testCase "Aeson roundtrip via Struct" $ do
      let original = Aeson.object
            [ "x" Aeson..= (1.0 :: Double)
            , "y" Aeson..= ("hello" :: Text)
            ]
          s = structFromAeson original
          back = structToAeson s
      back @?= original
  ]
