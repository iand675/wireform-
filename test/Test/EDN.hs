module Test.EDN (ednTests) where

import qualified Data.Aeson as Aeson
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import qualified EDN.Value as E
import EDN.Encode (encode, encodeBS)
import EDN.Decode (decode, decodeBS)
import EDN.JSON (toJSON, fromJSON)

ednTests :: TestTree
ednTests = testGroup "EDN"
  [ parseTests
  , encodeTests
  , roundtripTests
  , propertyRoundtrips
  , edgeCases
  , jsonTests
  ]

parseTests :: TestTree
parseTests = testGroup "Parse known EDN strings"
  [ testCase "nil" $
      decode "nil" @?= Right E.Nil

  , testCase "42" $
      decode "42" @?= Right (E.Integer 42)

  , testCase "-17" $
      decode "-17" @?= Right (E.Integer (-17))

  , testCase "0" $
      decode "0" @?= Right (E.Integer 0)

  , testCase "3.14" $
      decode "3.14" @?= Right (E.Float 3.14)

  , testCase "1.0e10" $
      decode "1.0e10" @?= Right (E.Float 1.0e10)

  , testCase "\"hello\"" $
      decode "\"hello\"" @?= Right (E.String "hello")

  , testCase "string with escapes" $
      decode "\"a\\nb\\tc\"" @?= Right (E.String "a\nb\tc")

  , testCase ":keyword" $
      decode ":keyword" @?= Right (E.Keyword Nothing "keyword")

  , testCase ":ns/key" $
      decode ":ns/key" @?= Right (E.Keyword (Just "ns") "key")

  , testCase "\\newline" $
      decode "\\newline" @?= Right (E.Char '\n')

  , testCase "\\space" $
      decode "\\space" @?= Right (E.Char ' ')

  , testCase "\\tab" $
      decode "\\tab" @?= Right (E.Char '\t')

  , testCase "\\return" $
      decode "\\return" @?= Right (E.Char '\r')

  , testCase "\\a" $
      decode "\\a" @?= Right (E.Char 'a')

  , testCase "(1 2 3)" $
      decode "(1 2 3)" @?= Right (E.List (V.fromList [E.Integer 1, E.Integer 2, E.Integer 3]))

  , testCase "[1 \"two\" :three]" $
      decode "[1 \"two\" :three]" @?= Right (E.Vector (V.fromList
        [E.Integer 1, E.String "two", E.Keyword Nothing "three"]))

  , testCase "{:a 1 :b 2}" $
      decode "{:a 1 :b 2}" @?= Right (E.Map (V.fromList
        [(E.Keyword Nothing "a", E.Integer 1), (E.Keyword Nothing "b", E.Integer 2)]))

  , testCase "#{1 2 3}" $
      decode "#{1 2 3}" @?= Right (E.Set (V.fromList [E.Integer 1, E.Integer 2, E.Integer 3]))

  , testCase "#inst \"1985-04-12T23:20:50.52Z\"" $
      decode "#inst \"1985-04-12T23:20:50.52Z\""
        @?= Right (E.Tagged "" "inst" (E.String "1985-04-12T23:20:50.52Z"))

  , testCase "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"" $
      decode "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""
        @?= Right (E.Tagged "" "uuid" (E.String "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"))

  , testCase "#myapp/Person {:name \"Joe\"}" $
      decode "#myapp/Person {:name \"Joe\"}"
        @?= Right (E.Tagged "myapp" "Person" (E.Map (V.fromList
              [(E.Keyword Nothing "name", E.String "Joe")])))

  , testCase "comments" $
      decode "; this is a comment\n42" @?= Right (E.Integer 42)

  , testCase "commas as whitespace" $
      decode "[1, 2, 3]" @?= Right (E.Vector (V.fromList
        [E.Integer 1, E.Integer 2, E.Integer 3]))

  , testCase "nested structure" $
      decode "{:users [{:name \"Alice\"} {:name \"Bob\"}]}"
        @?= Right (E.Map (V.fromList
              [ (E.Keyword Nothing "users", E.Vector (V.fromList
                  [ E.Map (V.fromList [(E.Keyword Nothing "name", E.String "Alice")])
                  , E.Map (V.fromList [(E.Keyword Nothing "name", E.String "Bob")])
                  ]))
              ]))

  , testCase "##Inf" $
      case decode "##Inf" of
        Right (E.Float d) -> assertBool "should be positive infinity" (isInfinite d && d > 0)
        other -> assertFailure $ "expected Float Inf, got: " ++ show other

  , testCase "##-Inf" $
      case decode "##-Inf" of
        Right (E.Float d) -> assertBool "should be negative infinity" (isInfinite d && d < 0)
        other -> assertFailure $ "expected Float -Inf, got: " ++ show other

  , testCase "##NaN" $
      case decode "##NaN" of
        Right (E.Float d) -> assertBool "should be NaN" (isNaN d)
        other -> assertFailure $ "expected Float NaN, got: " ++ show other

  , testCase "discard: [1 #_ 2 3]" $
      decode "[1 #_ 2 3]" @?= Right (E.Vector (V.fromList [E.Integer 1, E.Integer 3]))

  , testCase "discard at start of vector: [#_ 1 2]" $
      -- Previously the parser called parseValue after the discard
      -- expected to find a value, but the next byte was the
      -- container's data and the one after the discard. The
      -- collection parser now treats #_ transparently.
      decode "[#_ 1 2]" @?= Right (E.Vector (V.fromList [E.Integer 2]))

  , testCase "discard at end of vector: [1 #_ 2]" $
      decode "[1 #_ 2]" @?= Right (E.Vector (V.fromList [E.Integer 1]))

  , testCase "discard at start of list: (#_ 1 2 3)" $
      decode "(#_ 1 2 3)"
        @?= Right (E.List (V.fromList [E.Integer 2, E.Integer 3]))

  , testCase "double discard: [#_ #_ 1 2 3]" $
      -- '#_ #_ 1 2' drops both '1' and '2'; '3' remains.
      decode "[#_ #_ 1 2 3]"
        @?= Right (E.Vector (V.fromList [E.Integer 3]))

  , testCase "discard at start of set: #{#_ 1 2}" $
      decode "#{#_ 1 2}"
        @?= Right (E.Set (V.fromList [E.Integer 2]))

  , testCase "symbol" $
      decode "foo" @?= Right (E.Symbol Nothing "foo")

  , testCase "namespaced symbol" $
      decode "my-ns/bar" @?= Right (E.Symbol (Just "my-ns") "bar")

  , testCase "true" $
      decode "true" @?= Right (E.Bool True)

  , testCase "false" $
      decode "false" @?= Right (E.Bool False)

  , testCase "empty list" $
      decode "()" @?= Right (E.List V.empty)

  , testCase "empty vector" $
      decode "[]" @?= Right (E.Vector V.empty)

  , testCase "empty map" $
      decode "{}" @?= Right (E.Map V.empty)

  , testCase "empty set" $
      decode "#{}" @?= Right (E.Set V.empty)
  ]

encodeTests :: TestTree
encodeTests = testGroup "Encode"
  [ testCase "nil" $
      encode E.Nil @?= "nil"

  , testCase "true" $
      encode (E.Bool True) @?= "true"

  , testCase "false" $
      encode (E.Bool False) @?= "false"

  , testCase "integer" $
      encode (E.Integer 42) @?= "42"

  , testCase "negative integer" $
      encode (E.Integer (-17)) @?= "-17"

  , testCase "string" $
      encode (E.String "hello") @?= "\"hello\""

  , testCase "string with escapes" $
      encode (E.String "a\nb") @?= "\"a\\nb\""

  , testCase "keyword" $
      encode (E.Keyword Nothing "foo") @?= ":foo"

  , testCase "namespaced keyword" $
      encode (E.Keyword (Just "ns") "key") @?= ":ns/key"

  , testCase "char newline" $
      encode (E.Char '\n') @?= "\\newline"

  , testCase "char a" $
      encode (E.Char 'a') @?= "\\a"

  , testCase "list" $
      encode (E.List (V.fromList [E.Integer 1, E.Integer 2])) @?= "(1 2)"

  , testCase "vector" $
      encode (E.Vector (V.fromList [E.Integer 1, E.Integer 2])) @?= "[1 2]"

  , testCase "map" $
      encode (E.Map (V.fromList [(E.Keyword Nothing "a", E.Integer 1)])) @?= "{:a 1}"

  , testCase "set" $
      encode (E.Set (V.fromList [E.Integer 1, E.Integer 2])) @?= "#{1 2}"

  , testCase "tagged" $
      encode (E.Tagged "" "inst" (E.String "1985")) @?= "#inst \"1985\""

  , testCase "namespaced tagged" $
      encode (E.Tagged "myapp" "Person" (E.String "Joe")) @?= "#myapp/Person \"Joe\""

  , testCase "##Inf" $ do
      let t = encode (E.Float (1/0))
      t @?= "##Inf"

  , testCase "##-Inf" $ do
      let t = encode (E.Float (-1/0))
      t @?= "##-Inf"

  , testCase "##NaN" $ do
      let t = encode (E.Float (0/0))
      t @?= "##NaN"

  , testCase "encodeBS produces UTF-8" $ do
      let bs = encodeBS (E.String "hello")
      bs @?= "\"hello\""
  ]

roundtripTests :: TestTree
roundtripTests = testGroup "Roundtrip (encode then decode)"
  [ testCase "nil" $ rt E.Nil
  , testCase "true" $ rt (E.Bool True)
  , testCase "false" $ rt (E.Bool False)
  , testCase "integer" $ rt (E.Integer 42)
  , testCase "negative integer" $ rt (E.Integer (-100))
  , testCase "zero" $ rt (E.Integer 0)
  , testCase "float" $ rt (E.Float 3.14)
  , testCase "string" $ rt (E.String "hello world")
  , testCase "char" $ rt (E.Char 'x')
  , testCase "char newline" $ rt (E.Char '\n')
  , testCase "char space" $ rt (E.Char ' ')
  , testCase "keyword" $ rt (E.Keyword Nothing "key")
  , testCase "namespaced keyword" $ rt (E.Keyword (Just "ns") "key")
  , testCase "symbol" $ rt (E.Symbol Nothing "sym")
  , testCase "namespaced symbol" $ rt (E.Symbol (Just "ns") "sym")
  , testCase "empty list" $ rt (E.List V.empty)
  , testCase "list" $ rt (E.List (V.fromList [E.Integer 1, E.Integer 2, E.Integer 3]))
  , testCase "empty vector" $ rt (E.Vector V.empty)
  , testCase "vector" $ rt (E.Vector (V.fromList [E.Integer 1, E.String "two", E.Keyword Nothing "three"]))
  , testCase "empty map" $ rt (E.Map V.empty)
  , testCase "map" $ rt (E.Map (V.fromList [(E.Keyword Nothing "a", E.Integer 1)]))
  , testCase "empty set" $ rt (E.Set V.empty)
  , testCase "set" $ rt (E.Set (V.fromList [E.Integer 1, E.Integer 2]))
  , testCase "tagged" $ rt (E.Tagged "" "inst" (E.String "1985"))
  , testCase "namespaced tagged" $ rt (E.Tagged "myapp" "Person" (E.Map (V.fromList [(E.Keyword Nothing "name", E.String "Joe")])))
  , testCase "nested" $
      rt (E.Map (V.fromList
        [(E.Keyword Nothing "users", E.Vector (V.fromList
          [ E.Map (V.fromList [(E.Keyword Nothing "name", E.String "Alice")])
          , E.Map (V.fromList [(E.Keyword Nothing "name", E.String "Bob")])
          ]))]))
  ]
  where
    rt val = decode (encode val) @?= Right val

propertyRoundtrips :: TestTree
propertyRoundtrips = testGroup "Property roundtrips"
  [ testProperty "Integer roundtrip" $ property $ do
      n <- forAll $ Gen.integral (Range.linear (-10000) 10000)
      let val = E.Integer n
      decode (encode val) === Right val

  , testProperty "String roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
      let val = E.String t
      decode (encode val) === Right val

  , testProperty "Vector of integers roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 30) $
              Gen.integral (Range.linear (-1000) 1000)
      let val = E.Vector (V.fromList (map E.Integer ns))
      decode (encode val) === Right val

  , testProperty "Keyword roundtrip" $ property $ do
      name <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha
      let val = E.Keyword Nothing name
      decode (encode val) === Right val

  , testProperty "Bool roundtrip" $ property $ do
      b <- forAll Gen.bool
      let val = E.Bool b
      decode (encode val) === Right val
  ]

edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "large integer" $ do
      let val = E.Integer 999999999999999999
      decode (encode val) @?= Right val

  , testCase "negative large integer" $ do
      let val = E.Integer (-999999999999999999)
      decode (encode val) @?= Right val

  , testCase "deeply nested" $ do
      let nest 0 = E.Integer 42
          nest n = E.Vector (V.singleton (nest (n - 1)))
          val = nest (15 :: Int)
      decode (encode val) @?= Right val

  , testCase "string with all escape types" $ do
      let val = E.String "tab:\tnewline:\nreturn:\rquote:\"backslash:\\"
      decode (encode val) @?= Right val

  , testCase "multiple comments" $
      decode "; comment 1\n; comment 2\n42" @?= Right (E.Integer 42)

  , testCase "whitespace variations" $
      decode "  \t\n  42  " @?= Right (E.Integer 42)

  , testCase "map with mixed value types" $ do
      let val = E.Map (V.fromList
                  [ (E.Keyword Nothing "int", E.Integer 1)
                  , (E.Keyword Nothing "str", E.String "hello")
                  , (E.Keyword Nothing "bool", E.Bool True)
                  , (E.Keyword Nothing "nil", E.Nil)
                  ])
      decode (encode val) @?= Right val

  , testCase "discard in map" $
      decode "{:a 1 #_ :b #_ 2 :c 3}"
        @?= Right (E.Map (V.fromList
              [(E.Keyword Nothing "a", E.Integer 1), (E.Keyword Nothing "c", E.Integer 3)]))

  , testCase "decodeBS" $
      decodeBS "42" @?= Right (E.Integer 42)

  , testCase "empty input" $
      case decode "" of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"
  ]

jsonTests :: TestTree
jsonTests = testGroup "JSON conversion"
  [ testCase "Nil to JSON" $
      toJSON E.Nil @?= Aeson.Null

  , testCase "Bool to JSON" $ do
      toJSON (E.Bool True) @?= Aeson.Bool True
      toJSON (E.Bool False) @?= Aeson.Bool False

  , testCase "Integer to JSON" $
      toJSON (E.Integer 42) @?= Aeson.Number 42

  , testCase "String to JSON" $
      toJSON (E.String "hello") @?= Aeson.String "hello"

  , testCase "Char to JSON" $
      toJSON (E.Char 'x') @?= Aeson.String "x"

  , testCase "Keyword to JSON" $
      toJSON (E.Keyword Nothing "name") @?= Aeson.String ":name"

  , testCase "Symbol to JSON" $
      toJSON (E.Symbol Nothing "foo") @?= Aeson.String "foo"

  , testCase "Vector to JSON" $
      toJSON (E.Vector (V.fromList [E.Integer 1, E.Integer 2]))
        @?= Aeson.Array (V.fromList [Aeson.Number 1, Aeson.Number 2])

  , testCase "Set to JSON" $
      toJSON (E.Set (V.fromList [E.Integer 1]))
        @?= Aeson.Array (V.fromList [Aeson.Number 1])

  , testCase "Tagged to JSON" $ do
      let json = toJSON (E.Tagged "" "inst" (E.String "1985"))
      case json of
        Aeson.Object _ -> pure ()
        _ -> assertFailure "expected JSON object for tagged"

  , testCase "Map with keyword keys to JSON object" $ do
      let json = toJSON (E.Map (V.fromList [(E.Keyword Nothing "a", E.Integer 1)]))
      case json of
        Aeson.Object _ -> pure ()
        _ -> assertFailure "expected JSON object for keyword-keyed map"

  , testCase "fromJSON null" $
      fromJSON Aeson.Null @?= E.Nil

  , testCase "fromJSON bool" $
      fromJSON (Aeson.Bool True) @?= E.Bool True

  , testCase "fromJSON string" $
      fromJSON (Aeson.String "hi") @?= E.String "hi"

  , testCase "fromJSON integer" $
      fromJSON (Aeson.Number 42) @?= E.Integer 42

  , testCase "fromJSON array" $
      fromJSON (Aeson.Array (V.fromList [Aeson.Number 1]))
        @?= E.Vector (V.fromList [E.Integer 1])
  ]
