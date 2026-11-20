module Test.CBORDiagnostic (cborDiagnosticTests) where

import CBOR.Diagnostic (fromDiagnostic, toDiagnostic)
import CBOR.Value qualified as C
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()


cborDiagnosticTests :: Spec
cborDiagnosticTests =
  describe "CBOR Diagnostic" $
    sequence_
      [ renderTests
      , parseTests
      , roundtripTests
      ]


renderTests :: Spec
renderTests =
  describe "Render" $
    sequence_
      [ it "unsigned int 0" $
          toDiagnostic (C.UInt 0) `shouldBe` "0"
      , it "unsigned int 23" $
          toDiagnostic (C.UInt 23) `shouldBe` "23"
      , it "unsigned int 24" $
          toDiagnostic (C.UInt 24) `shouldBe` "24"
      , it "negative -1" $
          toDiagnostic (C.NInt 0) `shouldBe` "-1"
      , it "negative -100" $
          toDiagnostic (C.NInt 99) `shouldBe` "-100"
      , it "byte string" $
          toDiagnostic (C.ByteString (BS.pack [1, 2, 3, 4])) `shouldBe` "h'01020304'"
      , it "empty byte string" $
          toDiagnostic (C.ByteString BS.empty) `shouldBe` "h''"
      , it "text string" $
          toDiagnostic (C.TextString "hello") `shouldBe` "\"hello\""
      , it "text string with escape" $
          toDiagnostic (C.TextString "a\"b") `shouldBe` "\"a\\\"b\""
      , it "renders control characters as valid JSON string escapes" $ do
          let original = C.TextString "\NUL\b\f\x1f"
              rendered = toDiagnostic original
          T.any (< '\x20') rendered `shouldBe` False
          fromDiagnostic rendered `shouldBe` Right original
      , it "array" $
          toDiagnostic (C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3])) `shouldBe` "[1, 2, 3]"
      , it "empty array" $
          toDiagnostic (C.Array V.empty) `shouldBe` "[]"
      , it "map with string keys" $
          toDiagnostic (C.Map (V.fromList [(C.TextString "a", C.UInt 1)])) `shouldBe` "{\"a\": 1}"
      , it "map with int keys" $
          toDiagnostic (C.Map (V.fromList [(C.UInt 1, C.TextString "a")])) `shouldBe` "{1: \"a\"}"
      , it "tag" $
          toDiagnostic (C.Tag 0 (C.TextString "2013-03-21T20:04:00Z"))
            `shouldBe` "0(\"2013-03-21T20:04:00Z\")"
      , it "false" $
          toDiagnostic (C.Bool False) `shouldBe` "false"
      , it "true" $
          toDiagnostic (C.Bool True) `shouldBe` "true"
      , it "null" $
          toDiagnostic C.Null `shouldBe` "null"
      , it "undefined" $
          toDiagnostic C.Undefined `shouldBe` "undefined"
      , it "simple value" $
          toDiagnostic (C.Simple 16) `shouldBe` "simple(16)"
      , it "float 1.5" $
          toDiagnostic (C.Float64 1.5) `shouldBe` "1.5"
      , it "NaN" $
          toDiagnostic (C.Float64 (0 / 0)) `shouldBe` "NaN"
      , it "Infinity" $
          toDiagnostic (C.Float64 (1 / 0)) `shouldBe` "Infinity"
      , it "-Infinity" $
          toDiagnostic (C.Float64 ((-1) / 0)) `shouldBe` "-Infinity"
      ]


parseTests :: Spec
parseTests =
  describe "Parse" $
    sequence_
      [ it "unsigned int" $
          fromDiagnostic "42" `shouldBe` Right (C.UInt 42)
      , it "negative int" $
          fromDiagnostic "-1" `shouldBe` Right (C.NInt 0)
      , it "negative -100" $
          fromDiagnostic "-100" `shouldBe` Right (C.NInt 99)
      , it "byte string" $
          fromDiagnostic "h'01020304'" `shouldBe` Right (C.ByteString (BS.pack [1, 2, 3, 4]))
      , it "empty byte string" $
          fromDiagnostic "h''" `shouldBe` Right (C.ByteString BS.empty)
      , it "text string" $
          fromDiagnostic "\"hello\"" `shouldBe` Right (C.TextString "hello")
      , it "text string with escape" $
          fromDiagnostic "\"a\\\"b\"" `shouldBe` Right (C.TextString "a\"b")
      , it "decodes JSON escapes and a Unicode surrogate pair" $
          fromDiagnostic "\"\\u0061\\/\\b\\f\\uD834\\uDD1E\""
            `shouldBe` Right (C.TextString "a/\b\f\x1D11E")
      , it "rejects invalid JSON escapes and raw control characters" $
          mapM_
            (\s -> isLeft (fromDiagnostic s) `shouldBe` True)
            ["\"\\q\"", "\"\\uD800\"", "\"\\u12xz\"", "\"\NUL\""]
      , it "array" $
          fromDiagnostic "[1, 2, 3]"
            `shouldBe` Right (C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3]))
      , it "empty array" $
          fromDiagnostic "[]" `shouldBe` Right (C.Array V.empty)
      , it "map" $
          fromDiagnostic "{\"a\": 1}"
            `shouldBe` Right (C.Map (V.fromList [(C.TextString "a", C.UInt 1)]))
      , it "empty map" $
          fromDiagnostic "{}" `shouldBe` Right (C.Map V.empty)
      , it "tag" $
          fromDiagnostic "0(\"2013-03-21T20:04:00Z\")"
            `shouldBe` Right (C.Tag 0 (C.TextString "2013-03-21T20:04:00Z"))
      , it "false" $
          fromDiagnostic "false" `shouldBe` Right (C.Bool False)
      , it "true" $
          fromDiagnostic "true" `shouldBe` Right (C.Bool True)
      , it "null" $
          fromDiagnostic "null" `shouldBe` Right C.Null
      , it "undefined" $
          fromDiagnostic "undefined" `shouldBe` Right C.Undefined
      , it "NaN" $
          case fromDiagnostic "NaN" of
            Right (C.Float64 d) | isNaN d -> pure ()
            other -> expectationFailure $ "expected NaN, got " ++ show other
      , it "Infinity" $
          fromDiagnostic "Infinity" `shouldBe` Right (C.Float64 (1 / 0))
      , it "-Infinity" $
          fromDiagnostic "-Infinity" `shouldBe` Right (C.Float64 ((-1) / 0))
      , it "simple value" $
          fromDiagnostic "simple(16)" `shouldBe` Right (C.Simple 16)
      , it "rejects reserved and overflowing simple values" $ do
          isLeft (fromDiagnostic "simple(24)") `shouldBe` True
          isLeft (fromDiagnostic "simple(18446744073709551616)") `shouldBe` True
      , it "represents large decimal integers as bignums" $ do
          let magnitude = C.ByteString (BS.pack (1 : replicate 8 0))
          fromDiagnostic "18446744073709551616" `shouldBe` Right (C.Tag 2 magnitude)
          fromDiagnostic "-18446744073709551617" `shouldBe` Right (C.Tag 3 magnitude)
      , it "rejects a tag number above the 64-bit range" $
          isLeft (fromDiagnostic "18446744073709551616(1)") `shouldBe` True
      , it "float" $
          fromDiagnostic "1.5" `shouldBe` Right (C.Float64 1.5)
      , it "negative float" $
          fromDiagnostic "-1.5" `shouldBe` Right (C.Float64 (-1.5))
      , it "scientific notation" $
          fromDiagnostic "1.0e300" `shouldBe` Right (C.Float64 1.0e300)
      , it "nested" $
          fromDiagnostic "[1, [2, 3], {\"a\": true}]"
            `shouldBe` Right
              ( C.Array
                  ( V.fromList
                      [ C.UInt 1
                      , C.Array (V.fromList [C.UInt 2, C.UInt 3])
                      , C.Map (V.fromList [(C.TextString "a", C.Bool True)])
                      ]
                  )
              )
      , it "error on empty" $
          case fromDiagnostic "" of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on empty"
      , it "error on trailing" $
          case fromDiagnostic "42 extra" of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on trailing"
      ]


roundtripTests :: Spec
roundtripTests =
  describe "Roundtrip" $
    sequence_
      [ it "UInt roundtrip" $ property $ do
          n <- forAll $ Gen.word64 (Range.linear 0 1000000)
          let val = C.UInt n
          fromDiagnostic (toDiagnostic val) === Right val
      , it "NInt roundtrip" $ property $ do
          n <- forAll $ Gen.word64 (Range.linear 0 1000000)
          let val = C.NInt n
          fromDiagnostic (toDiagnostic val) === Right val
      , it "Bool roundtrip" $ property $ do
          b <- forAll Gen.bool
          let val = C.Bool b
          fromDiagnostic (toDiagnostic val) === Right val
      , it "Null roundtrip" $
          fromDiagnostic (toDiagnostic C.Null) `shouldBe` Right C.Null
      , it "Undefined roundtrip" $
          fromDiagnostic (toDiagnostic C.Undefined) `shouldBe` Right C.Undefined
      , it "TextString roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
          let val = C.TextString t
          fromDiagnostic (toDiagnostic val) === Right val
      , it "ByteString roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 50)
          let val = C.ByteString bs
          fromDiagnostic (toDiagnostic val) === Right val
      , it "Array of UInts roundtrip" $ property $ do
          ns <- forAll $ Gen.list (Range.linear 0 10) (Gen.word64 (Range.linear 0 1000))
          let val = C.Array (V.fromList (map C.UInt ns))
          fromDiagnostic (toDiagnostic val) === Right val
      , it "Tag roundtrip" $ property $ do
          tagNum <- forAll $ Gen.word64 (Range.linear 0 1000)
          n <- forAll $ Gen.word64 (Range.linear 0 1000)
          let val = C.Tag tagNum (C.UInt n)
          fromDiagnostic (toDiagnostic val) === Right val
      ]
