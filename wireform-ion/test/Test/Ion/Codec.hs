module Test.Ion.Codec (ionTests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ion.Decode (decode)
import Ion.Encode (encode)
import Ion.Value qualified as I
import Test.Syd
import Test.Syd.Hedgehog ()


ionTests :: Spec
ionTests =
  describe "Ion" $
    sequence_
      [ unitTests
      , propertyRoundtrips
      , edgeCases
      , wireFormatTests
      ]


unitTests :: Spec
unitTests =
  describe "Unit roundtrips" $
    sequence_
      [ it "Null" $ do
          decode (encode I.Null) `shouldBe` Right I.Null
      , it "Bool true" $ do
          decode (encode (I.Bool True)) `shouldBe` Right (I.Bool True)
      , it "Bool false" $ do
          decode (encode (I.Bool False)) `shouldBe` Right (I.Bool False)
      , it "Int 0" $ do
          decode (encode (I.Int 0)) `shouldBe` Right (I.Int 0)
      , it "Int positive" $ do
          decode (encode (I.Int 42)) `shouldBe` Right (I.Int 42)
      , it "Int negative" $ do
          decode (encode (I.Int (-42))) `shouldBe` Right (I.Int (-42))
      , it "Int large" $ do
          decode (encode (I.Int 1000000)) `shouldBe` Right (I.Int 1000000)
      , it "Float 0" $ do
          decode (encode (I.Float 0.0)) `shouldBe` Right (I.Float 0.0)
      , it "Float pi" $ do
          decode (encode (I.Float 3.14159)) `shouldBe` Right (I.Float 3.14159)
      , it "String empty" $ do
          decode (encode (I.String T.empty)) `shouldBe` Right (I.String T.empty)
      , it "String hello" $ do
          decode (encode (I.String (T.pack "hello"))) `shouldBe` Right (I.String (T.pack "hello"))
      , it "Symbol" $ do
          decode (encode (I.Symbol (T.pack "foo"))) `shouldBe` Right (I.Symbol (T.pack "foo"))
      , it "Blob" $ do
          decode (encode (I.Blob (BS.pack [1, 2, 3]))) `shouldBe` Right (I.Blob (BS.pack [1, 2, 3]))
      , it "Clob" $ do
          decode (encode (I.Clob (BS.pack [0x41, 0x42]))) `shouldBe` Right (I.Clob (BS.pack [0x41, 0x42]))
      , it "Empty list" $ do
          decode (encode (I.List V.empty)) `shouldBe` Right (I.List V.empty)
      , it "List of ints" $ do
          let val = I.List (V.fromList [I.Int 1, I.Int 2, I.Int 3])
          decode (encode val) `shouldBe` Right val
      , it "Empty struct" $ do
          decode (encode (I.Struct V.empty)) `shouldBe` Right (I.Struct V.empty)
      , it "Struct with fields" $ do
          let val = I.Struct (V.fromList [(T.pack "x", I.Int 1), (T.pack "y", I.String (T.pack "hi"))])
          decode (encode val) `shouldBe` Right val
      , it "Annotation" $ do
          let val = I.Annotation (T.pack "ann") (I.Int 42)
          decode (encode val) `shouldBe` Right val
      ]


propertyRoundtrips :: Spec
propertyRoundtrips =
  describe "Property roundtrips" $
    sequence_
      [ it "Bool roundtrip" $ property $ do
          b <- forAll Gen.bool
          decode (encode (I.Bool b)) === Right (I.Bool b)
      , it "Small positive Int roundtrip" $ property $ do
          n <- forAll $ Gen.int64 (Range.linear 0 100000)
          decode (encode (I.Int n)) === Right (I.Int n)
      , it "Negative Int roundtrip" $ property $ do
          n <- forAll $ Gen.int64 (Range.linear (-100000) (-1))
          decode (encode (I.Int n)) === Right (I.Int n)
      , it "Float roundtrip" $ property $ do
          d <- forAll $ Gen.double (Range.linearFrac (-1e6) 1e6)
          decode (encode (I.Float d)) === Right (I.Float d)
      , it "String roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 128) Gen.alphaNum
          decode (encode (I.String t)) === Right (I.String t)
      , it "Blob roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 256)
          decode (encode (I.Blob bs)) === Right (I.Blob bs)
      , it "List of ints roundtrip" $ property $ do
          ns <- forAll $ Gen.list (Range.linear 0 20) (Gen.int64 (Range.linear 0 1000))
          let val = I.List (V.fromList (map I.Int ns))
          decode (encode val) === Right val
      , it "Symbol roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 1 64) Gen.alphaNum
          decode (encode (I.Symbol t)) === Right (I.Symbol t)
      , it "Clob roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 128)
          decode (encode (I.Clob bs)) === Right (I.Clob bs)
      , it "Struct roundtrip" $ property $ do
          ns <-
            forAll $
              Gen.list
                (Range.linear 0 8)
                (Gen.int64 (Range.linear 0 1000))
          let fields = zipWith (\i n -> (T.pack ("f" ++ show i), I.Int n)) [(0 :: Int) ..] ns
              val = I.Struct (V.fromList fields)
          decode (encode val) === Right val
      , it "List of strings roundtrip" $ property $ do
          ts <- forAll $ Gen.list (Range.linear 0 10) (Gen.text (Range.linear 0 32) Gen.alphaNum)
          let val = I.List (V.fromList (map I.String ts))
          decode (encode val) === Right val
      , it "Annotation roundtrip" $ property $ do
          ann <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha
          n <- forAll $ Gen.int64 (Range.linear 0 10000)
          let val = I.Annotation ann (I.Int n)
          decode (encode val) === Right val
      ]


edgeCases :: Spec
edgeCases =
  describe "Edge cases" $
    sequence_
      [ it "Nested lists" $ do
          let val = I.List (V.singleton (I.List (V.singleton (I.Int 1))))
          decode (encode val) `shouldBe` Right val
      , it "Nested structs" $ do
          let val =
                I.Struct
                  ( V.singleton
                      ( T.pack "inner"
                      , I.Struct (V.singleton (T.pack "x", I.Int 42))
                      )
                  )
          decode (encode val) `shouldBe` Right val
      , it "Large int" $ do
          let val = I.Int 9999999999
          decode (encode val) `shouldBe` Right val
      , it "Empty blob" $ do
          let val = I.Blob BS.empty
          decode (encode val) `shouldBe` Right val
      , it "Empty clob" $ do
          let val = I.Clob BS.empty
          decode (encode val) `shouldBe` Right val
      , it "Decode empty input" $
          case decode BS.empty of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on empty input"
      , it "Decode missing BVM" $
          case decode (BS.pack [0x00, 0x00, 0x00, 0x00]) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on invalid BVM"
      ]


wireFormatTests :: Spec
wireFormatTests =
  describe "Wire format" $
    sequence_
      [ it "BVM header" $ do
          let bs = encode I.Null
          BS.index bs 0 `shouldBe` 0xE0
          BS.index bs 1 `shouldBe` 0x01
          BS.index bs 2 `shouldBe` 0x00
          BS.index bs 3 `shouldBe` 0xEA
      , it "Null type descriptor" $ do
          let bs = encode I.Null
          BS.index bs 4 `shouldBe` 0x0F
      , it "Bool true type descriptor" $ do
          let bs = encode (I.Bool True)
          BS.index bs 4 `shouldBe` 0x11
      , it "Bool false type descriptor" $ do
          let bs = encode (I.Bool False)
          BS.index bs 4 `shouldBe` 0x10
      , it "Int 0 type descriptor" $ do
          let bs = encode (I.Int 0)
          BS.index bs 4 `shouldBe` 0x20
      , it "Positive int has type nibble 2" $ do
          let bs = encode (I.Int 1)
          let td = BS.index bs 4
          (td `div` 16) `shouldBe` 2
      , it "Negative int has type nibble 3" $ do
          let bs = encode (I.Int (-1))
          let td = BS.index bs 4
          (td `div` 16) `shouldBe` 3
      ]
