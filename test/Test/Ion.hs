module Test.Ion (ionTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import qualified Ion.Value as I
import Ion.Encode (encode)
import Ion.Decode (decode)

ionTests :: TestTree
ionTests = testGroup "Ion"
  [ unitTests
  , propertyRoundtrips
  , edgeCases
  , wireFormatTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit roundtrips"
  [ testCase "Null" $ do
      decode (encode I.Null) @?= Right I.Null

  , testCase "Bool true" $ do
      decode (encode (I.Bool True)) @?= Right (I.Bool True)

  , testCase "Bool false" $ do
      decode (encode (I.Bool False)) @?= Right (I.Bool False)

  , testCase "Int 0" $ do
      decode (encode (I.Int 0)) @?= Right (I.Int 0)

  , testCase "Int positive" $ do
      decode (encode (I.Int 42)) @?= Right (I.Int 42)

  , testCase "Int negative" $ do
      decode (encode (I.Int (-42))) @?= Right (I.Int (-42))

  , testCase "Int large" $ do
      decode (encode (I.Int 1000000)) @?= Right (I.Int 1000000)

  , testCase "Float 0" $ do
      decode (encode (I.Float 0.0)) @?= Right (I.Float 0.0)

  , testCase "Float pi" $ do
      decode (encode (I.Float 3.14159)) @?= Right (I.Float 3.14159)

  , testCase "String empty" $ do
      decode (encode (I.String T.empty)) @?= Right (I.String T.empty)

  , testCase "String hello" $ do
      decode (encode (I.String (T.pack "hello"))) @?= Right (I.String (T.pack "hello"))

  , testCase "Symbol" $ do
      decode (encode (I.Symbol (T.pack "foo"))) @?= Right (I.Symbol (T.pack "foo"))

  , testCase "Blob" $ do
      decode (encode (I.Blob (BS.pack [1,2,3]))) @?= Right (I.Blob (BS.pack [1,2,3]))

  , testCase "Clob" $ do
      decode (encode (I.Clob (BS.pack [0x41,0x42]))) @?= Right (I.Clob (BS.pack [0x41,0x42]))

  , testCase "Empty list" $ do
      decode (encode (I.List V.empty)) @?= Right (I.List V.empty)

  , testCase "List of ints" $ do
      let val = I.List (V.fromList [I.Int 1, I.Int 2, I.Int 3])
      decode (encode val) @?= Right val

  , testCase "Empty struct" $ do
      decode (encode (I.Struct V.empty)) @?= Right (I.Struct V.empty)

  , testCase "Struct with fields" $ do
      let val = I.Struct (V.fromList [(T.pack "x", I.Int 1), (T.pack "y", I.String (T.pack "hi"))])
      decode (encode val) @?= Right val

  , testCase "Annotation" $ do
      let val = I.Annotation (T.pack "ann") (I.Int 42)
      decode (encode val) @?= Right val
  ]

propertyRoundtrips :: TestTree
propertyRoundtrips = testGroup "Property roundtrips"
  [ testProperty "Bool roundtrip" $ property $ do
      b <- forAll Gen.bool
      decode (encode (I.Bool b)) === Right (I.Bool b)

  , testProperty "Small positive Int roundtrip" $ property $ do
      n <- forAll $ Gen.int64 (Range.linear 0 100000)
      decode (encode (I.Int n)) === Right (I.Int n)

  , testProperty "Negative Int roundtrip" $ property $ do
      n <- forAll $ Gen.int64 (Range.linear (-100000) (-1))
      decode (encode (I.Int n)) === Right (I.Int n)

  , testProperty "Float roundtrip" $ property $ do
      d <- forAll $ Gen.double (Range.linearFrac (-1e6) 1e6)
      decode (encode (I.Float d)) === Right (I.Float d)

  , testProperty "String roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 128) Gen.alphaNum
      decode (encode (I.String t)) === Right (I.String t)

  , testProperty "Blob roundtrip" $ property $ do
      bs <- forAll $ Gen.bytes (Range.linear 0 256)
      decode (encode (I.Blob bs)) === Right (I.Blob bs)

  , testProperty "List of ints roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 20) (Gen.int64 (Range.linear 0 1000))
      let val = I.List (V.fromList (map I.Int ns))
      decode (encode val) === Right val

  , testProperty "Symbol roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 1 64) Gen.alphaNum
      decode (encode (I.Symbol t)) === Right (I.Symbol t)

  , testProperty "Clob roundtrip" $ property $ do
      bs <- forAll $ Gen.bytes (Range.linear 0 128)
      decode (encode (I.Clob bs)) === Right (I.Clob bs)

  , testProperty "Struct roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 8)
              (Gen.int64 (Range.linear 0 1000))
      let fields = zipWith (\i n -> (T.pack ("f" ++ show i), I.Int n)) [(0::Int)..] ns
          val = I.Struct (V.fromList fields)
      decode (encode val) === Right val

  , testProperty "List of strings roundtrip" $ property $ do
      ts <- forAll $ Gen.list (Range.linear 0 10) (Gen.text (Range.linear 0 32) Gen.alphaNum)
      let val = I.List (V.fromList (map I.String ts))
      decode (encode val) === Right val

  , testProperty "Annotation roundtrip" $ property $ do
      ann <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha
      n <- forAll $ Gen.int64 (Range.linear 0 10000)
      let val = I.Annotation ann (I.Int n)
      decode (encode val) === Right val
  ]

edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "Nested lists" $ do
      let val = I.List (V.singleton (I.List (V.singleton (I.Int 1))))
      decode (encode val) @?= Right val

  , testCase "Nested structs" $ do
      let val = I.Struct (V.singleton (T.pack "inner",
                  I.Struct (V.singleton (T.pack "x", I.Int 42))))
      decode (encode val) @?= Right val

  , testCase "Large int" $ do
      let val = I.Int 9999999999
      decode (encode val) @?= Right val

  , testCase "Empty blob" $ do
      let val = I.Blob BS.empty
      decode (encode val) @?= Right val

  , testCase "Empty clob" $ do
      let val = I.Clob BS.empty
      decode (encode val) @?= Right val

  , testCase "Decode empty input" $
      case decode BS.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"

  , testCase "Decode missing BVM" $
      case decode (BS.pack [0x00, 0x00, 0x00, 0x00]) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on invalid BVM"
  ]

wireFormatTests :: TestTree
wireFormatTests = testGroup "Wire format"
  [ testCase "BVM header" $ do
      let bs = encode I.Null
      BS.index bs 0 @?= 0xE0
      BS.index bs 1 @?= 0x01
      BS.index bs 2 @?= 0x00
      BS.index bs 3 @?= 0xEA

  , testCase "Null type descriptor" $ do
      let bs = encode I.Null
      BS.index bs 4 @?= 0x0F

  , testCase "Bool true type descriptor" $ do
      let bs = encode (I.Bool True)
      BS.index bs 4 @?= 0x11

  , testCase "Bool false type descriptor" $ do
      let bs = encode (I.Bool False)
      BS.index bs 4 @?= 0x10

  , testCase "Int 0 type descriptor" $ do
      let bs = encode (I.Int 0)
      BS.index bs 4 @?= 0x20

  , testCase "Positive int has type nibble 2" $ do
      let bs = encode (I.Int 1)
      let td = BS.index bs 4
      (td `div` 16) @?= 2

  , testCase "Negative int has type nibble 3" $ do
      let bs = encode (I.Int (-1))
      let td = BS.index bs 4
      (td `div` 16) @?= 3

  , testCase "Symbol-bearing payload emits a local symbol table" $ do
      let bs = encode (I.Symbol (T.pack "foo"))
      -- BVM, then the LST envelope (annotation TD = 0xE0 nibble),
      -- then the value (symbol TD = 0x70 nibble).
      BS.index bs 4 @?= 0xE9
      BS.index bs 5 @?= 0x81  -- annot length VarUInt = 1
      BS.index bs 6 @?= 0x83  -- annot SID = 3 ($ion_symbol_table)

  , testCase "Symbol value uses SID 10 for first user symbol" $ do
      let bs = encode (I.Symbol (T.pack "foo"))
      -- After the 10-byte LST envelope (annotation header + struct +
      -- list + string) the payload begins with the symbol TD 0x71 then
      -- VarUInt 10 = 0x0A.
      let n = BS.length bs
      BS.index bs (n - 2) @?= 0x71
      BS.index bs (n - 1) @?= 0x0A
  ]
