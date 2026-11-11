module Test.BSON (bsonTests) where

import BSON.Decode (decode)
import BSON.Encode (encode)
import BSON.Value qualified as B
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()


bsonTests :: Spec
bsonTests =
  describe "BSON" $
    sequence_
      [ unitTests
      , propertyRoundtrips
      , edgeCases
      , wireFormatTests
      , newTypeTests
      ]


unitTests :: Spec
unitTests =
  describe "Unit roundtrips" $
    sequence_
      [ it "Null" $ do
          let val = B.Document (V.singleton (T.pack "x", B.Null))
          decode (encode val) `shouldBe` Right val
      , it "Bool true" $ do
          let val = B.Document (V.singleton (T.pack "b", B.Bool True))
          decode (encode val) `shouldBe` Right val
      , it "Bool false" $ do
          let val = B.Document (V.singleton (T.pack "b", B.Bool False))
          decode (encode val) `shouldBe` Right val
      , it "Int32" $ do
          let val = B.Document (V.singleton (T.pack "n", B.Int32 42))
          decode (encode val) `shouldBe` Right val
      , it "Int64" $ do
          let val = B.Document (V.singleton (T.pack "n", B.Int64 (-999999999999)))
          decode (encode val) `shouldBe` Right val
      , it "Double" $ do
          let val = B.Document (V.singleton (T.pack "d", B.Double 3.14))
          decode (encode val) `shouldBe` Right val
      , it "String" $ do
          let val = B.Document (V.singleton (T.pack "s", B.String (T.pack "hello world")))
          decode (encode val) `shouldBe` Right val
      , it "Binary" $ do
          let val = B.Document (V.singleton (T.pack "b", B.Binary 0x00 (BS.pack [1, 2, 3, 4])))
          decode (encode val) `shouldBe` Right val
      , it "DateTime" $ do
          let val = B.Document (V.singleton (T.pack "dt", B.DateTime 1609459200000))
          decode (encode val) `shouldBe` Right val
      , it "ObjectId" $ do
          let oid = BS.pack [0x50, 0x7f, 0x1f, 0x77, 0xbc, 0xf8, 0x6c, 0xd7, 0x99, 0x43, 0x90, 0x11]
              val = B.Document (V.singleton (T.pack "id", B.ObjectId oid))
          decode (encode val) `shouldBe` Right val
      , it "Regex" $ do
          let val = B.Document (V.singleton (T.pack "r", B.Regex (T.pack "abc.*") (T.pack "i")))
          decode (encode val) `shouldBe` Right val
      , it "Nested document" $ do
          let inner = B.Document (V.singleton (T.pack "x", B.Int32 1))
              val = B.Document (V.singleton (T.pack "nested", inner))
          decode (encode val) `shouldBe` Right val
      , it "Array" $ do
          let val =
                B.Document
                  ( V.singleton
                      ( T.pack "arr"
                      , B.Array (V.fromList [B.Int32 1, B.Int32 2, B.Int32 3])
                      )
                  )
          decode (encode val) `shouldBe` Right val
      ]


propertyRoundtrips :: Spec
propertyRoundtrips =
  describe "Property roundtrips" $
    sequence_
      [ it "Int32 roundtrip" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          let val = B.Document (V.singleton (T.pack "v", B.Int32 n))
          decode (encode val) === Right val
      , it "Int64 roundtrip" $ property $ do
          n <- forAll $ Gen.int64 Range.linearBounded
          let val = B.Document (V.singleton (T.pack "v", B.Int64 n))
          decode (encode val) === Right val
      , it "Double roundtrip" $ property $ do
          d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
          let val = B.Document (V.singleton (T.pack "v", B.Double d))
          decode (encode val) === Right val
      , it "String roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 128) Gen.alphaNum
          let val = B.Document (V.singleton (T.pack "v", B.String t))
          decode (encode val) === Right val
      , it "Binary roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 256)
          let val = B.Document (V.singleton (T.pack "v", B.Binary 0x00 bs))
          decode (encode val) === Right val
      , it "Bool roundtrip" $ property $ do
          b <- forAll Gen.bool
          let val = B.Document (V.singleton (T.pack "v", B.Bool b))
          decode (encode val) === Right val
      , it "DateTime roundtrip" $ property $ do
          ms <- forAll $ Gen.int64 Range.linearBounded
          let val = B.Document (V.singleton (T.pack "v", B.DateTime ms))
          decode (encode val) === Right val
      , it "Nested document roundtrip" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          t <- forAll $ Gen.text (Range.linear 0 64) Gen.alphaNum
          b <- forAll Gen.bool
          let inner =
                B.Document
                  ( V.fromList
                      [ (T.pack "n", B.Int32 n)
                      , (T.pack "s", B.String t)
                      ]
                  )
              val =
                B.Document
                  ( V.fromList
                      [ (T.pack "inner", inner)
                      , (T.pack "flag", B.Bool b)
                      , (T.pack "top", B.Null)
                      ]
                  )
          decode (encode val) === Right val
      , it "Array roundtrip" $ property $ do
          ns <- forAll $ Gen.list (Range.linear 0 10) (Gen.int32 Range.linearBounded)
          let arr = B.Array (V.fromList (map B.Int32 ns))
              val = B.Document (V.singleton (T.pack "a", arr))
          decode (encode val) === Right val
      , it "Multiple fields roundtrip" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          m <- forAll $ Gen.int64 Range.linearBounded
          t <- forAll $ Gen.text (Range.linear 0 64) Gen.alphaNum
          b <- forAll Gen.bool
          let val =
                B.Document
                  ( V.fromList
                      [ (T.pack "i32", B.Int32 n)
                      , (T.pack "i64", B.Int64 m)
                      , (T.pack "str", B.String t)
                      , (T.pack "bool", B.Bool b)
                      , (T.pack "null", B.Null)
                      ]
                  )
          decode (encode val) === Right val
      ]


edgeCases :: Spec
edgeCases =
  describe "Edge cases" $
    sequence_
      [ it "Empty document" $ do
          let val = B.Document V.empty
          decode (encode val) `shouldBe` Right val
      , it "Empty string" $ do
          let val = B.Document (V.singleton (T.pack "s", B.String T.empty))
          decode (encode val) `shouldBe` Right val
      , it "Empty binary" $ do
          let val = B.Document (V.singleton (T.pack "b", B.Binary 0x00 BS.empty))
          decode (encode val) `shouldBe` Right val
      , it "Empty array" $ do
          let val = B.Document (V.singleton (T.pack "a", B.Array V.empty))
          decode (encode val) `shouldBe` Right val
      , it "Nested arrays" $ do
          let val =
                B.Document
                  ( V.singleton
                      ( T.pack "a"
                      , B.Array (V.singleton (B.Array (V.singleton (B.Int32 42))))
                      )
                  )
          decode (encode val) `shouldBe` Right val
      , it "Multiple fields" $ do
          let val =
                B.Document
                  ( V.fromList
                      [ (T.pack "a", B.Int32 1)
                      , (T.pack "b", B.String (T.pack "two"))
                      , (T.pack "c", B.Bool True)
                      , (T.pack "d", B.Null)
                      ]
                  )
          decode (encode val) `shouldBe` Right val
      , it "Decode empty input" $
          case decode BS.empty of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on empty input"
      , it "Rejects document lengths below the five-byte minimum" $
          mapM_
            (\size -> do
                -- A leading byte makes the old length-zero underread reproducible.
                let bytes = BS.drop 1 (BS.pack [0, size, 0, 0, 0, 0])
                decode bytes `shouldBe` Left "BSON.Decode: invalid document length"
            )
            [0 .. 4]
      , it "Rejects negative document lengths" $
          decode (BS.pack [0xff, 0xff, 0xff, 0xff, 0])
            `shouldBe` Left "BSON.Decode: invalid document length"
      , it "Rejects zero-length embedded documents" $
          decode (BS.pack [12, 0, 0, 0, 0x03, 0x78, 0, 0, 0, 0, 0, 0])
            `shouldBe` Left "BSON.Decode: invalid document length"
      ]


wireFormatTests :: Spec
wireFormatTests =
  describe "Wire format" $
    sequence_
      [ it "Empty document is 5 bytes" $ do
          let bs = encode (B.Document V.empty)
          BS.length bs `shouldBe` 5
          BS.unpack bs `shouldBe` [5, 0, 0, 0, 0]
      , it "Document size is first 4 LE bytes" $ do
          let bs = encode (B.Document V.empty)
              sz = BS.index bs 0
          sz `shouldBe` 5
      , it "Document ends with 0x00" $ do
          let bs = encode (B.Document V.empty)
          BS.last bs `shouldBe` 0x00
      , it "Bool true type tag is 0x08" $ do
          let bs = encode (B.Document (V.singleton (T.pack "b", B.Bool True)))
          BS.index bs 4 `shouldBe` 0x08
      ]


newTypeTests :: Spec
newTypeTests =
  describe "New BSON types" $
    sequence_
      [ it "Undefined roundtrip" $ do
          let val = B.Document (V.singleton (T.pack "u", B.Undefined))
          decode (encode val) `shouldBe` Right val
      , it "MinKey roundtrip" $ do
          let val = B.Document (V.singleton (T.pack "mk", B.MinKey))
          decode (encode val) `shouldBe` Right val
      , it "MaxKey roundtrip" $ do
          let val = B.Document (V.singleton (T.pack "mk", B.MaxKey))
          decode (encode val) `shouldBe` Right val
      , it "JavaScript roundtrip" $ do
          let val = B.Document (V.singleton (T.pack "js", B.JavaScript (T.pack "function(){}")))
          decode (encode val) `shouldBe` Right val
      , it "Symbol roundtrip" $ do
          let val = B.Document (V.singleton (T.pack "sym", B.Symbol (T.pack "mySymbol")))
          decode (encode val) `shouldBe` Right val
      , it "Timestamp roundtrip" $ do
          let val = B.Document (V.singleton (T.pack "ts", B.Timestamp 6832747927879254017))
          decode (encode val) `shouldBe` Right val
      , it "Decimal128 roundtrip" $ do
          let d128 = BS.pack [0 .. 15]
              val = B.Document (V.singleton (T.pack "d", B.Decimal128 d128))
          decode (encode val) `shouldBe` Right val
      , it "JavaScriptScope roundtrip" $ do
          let scope = B.Document (V.singleton (T.pack "x", B.Int32 42))
              val = B.Document (V.singleton (T.pack "jss", B.JavaScriptScope (T.pack "return x") scope))
          decode (encode val) `shouldBe` Right val
      , it "Binary with subtype roundtrip" $ do
          let val = B.Document (V.singleton (T.pack "b", B.Binary 0x04 (BS.pack [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16])))
          decode (encode val) `shouldBe` Right val
      , it "Binary subtype 0x00 (generic)" $ do
          let val = B.Document (V.singleton (T.pack "b", B.Binary 0x00 (BS.pack [0xDE, 0xAD])))
          decode (encode val) `shouldBe` Right val
      , it "Binary subtype preserved" $ do
          let val = B.Document (V.singleton (T.pack "b", B.Binary 0x05 (BS.pack [1, 2, 3])))
              Right (B.Document fields) = decode (encode val)
              (_, B.Binary sub _) = V.head fields
          sub `shouldBe` 0x05
      , it "MinKey type tag is 0xFF" $ do
          let bs = encode (B.Document (V.singleton (T.pack "mk", B.MinKey)))
          BS.index bs 4 `shouldBe` 0xFF
      , it "MaxKey type tag is 0x7F" $ do
          let bs = encode (B.Document (V.singleton (T.pack "mk", B.MaxKey)))
          BS.index bs 4 `shouldBe` 0x7F
      , it "Multiple new types in one document" $ do
          let val =
                B.Document
                  ( V.fromList
                      [ (T.pack "js", B.JavaScript (T.pack "1+1"))
                      , (T.pack "ts", B.Timestamp 100)
                      , (T.pack "mk", B.MinKey)
                      , (T.pack "MK", B.MaxKey)
                      , (T.pack "u", B.Undefined)
                      , (T.pack "sym", B.Symbol (T.pack "x"))
                      ]
                  )
          decode (encode val) `shouldBe` Right val
      ]
