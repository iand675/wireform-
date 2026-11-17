module Test.ASN1 (asn1Tests) where

import ASN1.Decode (decode)
import ASN1.Encode (encode)
import ASN1.Value
import Control.Exception (ErrorCall, evaluate, try)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text qualified as T
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()


asn1Tests :: Spec
asn1Tests =
  describe "ASN1" $
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
      [ it "Boolean True" $ do
          let val = Boolean True
          decode (encode val) `shouldBe` Right val
      , it "Boolean False" $ do
          let val = Boolean False
          decode (encode val) `shouldBe` Right val
      , it "Integer 0" $ do
          let val = Integer 0
          decode (encode val) `shouldBe` Right val
      , it "Integer 127" $ do
          let val = Integer 127
          decode (encode val) `shouldBe` Right val
      , it "Integer 128" $ do
          let val = Integer 128
          decode (encode val) `shouldBe` Right val
      , it "Integer -1" $ do
          let val = Integer (-1)
          decode (encode val) `shouldBe` Right val
      , it "Integer -128" $ do
          let val = Integer (-128)
          decode (encode val) `shouldBe` Right val
      , it "Integer large positive" $ do
          let val = Integer 123456789
          decode (encode val) `shouldBe` Right val
      , it "Integer large negative" $ do
          let val = Integer (-123456789)
          decode (encode val) `shouldBe` Right val
      , it "Null" $ do
          let val = Null
          decode (encode val) `shouldBe` Right val
      , it "OctetString" $ do
          let val = OctetString (BS.pack [1, 2, 3, 4])
          decode (encode val) `shouldBe` Right val
      , it "UTF8String" $ do
          let val = UTF8String (T.pack "hello world")
          decode (encode val) `shouldBe` Right val
      , it "PrintableString" $ do
          let val = PrintableString (T.pack "test123")
          decode (encode val) `shouldBe` Right val
      , it "IA5String" $ do
          let val = IA5String (T.pack "user@example.com")
          decode (encode val) `shouldBe` Right val
      , it "UTCTime" $ do
          let val = UTCTime (T.pack "230101120000Z")
          decode (encode val) `shouldBe` Right val
      , it "GeneralizedTime" $ do
          let val = GeneralizedTime (T.pack "20230101120000Z")
          decode (encode val) `shouldBe` Right val
      , it "BitString" $ do
          let val = BitString 0 (BS.pack [0xFF, 0xF0])
          decode (encode val) `shouldBe` Right val
      , it "OID 1.2.840.113549" $ do
          let val = OID (V.fromList [1, 2, 840, 113549])
          decode (encode val) `shouldBe` Right val
      , it "OID 2.5.4.3" $ do
          let val = OID (V.fromList [2, 5, 4, 3])
          decode (encode val) `shouldBe` Right val
      , it "encodes the combined first two OID arcs as a base-128 subidentifier" $ do
          let check arcs bytes = do
                let val = OID (V.fromList arcs)
                    der = BS.pack bytes
                encode val `shouldBe` der
                decode der `shouldBe` Right val
          check [2, 40, 3] [0x06, 2, 0x78, 3]
          check [2, 999, 3] [0x06, 3, 0x88, 0x37, 3]
          check
            [2, maxBound, maxBound]
            ([0x06, 20, 0x82] ++ replicate 8 0x80 ++ [0x4f, 0x81] ++ replicate 8 0xff ++ [0x7f])
      , it "rejects empty, overlong, truncated and out-of-range OID subidentifiers" $ do
          let bad =
                [ [0x06, 0]
                , [0x06, 2, 0x80, 0x2a]
                , [0x06, 2, 0x2a, 0x80]
                , [0x06, 1, 0x81]
                , [0x06, 10, 0x82] ++ replicate 8 0x80 ++ [0x50]
                , [0x06, 11, 0x2a, 0x82] ++ replicate 8 0x80 ++ [0]
                ]
          mapM_ (\bytes -> isLeft (decode (BS.pack bytes)) `shouldBe` True) bad
      , it "rejects invalid OID arcs on encode" $ do
          let check arcs = do
                result <- try (evaluate (BS.length (encode (OID (V.fromList arcs))))) :: IO (Either ErrorCall Int)
                isLeft result `shouldBe` True
          mapM_ check [[1], [3, 0], [1, 40]]
      , it "Sequence" $ do
          let val = Sequence (V.fromList [Integer 1, Boolean True, Null])
          decode (encode val) `shouldBe` Right val
      , it "Set" $ do
          let val = Set (V.fromList [Integer 42, UTF8String (T.pack "abc")])
          decode (encode val) `shouldBe` Right val
      , it "Nested sequence" $ do
          let inner = Sequence (V.fromList [Integer 1, Integer 2])
              val = Sequence (V.fromList [inner, Boolean True])
          decode (encode val) `shouldBe` Right val
      , it "Tagged context-specific" $ do
          let val = Tagged ContextSpecific 0 (Integer 42)
          decode (encode val) `shouldBe` Right val
      ]


propertyRoundtrips :: Spec
propertyRoundtrips =
  describe "Property roundtrips" $
    sequence_
      [ it "Integer roundtrip" $ property $ do
          n <- forAll $ Gen.integral (Range.linear (-1000000) 1000000)
          let val = Integer n
          decode (encode val) === Right val
      , it "OctetString roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 128)
          let val = OctetString bs
          decode (encode val) === Right val
      , it "UTF8String roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 64) Gen.alphaNum
          let val = UTF8String t
          decode (encode val) === Right val
      , it "Sequence of integers roundtrip" $ property $ do
          ns <- forAll $ Gen.list (Range.linear 0 10) (Gen.integral (Range.linear (-999) 999))
          let val = Sequence (V.fromList (map Integer ns))
          decode (encode val) === Right val
      ]


edgeCases :: Spec
edgeCases =
  describe "Edge cases" $
    sequence_
      [ it "Empty sequence" $ do
          let val = Sequence V.empty
          decode (encode val) `shouldBe` Right val
      , it "Empty set" $ do
          let val = Set V.empty
          decode (encode val) `shouldBe` Right val
      , it "Empty octet string" $ do
          let val = OctetString BS.empty
          decode (encode val) `shouldBe` Right val
      , it "Decode empty input" $
          case decode BS.empty of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on empty input"
      , it "Very large integer" $ do
          let val = Integer (2 ^ (256 :: Int))
          decode (encode val) `shouldBe` Right val
      ]


wireFormatTests :: Spec
wireFormatTests =
  describe "Wire format" $
    sequence_
      [ it "Null is 0x05 0x00" $ do
          let bs = encode Null
          BS.unpack bs `shouldBe` [0x05, 0x00]
      , it "Boolean True is 0x01 0x01 0xFF" $ do
          let bs = encode (Boolean True)
          BS.unpack bs `shouldBe` [0x01, 0x01, 0xFF]
      , it "Boolean False is 0x01 0x01 0x00" $ do
          let bs = encode (Boolean False)
          BS.unpack bs `shouldBe` [0x01, 0x01, 0x00]
      , it "Integer 0 is 0x02 0x01 0x00" $ do
          let bs = encode (Integer 0)
          BS.unpack bs `shouldBe` [0x02, 0x01, 0x00]
      , it "Sequence tag is 0x30" $ do
          let bs = encode (Sequence V.empty)
          BS.index bs 0 `shouldBe` 0x30
      ]
