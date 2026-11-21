module Test.Iceberg.Transform (tests) where

import Avro.Value qualified as AV
import Data.ByteString qualified as BS
import Iceberg.Transform
import Iceberg.Types
import Test.Syd


tests :: Spec
tests =
  describe "Iceberg.Transform" $
    sequence_
      [ it "Identity returns the value verbatim" $
          applyTransform Identity TInt (AV.Int 7) `shouldBe` Right (AV.Int 7)
      , it "Bucket on int 34 % 16 = 3" $
          applyTransform (Bucket 16) TInt (AV.Int 34) `shouldBe` Right (AV.Int 3)
      , it "Bucket on long 34 % 16 = 3" $
          applyTransform (Bucket 16) TLong (AV.Long 34) `shouldBe` Right (AV.Int 3)
      , it "Truncate(10) on int 23 = 20" $
          applyTransform (Truncate 10) TInt (AV.Int 23) `shouldBe` Right (AV.Int 20)
      , it "Truncate(10) on int -1 = -10 (floor towards -inf)" $
          applyTransform (Truncate 10) TInt (AV.Int (-1)) `shouldBe` Right (AV.Int (-10))
      , it "Truncate string \"iceberg\" by 3 = \"ice\"" $
          applyTransform (Truncate 3) TString (AV.String "iceberg") `shouldBe` Right (AV.String "ice")
      , it "truncates signed decimals at the column scale" $ do
          let truncateDecimal width octets = applyTransform (Truncate width) (TDecimal 9 2) (AV.Bytes (BS.pack octets))
          truncateDecimal 50 [0x04, 0x29] `shouldBe` Right (AV.Bytes (BS.pack [0x04, 0x1a])) -- 10.65 -> 10.50
          truncateDecimal 50 [0xfb, 0xd7] `shouldBe` Right (AV.Bytes (BS.pack [0xfb, 0xb4])) -- -10.65 -> -11.00
          truncateDecimal 10 [0x00, 0x87] `shouldBe` Right (AV.Bytes (BS.pack [0x00, 0x82])) -- 1.35 -> 1.30
      , it "Year of date 0 (1970-01-01) is 0" $
          applyTransform Year TDate (AV.Int 0) `shouldBe` Right (AV.Int 0)
      , it "Year of date 365 (1971-01-01) is 1" $
          applyTransform Year TDate (AV.Int 365) `shouldBe` Right (AV.Int 1)
      , it "Month of date 31 (1970-02-01) is 1" $
          applyTransform Month TDate (AV.Int 31) `shouldBe` Right (AV.Int 1)
      , it "Day of date 1 returns 1 (identity-on-day)" $
          applyTransform Day TDate (AV.Int 1) `shouldBe` Right (AV.Int 1)
      , it "Hour of timestamp 3600000000 (1 hour) = 1" $
          applyTransform Hour TTimestamp (AV.Long (3600 * 1000000)) `shouldBe` Right (AV.Int 1)
      , it "Void always yields null" $
          applyTransform Void TInt (AV.Int 99) `shouldBe` Right AV.Null
      , it "transformResultType picks correct types" $ do
          transformResultType Identity TInt `shouldBe` Just TInt
          transformResultType (Bucket 8) TString `shouldBe` Just TInt
          transformResultType Year TDate `shouldBe` Just TInt
          transformResultType Day TDate `shouldBe` Just TDate
          transformResultType Hour TDate `shouldBe` Nothing
          transformResultType Year TString `shouldBe` Nothing
      ]
