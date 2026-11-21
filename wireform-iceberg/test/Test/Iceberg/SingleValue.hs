module Test.Iceberg.SingleValue (tests) where

import Data.ByteString qualified as BS
import Iceberg.SingleValue
import Iceberg.Types
import Test.Syd
import Prelude hiding (decodeFloat, encodeFloat)


tests :: Spec
tests =
  describe "Iceberg.SingleValue" $
    sequence_
      [ it "encode/decode int round-trips" $ do
          decodeInt32 (encodeInt32 1) `shouldBe` Right 1
          decodeInt32 (encodeInt32 (-1)) `shouldBe` Right (-1)
          decodeInt32 (encodeInt32 maxBound) `shouldBe` Right maxBound
          decodeInt32 (encodeInt32 minBound) `shouldBe` Right minBound
      , it "encode/decode long round-trips" $ do
          decodeInt64 (encodeInt64 0) `shouldBe` Right 0
          decodeInt64 (encodeInt64 (-1)) `shouldBe` Right (-1)
          decodeInt64 (encodeInt64 maxBound) `shouldBe` Right maxBound
      , it "encode/decode float round-trips" $ do
          decodeFloat (encodeFloat 1.5) `shouldBe` Right 1.5
          decodeFloat (encodeFloat 0.0) `shouldBe` Right 0.0
          decodeFloat (encodeFloat (-3.5)) `shouldBe` Right (-3.5)
      , it "encode/decode double round-trips" $ do
          decodeDouble (encodeDouble 1.5) `shouldBe` Right 1.5
          decodeDouble (encodeDouble pi) `shouldBe` Right pi
      , it "encode/decode decimal round-trips" $ do
          decodeDecimal (encodeDecimal 0) `shouldBe` Right 0
          decodeDecimal (encodeDecimal 1) `shouldBe` Right 1
          decodeDecimal (encodeDecimal (-1)) `shouldBe` Right (-1)
          decodeDecimal (encodeDecimal 123456) `shouldBe` Right 123456
          decodeDecimal (encodeDecimal (-123456)) `shouldBe` Right (-123456)
          decodeDecimal (encodeDecimal (10 ^ (30 :: Int))) `shouldBe` Right (10 ^ (30 :: Int))
          encodeDecimal 128 `shouldBe` BS.pack [0x00, 0x80]
          encodeDecimal (-128) `shouldBe` BS.singleton 0x80
          encodeDecimal (-129) `shouldBe` BS.pack [0xff, 0x7f]
      , it "compareSingleValueBy on ints" $ do
          let a = encodeInt32 5
              b = encodeInt32 10
          compareSingleValueBy TInt a b `shouldBe` Right LT
          compareSingleValueBy TInt b a `shouldBe` Right GT
          compareSingleValueBy TInt a a `shouldBe` Right EQ
      , it "decodeInt32 rejects wrong size" $
          case decodeInt32 (BS.pack [0, 0]) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected Left for wrong-sized bytes"
      ]
