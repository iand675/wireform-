{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Class (tests) where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft, isRight)
import Data.Int (Int64, Int8)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import Numeric.Natural (Natural)
import Test.Syd
import YAML.Class qualified as C
import YAML.Decode qualified as D
import YAML.JSON qualified as J
import YAML.Value


tests :: Spec
tests = describe "native codecs" $ do
  it "preserves arbitrary precision integers in parsing, typed codecs, and JSON" $ do
    let n = 18446744073709551616
    D.decode "18446744073709551616" `shouldBe` Right (YInt n)
    D.decode "-9223372036854775809" `shouldBe` Right (YInt (-9223372036854775809))
    C.decodeYAML (C.encodeYAML n) `shouldBe` Right (n :: Integer)
    C.decodeYAML (C.encodeYAML (maxBound :: Word64)) `shouldBe` Right (maxBound :: Word64)
    C.decodeYAML (C.encodeYAML (fromInteger n :: Natural)) `shouldBe` Right (fromInteger n :: Natural)
    J.jsonToYAML (Aeson.Number (fromInteger n)) `shouldBe` YInt n
  it "rejects bounded overflow, negative unsigned values, and fractional integers" $ do
    (C.decodeYAML "256" :: Either String Word8) `shouldSatisfy` isLeft
    (C.decodeYAML "-1" :: Either String Word8) `shouldSatisfy` isLeft
    (C.decodeYAML "128" :: Either String Int8) `shouldSatisfy` isLeft
    (C.decodeYAML "9223372036854775808" :: Either String Int64) `shouldSatisfy` isLeft
    (C.decodeYAML "1.9" :: Either String Int) `shouldSatisfy` isLeft
    (C.decodeYAML ".inf" :: Either String Int) `shouldSatisfy` isLeft
  it "resolves explicitly tagged scalars before typed decoding" $ do
    (C.decodeYAML "!!str 001" :: Either String Text) `shouldBe` Right "001"
    (C.decodeYAML "!!str null" :: Either String Text) `shouldBe` Right "null"
    (C.decodeYAML "!!bool \"false\"" :: Either String Bool) `shouldBe` Right False
    (C.decodeYAML "!!int \"001\"" :: Either String Int) `shouldBe` Right 1
    fmap J.yamlToJSON (D.decode "[!!str 001, !!bool \"false\"]") `shouldBe` Right (Aeson.Array (V.fromList [Aeson.String "001", Aeson.Bool False]))
  it "round-trips arbitrary strict and lazy byte payloads without UTF-8 exceptions" $ do
    let bytes = BS.pack [255, 0, 128, 65]
    (C.decodeYAML (C.encodeYAML bytes) :: Either String BS.ByteString) `shouldBe` Right bytes
    let lazyBytes = BSL.fromStrict bytes
    (C.decodeYAML (C.encodeYAML lazyBytes) :: Either String BSL.ByteString) `shouldBe` Right lazyBytes
    (C.decodeYAML "[256]" :: Either String BS.ByteString) `shouldSatisfy` isLeft
  it "preserves Value metadata through the typeclass identity instance" $ do
    let value = YAnchored (Anchor "a") (YTagged (Tag "!custom") (YString "value"))
    C.fromYAML (C.toYAML value) `shouldBe` Right value
  it "rejects native map key collisions introduced by dropping tag metadata" $ do
    let source = "!a key: 1\n!b key: 2\n"
    (C.decodeYAML source :: Either String (Map.Map Text Int)) `shouldSatisfy` isLeft
    (C.decodeYAML source :: Either String Value) `shouldSatisfy` isRight
    (C.fromYAML (mapping [(YInt 1, YInt 1), (YInt 1, YInt 2)]) :: Either String (IntMap.IntMap Int)) `shouldSatisfy` isLeft
  it "preserves non-finite Float values and signed zero through native conversion" $ do
    let nan = C.decodeYAML ".nan" :: Either String Float
    either (const False) isNaN nan `shouldBe` True
    (C.decodeYAML ".inf" :: Either String Float) `shouldBe` Right (1 / 0)
    (C.decodeYAML "-.inf" :: Either String Float) `shouldBe` Right (-1 / 0)
    either (const False) isNegativeZero (C.decodeYAML (C.encodeYAML (negate 0 :: Float)) :: Either String Float) `shouldBe` True
