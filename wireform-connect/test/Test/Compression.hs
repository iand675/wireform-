{-# LANGUAGE OverloadedStrings #-}

module Test.Compression (tests) where

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.Connect.Compression
import Test.Syd
import Test.Syd.Hedgehog ()

tests :: Spec
tests =
  describe "Compression" $ do
    it "gzip round-trips" (roundtrip Gzip)
    it "brotli round-trips" (roundtrip Br)
    it "zstd round-trips" (roundtrip Zstd)
    it "identity round-trips" (roundtrip Identity)
    it "negotiate prefers client order intersected with server" $ do
      negotiate [Identity, Gzip] [Br, Gzip, Identity] `shouldBe` Gzip
      negotiate [Identity, Gzip] [Br] `shouldBe` Identity
      negotiate [Identity, Gzip, Zstd] [Zstd, Gzip] `shouldBe` Zstd
      parseAcceptEncoding "gzip, br , zstd" `shouldBe` [Gzip, Br, Zstd]

roundtrip :: ContentCoding -> Property
roundtrip coding = H.property $ do
  bs <- H.forAll (Gen.bytes (Range.linear 0 2000))
  out <- H.evalIO (decompress coding (compress coding bs))
  out H.=== Right bs
