{-# LANGUAGE OverloadedStrings #-}

module Test.Metadata (tests) where

import Data.ByteString (ByteString)
import Hedgehog (Gen)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Network.Connect.Metadata
import Network.GRPC.Spec (CustomMetadata (..), HeaderName (..))
import Test.Syd
import Test.Syd.Hedgehog ()

tests :: Spec
tests =
  describe "Metadata" $ do
    it "ascii leading metadata round-trips" $ H.property $ do
      cms <- H.forAll (Gen.list (Range.linear 0 5) genAscii)
      headersToLeading (leadingToHeaders cms) H.=== cms
    it "binary -bin leading metadata round-trips" $ H.property $ do
      cms <- H.forAll (Gen.list (Range.linear 0 5) genBinary)
      headersToLeading (leadingToHeaders cms) H.=== cms
    it "trailer- prefix round-trips" $ H.property $ do
      cms <- H.forAll (Gen.list (Range.linear 0 5) (Gen.choice [genAscii, genBinary]))
      prefixedHeadersToTrailing (trailingToPrefixedHeaders cms) H.=== cms
    it "reserved headers are dropped" $
      headersToLeading [("content-type", "application/json"), ("connect-timeout-ms", "5"), ("x-foo", "bar")]
        `shouldBe` [CustomMetadata (AsciiHeader "x-foo") "bar"]

-- ASCII header names: lowercase letters only (valid per the grammar).
genName :: Gen ByteString
genName = Gen.utf8 (Range.linear 1 8) Gen.lower

genAscii :: Gen CustomMetadata
genAscii = do
  nm <- genName
  val <- Gen.utf8 (Range.linear 0 20) Gen.alphaNum
  pure (CustomMetadata (AsciiHeader nm) val)

genBinary :: Gen CustomMetadata
genBinary = do
  nm <- genName
  val <- Gen.bytes (Range.linear 0 20)
  pure (CustomMetadata (BinaryHeader (nm <> "-bin")) val)
