{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Pkp (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.ExpectCT as ECT
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.PublicKeyPins as PKP
import qualified Network.HTTP.Headers.PublicKeyPinsReportOnly as RO
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Accept a parse result only when all (non-whitespace) input was consumed.
ok :: Result String a -> Either String a
ok = \case
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderPkp :: PKP.PublicKeyPins -> ByteString
renderPkp = M.toStrictByteString . PKP.renderPublicKeyPins


renderRo :: RO.PublicKeyPinsReportOnly -> ByteString
renderRo = M.toStrictByteString . RO.renderPublicKeyPinsReportOnly


renderEct :: ECT.ExpectCT -> ByteString
renderEct = M.toStrictByteString . ECT.renderExpectCT


-- Generators ---------------------------------------------------------------

genAlg :: Gen ST.ShortText
genAlg = Gen.element (map ST.fromString ["sha256", "sha384", "sha512"])


genB64 :: Gen ST.ShortText
genB64 = ST.fromString <$> Gen.string (Range.linear 8 24) (Gen.element b64)
  where
    b64 = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "+/="


genUri :: Gen ST.ShortText
genUri = ST.fromString . ("https://" ++) <$> Gen.string (Range.linear 5 20) (Gen.element uri)
  where
    uri = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-._/:"


genMaxAge :: Gen Word
genMaxAge = Gen.word (Range.linear 0 100_000_000)


genPin :: Gen PKP.Pin
genPin = PKP.Pin <$> genAlg <*> genB64


genPinRo :: Gen RO.Pin
genPinRo = RO.Pin <$> genAlg <*> genB64


genPkp :: Gen PKP.PublicKeyPins
genPkp =
  PKP.PublicKeyPins
    <$> Gen.list (Range.linear 0 3) genPin
    <*> genMaxAge
    <*> Gen.bool
    <*> Gen.maybe genUri


genRo :: Gen RO.PublicKeyPinsReportOnly
genRo =
  RO.PublicKeyPinsReportOnly
    <$> Gen.list (Range.linear 0 3) genPinRo
    <*> genMaxAge
    <*> Gen.bool
    <*> Gen.maybe genUri


genEct :: Gen ECT.ExpectCT
genEct = ECT.ExpectCT <$> genMaxAge <*> Gen.bool <*> Gen.maybe genUri


-- Public-Key-Pins ----------------------------------------------------------

unit_pkp_parse :: Spec
unit_pkp_parse = it "parses a Public-Key-Pins policy" $
  case ok (runParser PKP.publicKeyPinsParser "pin-sha256=\"abc+/=\"; pin-sha256=\"def123\"; max-age=5184000; includeSubDomains; report-uri=\"https://example.net/report\"") of
    Right pkp -> do
      PKP.pkpMaxAge pkp `shouldBe` 5_184_000
      PKP.pkpIncludeSubDomains pkp `shouldBe` True
      PKP.pkpReportUri pkp `shouldBe` Just (ST.fromString "https://example.net/report")
      map PKP.pinAlgorithm (PKP.pkpPins pkp) `shouldBe` [ST.fromString "sha256", ST.fromString "sha256"]
      map PKP.pinFingerprint (PKP.pkpPins pkp) `shouldBe` [ST.fromString "abc+/=", ST.fromString "def123"]
    Left err -> error err


unit_pkp_render :: Spec
unit_pkp_render =
  it "renders a Public-Key-Pins policy" $
    let v =
          PKP.PublicKeyPins
            [PKP.Pin (ST.fromString "sha256") (ST.fromString "abc==")]
            5_184_000
            True
            (Just (ST.fromString "https://example.net/report"))
    in renderPkp v `shouldBe` "pin-sha256=\"abc==\"; max-age=5184000; includeSubDomains; report-uri=\"https://example.net/report\""


prop_pkp_roundtrip :: Property
prop_pkp_roundtrip = property $ do
  v <- forAll genPkp
  let bs = renderPkp v
  case ok (runParser PKP.publicKeyPinsParser bs) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Public-Key-Pins-Report-Only ----------------------------------------------

unit_ro_parse :: Spec
unit_ro_parse = it "parses a Public-Key-Pins-Report-Only policy" $
  case ok (runParser RO.publicKeyPinsReportOnlyParser "pin-sha256=\"xyz\"; max-age=86400; report-uri=\"https://r.example/p\"") of
    Right p -> do
      RO.pkproMaxAge p `shouldBe` 86_400
      RO.pkproIncludeSubDomains p `shouldBe` False
      RO.pkproReportUri p `shouldBe` Just (ST.fromString "https://r.example/p")
      map RO.pinFingerprint (RO.pkproPins p) `shouldBe` [ST.fromString "xyz"]
    Left err -> error err


unit_ro_render :: Spec
unit_ro_render =
  it "renders a Public-Key-Pins-Report-Only policy" $
    let v =
          RO.PublicKeyPinsReportOnly
            [RO.Pin (ST.fromString "sha256") (ST.fromString "xyz")]
            86_400
            False
            Nothing
    in renderRo v `shouldBe` "pin-sha256=\"xyz\"; max-age=86400"


prop_ro_roundtrip :: Property
prop_ro_roundtrip = property $ do
  v <- forAll genRo
  let bs = renderRo v
  case ok (runParser RO.publicKeyPinsReportOnlyParser bs) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Expect-CT ----------------------------------------------------------------

unit_ect_parse :: Spec
unit_ect_parse = it "parses an Expect-CT policy" $
  case ok (runParser ECT.expectCTParser "max-age=86400, enforce, report-uri=\"https://ct.example/report\"") of
    Right e -> do
      ECT.expectCtMaxAge e `shouldBe` 86_400
      ECT.expectCtEnforce e `shouldBe` True
      ECT.expectCtReportUri e `shouldBe` Just (ST.fromString "https://ct.example/report")
    Left err -> error err


unit_ect_render :: Spec
unit_ect_render =
  it "renders an Expect-CT policy" $
    let v = ECT.ExpectCT 86_400 True (Just (ST.fromString "https://ct.example/report"))
    in renderEct v `shouldBe` "max-age=86400, enforce, report-uri=\"https://ct.example/report\""


prop_ect_roundtrip :: Property
prop_ect_roundtrip = property $ do
  v <- forAll genEct
  let bs = renderEct v
  case ok (runParser ECT.expectCTParser bs) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Pkp" $
    sequence_
      [ unit_pkp_parse
      , unit_pkp_render
      , unit_ro_parse
      , unit_ro_render
      , unit_ect_parse
      , unit_ect_render
      , it "Public-Key-Pins round-trip" prop_pkp_roundtrip
      , it "Public-Key-Pins-Report-Only round-trip" prop_ro_roundtrip
      , it "Expect-CT round-trip" prop_ect_roundtrip
      ]
