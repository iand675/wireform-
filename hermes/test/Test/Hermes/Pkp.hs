{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Pkp (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, headerMapToList, lookupHeader, setHeader)
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

genB64 :: Gen ST.ShortText
genB64 = do
  prefix <- Gen.string (Range.singleton 42) (Gen.element b64)
  lastSextet <- Gen.element ("AEIMQUYcgkosw048" :: String)
  pure (ST.fromString (prefix ++ [lastSextet, '=']))
  where
    b64 = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "+/"


genUri :: Gen ST.ShortText
genUri = ST.fromString . ("https://report.example/" ++) <$> Gen.string (Range.linear 5 20) (Gen.element uri)
  where
    uri = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-._/:"


genMaxAge :: Gen Word
genMaxAge = Gen.word (Range.linear 0 100_000_000)


genPin :: Gen PKP.Pin
genPin = PKP.Pin "sha256" <$> genB64


genPinRo :: Gen RO.Pin
genPinRo = RO.Pin "sha256" <$> genB64


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
  case ok (runParser PKP.publicKeyPinsParser "pin-sha256=\"E9CZ9INDbd+2eRQozYqqbQ2yXLVKB9+xcprMF+44U1g=\"; pin-sha256=\"LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=\"; max-age=5184000; includeSubDomains; report-uri=\"https://example.net/report\"") of
    Right pkp -> do
      PKP.pkpMaxAge pkp `shouldBe` 5_184_000
      PKP.pkpIncludeSubDomains pkp `shouldBe` True
      PKP.pkpReportUri pkp `shouldBe` Just (ST.fromString "https://example.net/report")
      map PKP.pinAlgorithm (PKP.pkpPins pkp) `shouldBe` [ST.fromString "sha256", ST.fromString "sha256"]
      map PKP.pinFingerprint (PKP.pkpPins pkp) `shouldBe` [ST.fromString "E9CZ9INDbd+2eRQozYqqbQ2yXLVKB9+xcprMF+44U1g=", ST.fromString "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ="]
    Left err -> error err


unit_pkp_render :: Spec
unit_pkp_render =
  it "renders a Public-Key-Pins policy" $
    let v =
          PKP.PublicKeyPins
            [PKP.Pin (ST.fromString "sha256") (ST.fromString "E9CZ9INDbd+2eRQozYqqbQ2yXLVKB9+xcprMF+44U1g=")]
            5_184_000
            True
            (Just (ST.fromString "https://example.net/report"))
    in renderPkp v `shouldBe` "pin-sha256=\"E9CZ9INDbd+2eRQozYqqbQ2yXLVKB9+xcprMF+44U1g=\"; max-age=5184000; includeSubDomains; report-uri=\"https://example.net/report\""


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
  case ok (runParser RO.publicKeyPinsReportOnlyParser "pin-sha256=\"LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=\"; max-age=86400; report-uri=\"https://r.example/p\"") of
    Right p -> do
      RO.pkproMaxAge p `shouldBe` 86_400
      RO.pkproIncludeSubDomains p `shouldBe` False
      RO.pkproReportUri p `shouldBe` Just (ST.fromString "https://r.example/p")
      map RO.pinFingerprint (RO.pkproPins p) `shouldBe` [ST.fromString "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ="]
    Left err -> error err


unit_ro_render :: Spec
unit_ro_render =
  it "renders a Public-Key-Pins-Report-Only policy" $
    let v =
          RO.PublicKeyPinsReportOnly
            [RO.Pin (ST.fromString "sha256") (ST.fromString "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=")]
            86_400
            False
            Nothing
    in renderRo v `shouldBe` "pin-sha256=\"LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ=\"; max-age=86400"


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


unit_pkp_typed :: Spec
unit_pkp_typed = it "validates pin width, pad bits and repeated policy directives through typed lookup" $ do
  let field wire = ("Public-Key-Pins", wire)
      readPins = lookupHeader @PKP.PublicKeyPins . headerMapFromList
      fingerprint = BS.replicate 43 65 <> "="
      pin = "PIN-SHA256=\"" <> fingerprint <> "\""
      expected = PKP.Pin "sha256" (ST.fromString (replicate 43 'A' <> "="))
  readPins [field (pin <> "; pin-shaFuture=\"aWdub3JlZA==\"; " <> pin <> "; MAX-AGE=\"\\4\\2\""), field "broken"]
    `shouldBe` Right (Just (PKP.PublicKeyPins [expected, expected] 42 False Nothing))
  readPins [field "max-age=9", field "max-age=0"]
    `shouldBe` Right (Just (PKP.PublicKeyPins [] 9 False Nothing))
  readPins [field "broken", field "max-age=9"] `shouldSatisfy` isLeft
  mapM_
    (\wire -> readPins [field wire] `shouldSatisfy` isLeft)
    [ pin
    , pin <> "; max-age=1; MAX-AGE=2"
    , pin <> "; max-age=1; includeSubDomains; includesubdomains"
    , pin <> "; max-age=1; future=good; FUTURE=bad"
    , pin <> "; max-age=1; report-uri=\"https://example.net\"; REPORT-URI=\"https://other.net\""
    , pin <> "; max-age=1;"
    , pin <> "; ; max-age=1"
    , "pin-sha256=\"AQ==\"; max-age=1"
    , "pin-sha256=\"" <> BS.replicate 42 65 <> "B=\"; max-age=1"
    , "pin-sha256=\"" <> BS.replicate 44 65 <> "\"; max-age=1"
    , "max-age=1; report-uri=\"/relative\""
    ]


unit_unknown_pins :: Spec
unit_unknown_pins = it "validates the wire syntax of ignored pin algorithms in both policy fields" $ do
  let readPins value = lookupHeader @PKP.PublicKeyPins (headerMapFromList [("Public-Key-Pins", value)])
      readReport value = lookupHeader @RO.PublicKeyPinsReportOnly (headerMapFromList [("Public-Key-Pins-Report-Only", value)])
      policy = "max-age=42; pin-future=\"A\\Q==\"; PIN-FUTURE=\"AAAA\"; report-uri=\"https\\://example.net/ok%20yes#report\""
      report = Just "https://example.net/ok%20yes#report"
  readPins policy `shouldBe` Right (Just (PKP.PublicKeyPins [] 42 False report))
  readReport policy `shouldBe` Right (Just (RO.PublicKeyPinsReportOnly [] 42 False report))
  readPins "max-age=42; report-uri=\"https://example.net/ok%20yes#report\""
    `shouldBe` Right (Just (PKP.PublicKeyPins [] 42 False report))
  mapM_ (\wire -> do
      readPins wire `shouldSatisfy` isLeft
      readReport wire `shouldSatisfy` isLeft)
    [ "max-age=1; pin-future"
    , "max-age=1; pin-future=AAAA"
    , "max-age=1; pin-future=\"\""
    , "max-age=1; pin-future=\"AAA\""
    , "max-age=1; pin-future=\"A===\""
    , "max-age=1; pin-future=\"AB==\""
    , "max-age=1; pin-future=\"AA-_\""
    , "max-age=1; pin-future=\"AA A\""
    ]


unit_ro_without_age :: Spec
unit_ro_without_age = it "reads and writes report-only policies without requiring an age" $ do
  let uri = "https://ct.example/report"
      field bytes = ("Public-Key-Pins-Report-Only", bytes)
      wire = "report-uri=\"https://ct.example/report\""
      readReport = lookupHeader @RO.PublicKeyPinsReportOnly . headerMapFromList
      value = RO.PublicKeyPinsReportOnly [] 0 False (Just uri)
  readReport [field wire, field "max-age=1"] `shouldBe` Right (Just value)
  headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("public-key-pins-report-only", wire)]
  readReport [field "max-age=1; MAX-AGE=2"] `shouldSatisfy` isLeft


unit_ect_typed :: Spec
unit_ect_typed = it "combines Expect-CT field lines before validating the required and unique directives" $ do
  let field wire = ("Expect-CT", wire)
      readCT = lookupHeader @ECT.ExpectCT . headerMapFromList
      value = ECT.ExpectCT maxBound True (Just "https://ct.example")
  readCT [field ", report-uri=\"https://ct.example\",", field " , enforce,", field "MaX-AgE=9999999999999999999999999999999"]
    `shouldBe` Right (Just value)
  readCT [field "ENFORCE, max-age=\"7\\9\"", field "x=\"extension,quoted\""]
    `shouldBe` Right (Just (ECT.ExpectCT 79 True Nothing))
  mapM_
    (\wires -> readCT (map field wires) `shouldSatisfy` isLeft)
    [ ["enforce", "report-uri=\"https://ct.example\""]
    , ["max-age=1", "MAX-AGE=2"]
    , ["max-age=1, enforce", "ENFORCE"]
    , ["max-age=1, extension", "EXTENSION=present"]
    , ["max-age=1x, enforce"]
    , ["max-age=1, enforce=1"]
    , ["max-age=1, report-uri=\"/relative\""]
    , ["max-age=1, report-uri=\"https://ct.example/#fragment\""]
    , ["max-age=1, report-uri=\"https://ct.example/%FG\""]
    , ["max-age=1, report-uri=https://ct.example/report"]
    ]


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
      , unit_pkp_typed
      , unit_unknown_pins
      , unit_ro_without_age
      , unit_ect_typed
      , it "Public-Key-Pins round-trip" prop_pkp_roundtrip
      , it "Public-Key-Pins-Report-Only round-trip" prop_ro_roundtrip
      , it "Expect-CT round-trip" prop_ect_roundtrip
      ]
