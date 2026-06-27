{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.SecurityMisc (tests) where

import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.ClearSiteData as CSD
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.StrictTransportSecurity as STS
import qualified Network.HTTP.Headers.XContentTypeOptions as XCTO
import qualified Network.HTTP.Headers.XFrameOptions as XFO
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Accept a parse only when all (non-OWS) input was consumed.
runOk :: Result String a -> Either String a
runOk r = case r of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


-- ---------------------------------------------------------------------------
-- Strict-Transport-Security
-- ---------------------------------------------------------------------------

unit_sts_parse :: Spec
unit_sts_parse = it "parses max-age with both flags" $
  case runOk (runParser STS.strictTransportSecurityParser "max-age=31536000; includeSubDomains; preload") of
    Right (STS.StrictTransportSecurity 31536000 True True) -> pure () :: IO ()
    other -> error (show other)


unit_sts_parse_quoted :: Spec
unit_sts_parse_quoted = it "accepts a quoted max-age value" $
  case runOk (runParser STS.strictTransportSecurityParser "max-age=\"600\"") of
    Right (STS.StrictTransportSecurity 600 False False) -> pure () :: IO ()
    other -> error (show other)


unit_sts_render :: Spec
unit_sts_render =
  it "renders max-age with includeSubDomains" $
    let v = STS.StrictTransportSecurity 63072000 True False
    in M.toStrictByteString (STS.renderStrictTransportSecurity v)
        `shouldBe` "max-age=63072000; includeSubDomains"


stsGen :: Gen STS.StrictTransportSecurity
stsGen =
  STS.StrictTransportSecurity
    <$> Gen.word (Range.linear 0 63072000)
    <*> Gen.bool
    <*> Gen.bool


prop_sts_roundtrip :: Property
prop_sts_roundtrip = property $ do
  v <- forAll stsGen
  let bs = M.toStrictByteString (STS.renderStrictTransportSecurity v)
  case runOk (runParser STS.strictTransportSecurityParser bs) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- X-Content-Type-Options
-- ---------------------------------------------------------------------------

unit_xcto_parse :: Spec
unit_xcto_parse = it "parses nosniff" $
  case runOk (runParser XCTO.xContentTypeOptionsParser "nosniff") of
    Right XCTO.NoSniff -> pure () :: IO ()
    other -> error (show other)


unit_xcto_render :: Spec
unit_xcto_render =
  it "renders nosniff" $
    M.toStrictByteString (XCTO.renderXContentTypeOptions XCTO.NoSniff) `shouldBe` "nosniff"


xctoGen :: Gen XCTO.XContentTypeOptions
xctoGen =
  Gen.choice
    [ pure XCTO.NoSniff
    , XCTO.XContentTypeOptionsOther . ST.fromString <$> Gen.element ["sniff", "none", "webkit"]
    ]


prop_xcto_roundtrip :: Property
prop_xcto_roundtrip = property $ do
  v <- forAll xctoGen
  let bs = M.toStrictByteString (XCTO.renderXContentTypeOptions v)
  case runOk (runParser XCTO.xContentTypeOptionsParser bs) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- X-Frame-Options
-- ---------------------------------------------------------------------------

unit_xfo_parse :: Spec
unit_xfo_parse = it "parses SAMEORIGIN" $
  case runOk (runParser XFO.xFrameOptionsParser "SAMEORIGIN") of
    Right XFO.SameOrigin -> pure () :: IO ()
    other -> error (show other)


unit_xfo_render :: Spec
unit_xfo_render =
  it "renders DENY" $
    M.toStrictByteString (XFO.renderXFrameOptions XFO.Deny) `shouldBe` "DENY"


unit_xfo_allowfrom :: Spec
unit_xfo_allowfrom = it "parses and preserves the obsolete ALLOW-FROM form" $ do
  case runOk (runParser XFO.xFrameOptionsParser "ALLOW-FROM https://example.com") of
    Right (XFO.AllowFrom o) -> o `shouldBe` ST.fromString "https://example.com"
    other -> error (show other)
  M.toStrictByteString (XFO.renderXFrameOptions (XFO.AllowFrom (ST.fromString "https://example.com")))
    `shouldBe` "ALLOW-FROM https://example.com"


xfoGen :: Gen XFO.XFrameOptions
xfoGen =
  Gen.choice
    [ pure XFO.Deny
    , pure XFO.SameOrigin
    , XFO.AllowFrom . ST.fromString
        <$> Gen.element ["https://example.com", "https://a.test", "https://b.example:8443"]
    ]


prop_xfo_roundtrip :: Property
prop_xfo_roundtrip = property $ do
  v <- forAll xfoGen
  let bs = M.toStrictByteString (XFO.renderXFrameOptions v)
  case runOk (runParser XFO.xFrameOptionsParser bs) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Clear-Site-Data
-- ---------------------------------------------------------------------------

unit_csd_parse :: Spec
unit_csd_parse = it "parses a quoted directive list" $
  case runOk (runParser CSD.clearSiteDataParser "\"cache\", \"cookies\", \"*\"") of
    Right (CSD.ClearSiteData (CSD.SiteDataCache :| [CSD.SiteDataCookies, CSD.SiteDataWildcard])) ->
      pure () :: IO ()
    other -> error (show other)


unit_csd_render :: Spec
unit_csd_render =
  it "renders a quoted directive list" $
    let v = CSD.ClearSiteData (CSD.SiteDataStorage :| [CSD.SiteDataCache])
    in M.toStrictByteString (CSD.renderClearSiteData v) `shouldBe` "\"storage\", \"cache\""


siteDataGen :: Gen CSD.SiteData
siteDataGen =
  Gen.choice
    [ pure CSD.SiteDataCache
    , pure CSD.SiteDataCookies
    , pure CSD.SiteDataStorage
    , pure CSD.SiteDataExecutionContexts
    , pure CSD.SiteDataWildcard
    , CSD.SiteDataOther . ST.fromString <$> Gen.element ["downloads", "appcache", "clientHints"]
    ]


csdGen :: Gen CSD.ClearSiteData
csdGen = do
  first <- siteDataGen
  rest <- Gen.list (Range.linear 0 4) siteDataGen
  pure (CSD.ClearSiteData (first :| rest))


prop_csd_roundtrip :: Property
prop_csd_roundtrip = property $ do
  v <- forAll csdGen
  let bs = M.toStrictByteString (CSD.renderClearSiteData v)
  case runOk (runParser CSD.clearSiteDataParser bs) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "SecurityMisc" $
    sequence_
      [ unit_sts_parse
      , unit_sts_parse_quoted
      , unit_sts_render
      , it "Strict-Transport-Security round-trip" prop_sts_roundtrip
      , unit_xcto_parse
      , unit_xcto_render
      , it "X-Content-Type-Options round-trip" prop_xcto_roundtrip
      , unit_xfo_parse
      , unit_xfo_render
      , unit_xfo_allowfrom
      , it "X-Frame-Options round-trip" prop_xfo_roundtrip
      , unit_csd_parse
      , unit_csd_render
      , it "Clear-Site-Data round-trip" prop_csd_roundtrip
      ]
