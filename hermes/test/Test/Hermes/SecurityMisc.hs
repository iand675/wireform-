{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Test.Hermes.SecurityMisc (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Grammar.Authority (SerializedOrigin (..))
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, headerMapToList, lookupHeader, lookupRawHeader, renderHeaderLines, setHeader)
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


unit_sts_typed :: Spec
unit_sts_typed = it "enforces HSTS directive uniqueness and keeps the first field line" $ do
  let field wire = ("Strict-Transport-Security", wire)
      readSTS = lookupHeader @STS.StrictTransportSecurity . headerMapFromList . map field
      value = STS.StrictTransportSecurity 42 True True
  readSTS [" ;; InClUdEsUbDoMaInS ; ext=\";quoted;\";; maX-aGe = \"\\4\\2\" ; PRELOAD ; ", "max-age=0"]
    `shouldBe` Right (Just value)
  headerMapToList (setHeader value (headerMapFromList []))
    `shouldBe` [("strict-transport-security", "max-age=42; includeSubDomains; preload")]
  readSTS ["max-age=31536000"] `shouldBe` Right (Just (STS.StrictTransportSecurity 31_536_000 False False))
  readSTS ["max-age=000000042"] `shouldBe` Right (Just (STS.StrictTransportSecurity 42 False False))
  readSTS ["max-age=42;"] `shouldBe` Right (Just (STS.StrictTransportSecurity 42 False False))
  readSTS ["max-age=18446744073709551615"] `shouldBe` Right (Just (STS.StrictTransportSecurity maxBound False False))
  readSTS ["max-age=18446744073709551616; includeSubDomains"] `shouldBe` Right (Just (STS.StrictTransportSecurity maxBound True False))
  readSTS ["broken", "max-age=1"] `shouldSatisfy` isLeft
  mapM_
    (\wire -> readSTS [wire] `shouldSatisfy` isLeft)
    [ ";;;"
    , "max-age="
    , "max-age=1; MAX-AGE=0"
    , "max-age=1; includeSubDomains; INCLUDESUBDOMAINS"
    , "max-age=1; preload; PRELOAD"
    , "max-age=1; preload=1"
    , "max-age=1; ext=abc; EXT=\"def\""
    , "max-age=1; ext="
    , "max-age=184467440737095516160x"
    , "max-age=\"12\\x\""
    ]


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
    , pure CSD.SiteDataClientHints
    , pure CSD.SiteDataWildcard
    , CSD.SiteDataOther . ST.fromString <$> Gen.element ["downloads", "appcache", "CLIENTHINTS", "a\"b\\c"]
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


typedBrowserFields :: Spec
typedBrowserFields = describe "typed browser response fields" $ do
  it "matches nosniff on the complete singleton token and preserves extensions" $ do
    lookupNoSniff ["\tNoSniFF \t"] `shouldBe` Right (Just XCTO.NoSniff)
    lookupNoSniff ["nosniff-ext"] `shouldBe` Right (Just (XCTO.XContentTypeOptionsOther "nosniff-ext"))
    lookupNoSniff ["nosniff", "nosniff"] `shouldSatisfy` isLeft
    mapM_ (\wire -> lookupNoSniff [wire] `shouldSatisfy` isLeft) ["", "nosniff, sniff", "nosniff;mode", "\"nosniff\"", "nosniff\r"]
    lookupRawHeader "X-Content-Type-Options" (setHeader XCTO.NoSniff empty) `shouldBe` Just ("nosniff" :| [])
  it "uses the first member for Fetch's nosniff decision" $ do
    mapM_
      (\values -> XCTO.hasNoSniff (noSniffField values) `shouldBe` True)
      [["NoSnIFf"], ["nosniff, sniff"], ["nosniff", "sniff"], [" \tnosniff \t,\"ignored"], ["nosniff", ""]]
    mapM_
      (\values -> XCTO.hasNoSniff (noSniffField values) `shouldBe` False)
      [[], [""], ["", "nosniff"], [",nosniff"], ["sniff", "nosniff"], ["\"nosniff\""], ["\"ignored,nosniff"], ["nosniff-ext, nosniff"], ["nosniff\r,"]]
  it "validates legacy ALLOW-FROM syntax and projects only its origin" $ do
    let address = "HtTpS://[2001:db8::1]:08443/path%20data?x=1#top"
        field = XFO.AllowFrom (ST.fromString address)
    lookupFrame [" \tallow-fROM\t HtTpS://[2001:db8::1]:08443/path%20data?x=1#top \t"] `shouldBe` Right (Just field)
    lookupFrame ["ALLOW-FROM https://example.com/"] `shouldBe` Right (Just (XFO.AllowFrom "https://example.com/"))
    XFO.xFrameOptionsOrigin field `shouldBe` Just (SerializedOrigin "HtTpS" "[2001:db8::1]" (Just 8443))
    XFO.xFrameOptionsOrigin (XFO.AllowFrom "arbitrary text") `shouldBe` Nothing
    lookupRawHeader "X-Frame-Options" (setHeader field empty) `shouldBe` Just ("ALLOW-FROM HtTpS://[2001:db8::1]:08443/path%20data?x=1#top" :| [])
  it "rejects repeated framing fields and invalid ALLOW-FROM origins" $ do
    lookupFrame ["\tDeNy "] `shouldBe` Right (Just XFO.Deny)
    lookupFrame ["SameOrigin", "Deny"] `shouldSatisfy` isLeft
    lookupFrame ["DENY", "DENY"] `shouldSatisfy` isLeft
    mapM_
      (\wire -> lookupFrame [wire] `shouldSatisfy` isLeft)
      [ "deny-more"
      , "DENY, SAMEORIGIN"
      , "ALLOW-FROM"
      , "ALLOW-FROM "
      , "ALLOW-FROM= https://a.test"
      , "ALLOW-FROM null"
      , "ALLOW-FROM //a.test"
      , "ALLOW-FROM https://user@a.test"
      , "ALLOW-FROM https://*.a.test"
      , "ALLOW-FROM https://a.test,b.test"
      , "ALLOW-FROM https://a.test:65536"
      , "ALLOW-FROM https://a.test:"
      , "ALLOW-FROM https://a.test/%2"
      ]
  it "combines Clear-Site-Data directives in field order and skips empty slots" $ do
    let field = CSD.ClearSiteData (CSD.SiteDataCache :| [CSD.SiteDataClientHints, CSD.SiteDataOther "CACHE", CSD.SiteDataStorage])
    lookupSiteData [", \"cache\",,", "", "\"clientHints\", \"CACHE\", \"storage\", \t"] `shouldBe` Right (Just field)
    lookupRawHeader "Clear-Site-Data" (setHeader field empty) `shouldBe` Just ("\"cache\", \"clientHints\", \"CACHE\", \"storage\"" :| [])
    lookupSiteData [",", "", "\t ,"] `shouldSatisfy` isLeft
    lookupSiteData ["\"cache\"", "cookies"] `shouldSatisfy` isLeft
  it "retains escaped and opaque Clear-Site-Data extensions" $ do
    let wire = "\"\\cache\", \"a,b\", \"\xff\\\"\""
        field = CSD.ClearSiteData (CSD.SiteDataOtherWireBytes "\\cache" :| [CSD.SiteDataOther "a,b", CSD.SiteDataOtherWireBytes "\xff\\\""])
    lookupSiteData [wire] `shouldBe` Right (Just field)
    lookupRawHeader "Clear-Site-Data" (setHeader field empty) `shouldBe` Just (wire :| [])
    mapM_ (\value -> lookupSiteData [value] `shouldSatisfy` isLeft) ["\"cache\"garbage", "\"cache", "\"a\x7f\"", "\"\\\r\""]
  where
    empty = headerMapFromList []
    noSniffField = headerMapFromList . map ("X-Content-Type-Options",)
    lookupNoSniff = lookupHeader @XCTO.XContentTypeOptions . noSniffField
    lookupFrame = lookupHeader @XFO.XFrameOptions . headerMapFromList . map ("X-Frame-Options",)
    lookupSiteData = lookupHeader @CSD.ClearSiteData . headerMapFromList . map ("Clear-Site-Data",)


tests :: Spec
tests =
  describe "SecurityMisc" $
    sequence_
      [ unit_sts_parse
      , unit_sts_parse_quoted
      , unit_sts_render
      , unit_sts_typed
      , it "Strict-Transport-Security round-trip" prop_sts_roundtrip
      , unit_xcto_parse
      , unit_xcto_render
      , it "X-Content-Type-Options round-trip" prop_xcto_roundtrip
      , it "validates extension X-Content-Type-Options before writing" $ do
          let fields = headerMapFromList [("X-Content-Type-Options", "nosniff")]
          mapM_ (\token -> do
            let value = XCTO.XContentTypeOptionsOther token
            renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
            setHeader value fields `shouldBe` fields) ["", "NoSnIfF", "xx yy", "xx,yy", "é"]
      , unit_xfo_parse
      , unit_xfo_render
      , unit_xfo_allowfrom
      , it "X-Frame-Options round-trip" prop_xfo_roundtrip
      , unit_csd_parse
      , unit_csd_render
      , it "validates authored Clear-Site-Data extension bodies without rewriting wire escapes" $ do
          let fields = headerMapFromList [("Clear-Site-Data", "\"cache\"")]
              wrap item = CSD.ClearSiteData (CSD.SiteDataCache :| [item])
          mapM_ (\item -> do
            let value = wrap item
            renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
            setHeader value fields `shouldBe` fields)
            ([CSD.SiteDataOther "a\DEL", CSD.SiteDataOther "cookies", CSD.SiteDataOther "*"]
              <> map (CSD.SiteDataOtherWireBytes . SBS.toShort) ["a\"b", "a\\", "a\\\DEL", "a\n", "storage"])
          renderHeaderLines defaultHeaderSettings (wrap (CSD.SiteDataOtherWireBytes (SBS.toShort "\\x\xff")))
            `shouldBe` Just ("\"cache\", \"\\x\xff\"" :| [])
      , it "Clear-Site-Data round-trip" prop_csd_roundtrip
      , typedBrowserFields
      ]
