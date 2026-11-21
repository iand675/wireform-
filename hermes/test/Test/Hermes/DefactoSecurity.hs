{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.DefactoSecurity (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (defaultHeaderSettings, headerMapFromList, lookupHeader, lookupRawHeader, renderHeaderLines, setHeader)
import qualified Network.HTTP.Headers.DNT as DNT
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.HTTP.Headers.XDNSPrefetchControl as XDP
import qualified Network.HTTP.Headers.XDownloadOptions as XDO
import qualified Network.HTTP.Headers.XPermittedCrossDomainPolicies as XP
import qualified Network.HTTP.Headers.XXSSProtection as XX
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Run a parser, requiring all input (modulo trailing OWS) be consumed.
parseOk :: ParserT st String a -> ByteString -> Either String a
parseOk p bs = case runParser p bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


--------------------------------------------------------------------------------
-- X-XSS-Protection
--------------------------------------------------------------------------------

renderXX :: XX.XXSSProtection -> ByteString
renderXX = M.toStrictByteString . XX.renderXXSSProtection


unit_xxss_disabled :: Spec
unit_xxss_disabled = it "parses 0 as disabled" $
  case parseOk XX.xXSSProtectionParser "0" of
    Right XX.XXSSDisabled -> pure () :: IO ()
    other -> error (show other)


unit_xxss_enabled_bare :: Spec
unit_xxss_enabled_bare = it "parses 1 as enabled without directives" $
  case parseOk XX.xXSSProtectionParser "1" of
    Right (XX.XXSSEnabled False Nothing) -> pure () :: IO ()
    other -> error (show other)


unit_xxss_mode_block :: Spec
unit_xxss_mode_block = it "parses 1; mode=block" $
  case parseOk XX.xXSSProtectionParser "1; mode=block" of
    Right (XX.XXSSEnabled True Nothing) -> pure () :: IO ()
    other -> error (show other)


unit_xxss_full :: Spec
unit_xxss_full = it "parses mode=block plus report uri" $
  case parseOk XX.xXSSProtectionParser "1; mode=block; report=https://r.example/xss" of
    Right (XX.XXSSEnabled True (Just u)) -> u `shouldBe` ST.fromString "https://r.example/xss"
    other -> error (show other)


unit_xxss_render :: Spec
unit_xxss_render = it "renders disabled and full forms" $ do
  renderXX XX.XXSSDisabled `shouldBe` "0"
  renderXX (XX.XXSSEnabled False Nothing) `shouldBe` "1"
  renderXX (XX.XXSSEnabled True (Just (ST.fromString "https://r.example/xss")))
    `shouldBe` "1; mode=block; report=https://r.example/xss"


reportGen :: Gen ST.ShortText
reportGen = do
  prefix <- Gen.element ["/", "../", "https://example.test/", "?event=", "#"]
  suffix <- ST.fromString <$> Gen.string (Range.linear 1 16) (Gen.element uriChars)
  pure (prefix <> suffix <> "%2F")
  where
    uriChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "/.-_~"


xxssGen :: Gen XX.XXSSProtection
xxssGen =
  Gen.choice
    [ pure XX.XXSSDisabled
    , XX.XXSSEnabled <$> Gen.bool <*> Gen.maybe reportGen
    ]


prop_xxss_roundtrip :: Property
prop_xxss_roundtrip = property $ do
  v <- forAll xxssGen
  let bs = renderXX v
  case parseOk XX.xXSSProtectionParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


unit_xxss_typed :: Spec
unit_xxss_typed = it "checks directive uniqueness and validates the reporting URI-reference" $ do
  let field values = headerMapFromList (map (\bytes -> ("X-XSS-Protection", bytes)) values)
      value = XX.XXSSEnabled True (Just "../reports/x%2Fy?value=ok#latest")
  lookupHeader @XX.XXSSProtection (field ["\t 1 ; REPORT = ../reports/x%2Fy?value=ok#latest ; MoDe=BLOCK ; \t"])
    `shouldBe` Right (Just value)
  lookupHeader @XX.XXSSProtection (field ["1;\t "]) `shouldBe` Right (Just (XX.XXSSEnabled False Nothing))
  lookupRawHeader "X-XSS-Protection" (setHeader value (field []))
    `shouldBe` Just ("1; mode=block; report=../reports/x%2Fy?value=ok#latest" :| [])
  mapM_
    (\bad -> lookupHeader @XX.XXSSProtection (field bad) `shouldSatisfy` isLeft)
    [ ["0", "1"]
    , ["1", "1"]
    , ["0;mode=block"]
    , ["1; mode=block; MODE=block"]
    , ["1;report=/a;Report=/b"]
    , ["1;mode=invalid"]
    , ["1;report="]
    , ["1;;"]
    , ["1;other=ignore"]
    , ["1;report=/bad%2"]
    , ["1;report=http://[bad]"]
    , ["1;report=/x\r"]
    , ["1;report=\"/x\""]
    , ["1;report=/file\xff"]
    ]


--------------------------------------------------------------------------------
-- X-Download-Options
--------------------------------------------------------------------------------

unit_xdo_parse :: Spec
unit_xdo_parse = it "parses noopen" $
  case parseOk XDO.xDownloadOptionsParser "noopen" of
    Right XDO.NoOpen -> pure () :: IO ()
    other -> error (show other)


unit_xdo_render :: Spec
unit_xdo_render =
  it "renders noopen" $
    M.toStrictByteString (XDO.renderXDownloadOptions XDO.NoOpen) `shouldBe` "noopen"


unit_xdo_typed :: Spec
unit_xdo_typed = it "requires a single complete noopen token" $ do
  let field values = headerMapFromList (map (\bytes -> ("X-Download-Options", bytes)) values)
  lookupHeader @XDO.XDownloadOptions (field [" \tnoopen \t"]) `shouldBe` Right (Just XDO.NoOpen)
  mapM_
    (\bad -> lookupHeader @XDO.XDownloadOptions (field bad) `shouldSatisfy` isLeft)
    [["noopen", "noopen"], ["noopen, noopen"], ["noopen-now"], ["noopen\r"], ["NOOPEN"]]
  lookupRawHeader "X-Download-Options" (setHeader XDO.NoOpen (field [])) `shouldBe` Just ("noopen" :| [])


--------------------------------------------------------------------------------
-- X-Permitted-Cross-Domain-Policies
--------------------------------------------------------------------------------

renderPcdp :: XP.XPermittedCrossDomainPolicies -> ByteString
renderPcdp = M.toStrictByteString . XP.renderXPermittedCrossDomainPolicies


unit_pcdp_parse :: Spec
unit_pcdp_parse = it "parses representative keywords" $ do
  case parseOk XP.xPermittedCrossDomainPoliciesParser "none" of
    Right XP.PcdpNone -> pure () :: IO ()
    other -> error (show other)
  case parseOk XP.xPermittedCrossDomainPoliciesParser "by-content-type" of
    Right XP.PcdpByContentType -> pure () :: IO ()
    other -> error (show other)


unit_pcdp_render :: Spec
unit_pcdp_render = it "renders keywords" $ do
  renderPcdp XP.PcdpMasterOnly `shouldBe` "master-only"
  renderPcdp XP.PcdpByFtpFilename `shouldBe` "by-ftp-filename"
  renderPcdp XP.PcdpAll `shouldBe` "all"


pcdpGen :: Gen XP.XPermittedCrossDomainPolicies
pcdpGen =
  Gen.element
    [ XP.PcdpNone
    , XP.PcdpMasterOnly
    , XP.PcdpByContentType
    , XP.PcdpByFtpFilename
    , XP.PcdpAll
    , XP.PcdpNoneThisResponse
    , XP.PcdpWithNoneThisResponse XP.MetaPolicyMasterOnly
    , XP.PcdpWithNoneThisResponse XP.MetaPolicyNone
    ]


prop_pcdp_roundtrip :: Property
prop_pcdp_roundtrip = property $ do
  v <- forAll pcdpGen
  let bs = renderPcdp v
  case parseOk XP.xPermittedCrossDomainPoliciesParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


unit_pcdp_typed :: Spec
unit_pcdp_typed = it "combines one meta-policy with the response-only directive" $ do
  let field values = headerMapFromList (map (\bytes -> ("X-Permitted-Cross-Domain-Policies", bytes)) values)
      value = XP.PcdpWithNoneThisResponse XP.MetaPolicyMasterOnly
  lookupHeader @XP.XPermittedCrossDomainPolicies (field ["\t none-this-response\t "]) `shouldBe` Right (Just XP.PcdpNoneThisResponse)
  mapM_
    (\wire -> lookupHeader @XP.XPermittedCrossDomainPolicies (field wire) `shouldBe` Right (Just value))
    [["master-only", "none-this-response"], ["none-this-response", "master-only"], ["master-only; none-this-response"], ["none-this-response, master-only"]]
  lookupRawHeader "X-Permitted-Cross-Domain-Policies" (setHeader value (field []))
    `shouldBe` Just ("master-only; none-this-response" :| [])
  mapM_
    (\bad -> lookupHeader @XP.XPermittedCrossDomainPolicies (field bad) `shouldSatisfy` isLeft)
    [["all", "none"], ["all", "all"], ["none-this-response", "none-this-response"], ["none-this"], ["master-only;"], ["none-this-response, master-only, all"]]


--------------------------------------------------------------------------------
-- X-DNS-Prefetch-Control
--------------------------------------------------------------------------------

unit_xdp_parse :: Spec
unit_xdp_parse = it "parses on/off" $ do
  case parseOk XDP.xDNSPrefetchControlParser "on" of
    Right (XDP.XDNSPrefetchControl True) -> pure () :: IO ()
    other -> error (show other)
  case parseOk XDP.xDNSPrefetchControlParser "off" of
    Right (XDP.XDNSPrefetchControl False) -> pure () :: IO ()
    other -> error (show other)


unit_xdp_render :: Spec
unit_xdp_render = it "renders on/off" $ do
  M.toStrictByteString (XDP.renderXDNSPrefetchControl (XDP.XDNSPrefetchControl True)) `shouldBe` "on"
  M.toStrictByteString (XDP.renderXDNSPrefetchControl (XDP.XDNSPrefetchControl False)) `shouldBe` "off"


unit_xdp_typed :: Spec
unit_xdp_typed = it "folds ASCII keyword case and rejects repeated lines" $ do
  let field values = headerMapFromList (map (\bytes -> ("X-DNS-Prefetch-Control", bytes)) values)
  lookupHeader @XDP.XDNSPrefetchControl (field ["\t On "]) `shouldBe` Right (Just (XDP.XDNSPrefetchControl True))
  lookupHeader @XDP.XDNSPrefetchControl (field [" OfF \t"]) `shouldBe` Right (Just (XDP.XDNSPrefetchControl False))
  mapM_
    (\bad -> lookupHeader @XDP.XDNSPrefetchControl (field bad) `shouldSatisfy` isLeft)
    [["on", "off"], ["off", "off"], ["off, on"], ["onward"], ["off\r"]]
  lookupRawHeader "X-DNS-Prefetch-Control" (setHeader (XDP.XDNSPrefetchControl False) (field [])) `shouldBe` Just ("off" :| [])


--------------------------------------------------------------------------------
-- DNT
--------------------------------------------------------------------------------

unit_dnt_parse :: Spec
unit_dnt_parse = it "parses 0/1" $ do
  case parseOk DNT.dNTParser "1" of
    Right (DNT.DNT True) -> pure () :: IO ()
    other -> error (show other)
  case parseOk DNT.dNTParser "0" of
    Right (DNT.DNT False) -> pure () :: IO ()
    other -> error (show other)


unit_dnt_render :: Spec
unit_dnt_render = it "renders 0/1" $ do
  M.toStrictByteString (DNT.renderDNT (DNT.DNT True)) `shouldBe` "1"
  M.toStrictByteString (DNT.renderDNT (DNT.DNT False)) `shouldBe` "0"


unit_dnt_field_lines :: Spec
unit_dnt_field_lines = it "retains a valid extension suffix on either preference and enforces one line" $ do
  let field values = headerMapFromList (map (\wire -> ("DNT", wire)) values)
      suffix = "abc:/[];!%_~"
      check preference digit = case DNT.dntWithExtension preference suffix of
        Nothing -> expectationFailure "valid DNT extension failed validation"
        Just value -> do
          DNT.dntDoNotTrack value `shouldBe` preference
          DNT.dntExtension value `shouldBe` suffix
          let wire = digit <> ST.toByteString suffix
          lookupHeader @DNT.DNT (field [" \t" <> wire <> "\t "]) `shouldBe` Right (Just value)
          lookupRawHeader "DNT" (setHeader value (field [])) `shouldBe` Just (wire :| [])
  check True "1"
  check False "0"
  lookupHeader @DNT.DNT (field []) `shouldBe` Right Nothing
  mapM_
    (\bad -> lookupHeader @DNT.DNT (field bad) `shouldSatisfy` isLeft)
    [["1", "1"], ["1", "0"], [""], ["2abc"], ["1a b"], ["1a\""], ["1a\\"], ["1a,foo"], ["1a\xff"]]
  mapM_ (\suffixBytes -> DNT.dntWithExtension True suffixBytes `shouldBe` Nothing) ["\t", "\"", "\\", ",", "é"]


checkedXXSSWrites :: Spec
checkedXXSSWrites = it "checks authored X-XSS-Protection reporting references" $ do
  let old = headerMapFromList [("X-XSS-Protection", "0")]
  mapM_ (\endpoint -> do
    let value = XX.XXSSEnabled True (Just endpoint)
    renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
    setHeader value old `shouldBe` old) ["", "report path", "/x;mode=block", "/bad%XZ"]
  renderHeaderLines defaultHeaderSettings (XX.XXSSEnabled True (Just "../r%3B#here"))
    `shouldBe` Just ("1; mode=block; report=../r%3B#here" :| [])


tests :: Spec
tests =
  describe "DefactoSecurity" $ do
    describe "X-XSS-Protection" $
      sequence_
        [ checkedXXSSWrites
        , unit_xxss_disabled
        , unit_xxss_enabled_bare
        , unit_xxss_mode_block
        , unit_xxss_full
        , unit_xxss_render
        , unit_xxss_typed
        , it "round-trips" prop_xxss_roundtrip
        ]
    describe "X-Download-Options" $
      sequence_
        [ unit_xdo_parse
        , unit_xdo_render
        , unit_xdo_typed
        ]
    describe "X-Permitted-Cross-Domain-Policies" $
      sequence_
        [ unit_pcdp_parse
        , unit_pcdp_render
        , unit_pcdp_typed
        , it "round-trips" prop_pcdp_roundtrip
        ]
    describe "X-DNS-Prefetch-Control" $
      sequence_
        [ unit_xdp_parse
        , unit_xdp_render
        , unit_xdp_typed
        ]
    describe "DNT" $
      sequence_
        [ unit_dnt_parse
        , unit_dnt_render
        , unit_dnt_field_lines
        ]
