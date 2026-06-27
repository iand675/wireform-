{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.DefactoSecurity (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
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
reportGen = ST.fromString <$> Gen.string (Range.linear 1 16) (Gen.element uriChars)
  where
    uriChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "/:.-_?=&%"


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
    ]


prop_pcdp_roundtrip :: Property
prop_pcdp_roundtrip = property $ do
  v <- forAll pcdpGen
  let bs = renderPcdp v
  case parseOk XP.xPermittedCrossDomainPoliciesParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


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


tests :: Spec
tests =
  describe "DefactoSecurity" $ do
    describe "X-XSS-Protection" $
      sequence_
        [ unit_xxss_disabled
        , unit_xxss_enabled_bare
        , unit_xxss_mode_block
        , unit_xxss_full
        , unit_xxss_render
        , it "round-trips" prop_xxss_roundtrip
        ]
    describe "X-Download-Options" $
      sequence_
        [ unit_xdo_parse
        , unit_xdo_render
        ]
    describe "X-Permitted-Cross-Domain-Policies" $
      sequence_
        [ unit_pcdp_parse
        , unit_pcdp_render
        , it "round-trips" prop_pcdp_roundtrip
        ]
    describe "X-DNS-Prefetch-Control" $
      sequence_
        [ unit_xdp_parse
        , unit_xdp_render
        ]
    describe "DNT" $
      sequence_
        [ unit_dnt_parse
        , unit_dnt_render
        ]
