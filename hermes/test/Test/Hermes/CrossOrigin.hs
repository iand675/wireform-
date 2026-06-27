{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.CrossOrigin (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.CrossOriginEmbedderPolicy as COEP
import qualified Network.HTTP.Headers.CrossOriginEmbedderPolicyReportOnly as COEPRO
import qualified Network.HTTP.Headers.CrossOriginOpenerPolicy as COOP
import qualified Network.HTTP.Headers.CrossOriginOpenerPolicyReportOnly as COOPRO
import qualified Network.HTTP.Headers.CrossOriginResourcePolicy as CORP
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.OriginAgentCluster as OAC
import Network.HTTP.Headers.Parsing.Util (ParserT, RFC8941String (..), Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- Generic helpers ----------------------------------------------------------

parseWith :: ParserT () String a -> ByteString -> Either String a
parseWith p bs = case runParser p bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderWith :: (a -> M.Builder) -> a -> ByteString
renderWith f = M.toStrictByteString . f


-- A generated reporting-endpoint name for the @report-to@ parameter.
reportToGen :: Gen (Maybe RFC8941String)
reportToGen =
  Gen.maybe (RFC8941String . ST.fromString <$> Gen.string (Range.linear 1 12) endpointChar)
  where
    endpointChar = Gen.element ("abcdefghijklmnopqrstuvwxyz0123456789-" :: String)


-- Cross-Origin-Embedder-Policy ---------------------------------------------

coepGen :: Gen COEP.CrossOriginEmbedderPolicy
coepGen =
  COEP.CrossOriginEmbedderPolicy
    <$> Gen.element [COEP.CoepUnsafeNone, COEP.CoepRequireCorp, COEP.CoepCredentialless]
    <*> reportToGen


unit_coep_parse :: Spec
unit_coep_parse = it "COEP parses require-corp with report-to" $
  case parseWith COEP.crossOriginEmbedderPolicyParser "require-corp;report-to=\"default\"" of
    Right (COEP.CrossOriginEmbedderPolicy COEP.CoepRequireCorp (Just (RFC8941String e))) ->
      e `shouldBe` ST.fromString "default"
    other -> error (show other)


unit_coep_render :: Spec
unit_coep_render =
  it "COEP renders credentialless" $
    renderWith
      COEP.renderCrossOriginEmbedderPolicy
      (COEP.CrossOriginEmbedderPolicy COEP.CoepCredentialless Nothing)
      `shouldBe` "credentialless"


prop_coep_roundtrip :: Property
prop_coep_roundtrip = property $ do
  v <- forAll coepGen
  let bs = renderWith COEP.renderCrossOriginEmbedderPolicy v
  case parseWith COEP.crossOriginEmbedderPolicyParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Cross-Origin-Embedder-Policy-Report-Only ---------------------------------

unit_coep_ro_parse :: Spec
unit_coep_ro_parse = it "COEP-Report-Only parses unsafe-none" $
  case parseWith COEPRO.crossOriginEmbedderPolicyReportOnlyParser "unsafe-none" of
    Right (COEPRO.CrossOriginEmbedderPolicyReportOnly (COEP.CrossOriginEmbedderPolicy COEP.CoepUnsafeNone Nothing)) ->
      pure () :: IO ()
    other -> error (show other)


unit_coep_ro_render :: Spec
unit_coep_ro_render =
  it "COEP-Report-Only renders require-corp" $
    renderWith
      COEPRO.renderCrossOriginEmbedderPolicyReportOnly
      (COEPRO.CrossOriginEmbedderPolicyReportOnly (COEP.CrossOriginEmbedderPolicy COEP.CoepRequireCorp Nothing))
      `shouldBe` "require-corp"


prop_coep_ro_roundtrip :: Property
prop_coep_ro_roundtrip = property $ do
  v <- COEPRO.CrossOriginEmbedderPolicyReportOnly <$> forAll coepGen
  let bs = renderWith COEPRO.renderCrossOriginEmbedderPolicyReportOnly v
  case parseWith COEPRO.crossOriginEmbedderPolicyReportOnlyParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Cross-Origin-Opener-Policy -----------------------------------------------

coopGen :: Gen COOP.CrossOriginOpenerPolicy
coopGen =
  COOP.CrossOriginOpenerPolicy
    <$> Gen.element
      [ COOP.CoopUnsafeNone
      , COOP.CoopSameOrigin
      , COOP.CoopSameOriginAllowPopups
      , COOP.CoopNoopenerAllowPopups
      ]
    <*> reportToGen


unit_coop_parse :: Spec
unit_coop_parse = it "COOP parses same-origin-allow-popups" $
  case parseWith COOP.crossOriginOpenerPolicyParser "same-origin-allow-popups" of
    Right (COOP.CrossOriginOpenerPolicy COOP.CoopSameOriginAllowPopups Nothing) -> pure () :: IO ()
    other -> error (show other)


unit_coop_parse_report :: Spec
unit_coop_parse_report = it "COOP parses same-origin with report-to" $
  case parseWith COOP.crossOriginOpenerPolicyParser "same-origin;report-to=\"coop\"" of
    Right (COOP.CrossOriginOpenerPolicy COOP.CoopSameOrigin (Just (RFC8941String e))) ->
      e `shouldBe` ST.fromString "coop"
    other -> error (show other)


unit_coop_render :: Spec
unit_coop_render =
  it "COOP renders same-origin with report-to" $
    renderWith
      COOP.renderCrossOriginOpenerPolicy
      (COOP.CrossOriginOpenerPolicy COOP.CoopSameOrigin (Just (RFC8941String (ST.fromString "coop"))))
      `shouldBe` "same-origin;report-to=\"coop\""


prop_coop_roundtrip :: Property
prop_coop_roundtrip = property $ do
  v <- forAll coopGen
  let bs = renderWith COOP.renderCrossOriginOpenerPolicy v
  case parseWith COOP.crossOriginOpenerPolicyParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Cross-Origin-Opener-Policy-Report-Only -----------------------------------

unit_coop_ro_parse :: Spec
unit_coop_ro_parse = it "COOP-Report-Only parses noopener-allow-popups" $
  case parseWith COOPRO.crossOriginOpenerPolicyReportOnlyParser "noopener-allow-popups" of
    Right (COOPRO.CrossOriginOpenerPolicyReportOnly (COOP.CrossOriginOpenerPolicy COOP.CoopNoopenerAllowPopups Nothing)) ->
      pure () :: IO ()
    other -> error (show other)


unit_coop_ro_render :: Spec
unit_coop_ro_render =
  it "COOP-Report-Only renders same-origin" $
    renderWith
      COOPRO.renderCrossOriginOpenerPolicyReportOnly
      (COOPRO.CrossOriginOpenerPolicyReportOnly (COOP.CrossOriginOpenerPolicy COOP.CoopSameOrigin Nothing))
      `shouldBe` "same-origin"


prop_coop_ro_roundtrip :: Property
prop_coop_ro_roundtrip = property $ do
  v <- COOPRO.CrossOriginOpenerPolicyReportOnly <$> forAll coopGen
  let bs = renderWith COOPRO.renderCrossOriginOpenerPolicyReportOnly v
  case parseWith COOPRO.crossOriginOpenerPolicyReportOnlyParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Cross-Origin-Resource-Policy ---------------------------------------------

corpGen :: Gen CORP.CrossOriginResourcePolicy
corpGen = Gen.element [CORP.CorpSameOrigin, CORP.CorpSameSite, CORP.CorpCrossOrigin]


unit_corp_parse :: Spec
unit_corp_parse =
  it "CORP parses same-site" $
    parseWith CORP.crossOriginResourcePolicyParser "same-site" `shouldBe` Right CORP.CorpSameSite


unit_corp_render :: Spec
unit_corp_render =
  it "CORP renders cross-origin" $
    renderWith CORP.renderCrossOriginResourcePolicy CORP.CorpCrossOrigin `shouldBe` "cross-origin"


prop_corp_roundtrip :: Property
prop_corp_roundtrip = property $ do
  v <- forAll corpGen
  let bs = renderWith CORP.renderCrossOriginResourcePolicy v
  case parseWith CORP.crossOriginResourcePolicyParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Origin-Agent-Cluster -----------------------------------------------------

oacGen :: Gen OAC.OriginAgentCluster
oacGen = Gen.element [OAC.OriginAgentClusterEnabled, OAC.OriginAgentClusterDisabled]


unit_oac_parse :: Spec
unit_oac_parse =
  it "Origin-Agent-Cluster parses ?1" $
    parseWith OAC.originAgentClusterParser "?1" `shouldBe` Right OAC.OriginAgentClusterEnabled


unit_oac_render :: Spec
unit_oac_render =
  it "Origin-Agent-Cluster renders ?0" $
    renderWith OAC.renderOriginAgentCluster OAC.OriginAgentClusterDisabled `shouldBe` "?0"


prop_oac_roundtrip :: Property
prop_oac_roundtrip = property $ do
  v <- forAll oacGen
  let bs = renderWith OAC.renderOriginAgentCluster v
  case parseWith OAC.originAgentClusterParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "CrossOrigin" $
    sequence_
      [ unit_coep_parse
      , unit_coep_render
      , it "COEP round-trip" prop_coep_roundtrip
      , unit_coep_ro_parse
      , unit_coep_ro_render
      , it "COEP-Report-Only round-trip" prop_coep_ro_roundtrip
      , unit_coop_parse
      , unit_coop_parse_report
      , unit_coop_render
      , it "COOP round-trip" prop_coop_roundtrip
      , unit_coop_ro_parse
      , unit_coop_ro_render
      , it "COOP-Report-Only round-trip" prop_coop_ro_roundtrip
      , unit_corp_parse
      , unit_corp_render
      , it "CORP round-trip" prop_corp_roundtrip
      , unit_oac_parse
      , unit_oac_render
      , it "Origin-Agent-Cluster round-trip" prop_oac_roundtrip
      ]
