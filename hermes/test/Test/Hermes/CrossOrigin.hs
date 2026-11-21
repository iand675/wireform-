{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Hermes.CrossOrigin (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (HeaderCardinality (..), HeaderRenderingResultToNonEmpty, KnownHeader (..), defaultHeaderSettings, headerMapFromList, insertRawHeader, lookupHeader, lookupRawHeader, renderHeaderLines, setHeader, setRawHeader)
import Network.HTTP.Headers.HeaderFieldName (HeaderFieldName)
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


typedPolicies :: Spec
typedPolicies = describe "typed policy fields" $ do
  it "reads Embedder policy endpoints after arbitrary valid Structured Field parameters" $
    checkField
      (COEP.CrossOriginEmbedderPolicy COEP.CoepRequireCorp (Just (RFC8941String "new\"\\site")))
      "require-corp;report-to=\"new\\\"\\\\site\""
      [ "\t require-corp;report-to=\"old\";sample=0.5;report-to=\"new\\\"\\\\site\" \t"
      , "require-corp;level=token;epoch=@0;data=:YQ==:;display=%\"caf%c3%a9\"; report-to=\"new\\\"\\\\site\";enabled"
      ]
      [ "REQUIRE-CORP"
      , "\"require-corp\""
      , "require-corp ;flag"
      , "require-corp;\tflag"
      , "require-corp;flag="
      , "require-corp;report-to=\"bad\\q\""
      ]
  it "uses the last report-to value and ignores it if it is not an SF String" $
    checkField
      (COEP.CrossOriginEmbedderPolicy COEP.CoepCredentialless Nothing)
      "credentialless"
      ["credentialless;report-to=\"old\";report-to", "credentialless;report-to=\"old\";report-to=token", "credentialless;report-to=?0"]
      ["credentialless;report-To=\"ignored\"", "credentialless;report-to=(\"array\")"]
  it "applies the same Item grammar to Embedder-Policy-Report-Only" $
    checkField
      (COEPRO.CrossOriginEmbedderPolicyReportOnly (COEP.CrossOriginEmbedderPolicy COEP.CoepRequireCorp (Just (RFC8941String ""))))
      "require-corp;report-to=\"\""
      [" require-corp;report-to=\"obsolete\";report-to=\"\";other"]
      ["require-corp, require-corp", "require-corp;report-to= \"space\""]
  it "reads the complete Opener policy token and an endpoint surrounded by extensions" $
    checkField
      (COOP.CrossOriginOpenerPolicy COOP.CoopSameOriginAllowPopups (Just (RFC8941String "group")))
      "same-origin-allow-popups;report-to=\"group\""
      [" same-origin-allow-popups;report-to-alt=foo;report-to=\"group\";limit=999999999999999\t"]
      ["same-origin-plus-coep", "same-origin-allow-popups-extra", "same-origin-allow-popups;limit=1000000000000000"]
  it "applies the same Item grammar to Opener-Policy-Report-Only" $
    checkField
      (COOPRO.CrossOriginOpenerPolicyReportOnly (COOP.CrossOriginOpenerPolicy COOP.CoopSameOrigin Nothing))
      "same-origin"
      ["same-origin;report-to=\"old\";report-to=:YQ==:;sample=?1"]
      ["same-origin;report-to=\"old\";", "same-origin;report-to==\"extra\""]
  it "uses exact Resource policy tokens and accepts field OWS" $
    checkField
      CORP.CorpSameSite
      "same-site"
      ["\t same-site \t"]
      ["SAME-SITE", "same-site;flag", "same-site, same-origin", "same-siteing"]
  it "parses a Boolean Item for Origin-Agent-Cluster" $ do
    checkField
      OAC.OriginAgentClusterEnabled
      "?1"
      [" \t?1;flag;parameter=\"extension\"\t"]
      ["?2", "1", "true", "?1;bad=", "?1;\tflag", "?1, ?0"]
    checkField OAC.OriginAgentClusterDisabled "?0" ["?0;sample=?1"] []
  where
    checkField :: forall a. (KnownHeader a, Cardinality a ~ 'ZeroOrOne, ParseFailure a ~ String, Eq a, Show a) => a -> ByteString -> [ByteString] -> [ByteString] -> IO ()
    checkField value canonical accepted rejected = do
      let name = headerName (Proxy @a)
          empty = headerMapFromList []
          field bytes = insertRawHeader name bytes empty
          lookupOne = lookupHeader @a . field
      lookupRawHeader name (setHeader value empty) `shouldBe` Just (canonical :| [])
      mapM_ (\wire -> lookupOne wire `shouldBe` Right (Just value)) (canonical : accepted)
      mapM_ (\wire -> lookupOne wire `shouldSatisfy` isLeft) rejected
      lookupHeader @a (insertRawHeader name canonical (field canonical)) `shouldSatisfy` isLeft


checkedPolicyWrites :: Spec
checkedPolicyWrites = do
  it "checks authored COEP reporting endpoint strings" $
    mapM_ (reject "Cross-Origin-Embedder-Policy" . coep) malformed
  it "checks authored COEP report-only endpoint strings" $
    mapM_ (reject "Cross-Origin-Embedder-Policy-Report-Only" . COEPRO.CrossOriginEmbedderPolicyReportOnly . coep) malformed
  it "checks authored COOP reporting endpoint strings" $
    mapM_ (reject "Cross-Origin-Opener-Policy" . coop) malformed
  it "checks authored COOP report-only endpoint strings" $
    mapM_ (reject "Cross-Origin-Opener-Policy-Report-Only" . COOPRO.CrossOriginOpenerPolicyReportOnly . coop) malformed
  where
    malformed = map (Just . RFC8941String) ["a\tb", "a\n", "a\DEL", "é"]
    coep = COEP.CrossOriginEmbedderPolicy COEP.CoepRequireCorp
    coop = COOP.CrossOriginOpenerPolicy COOP.CoopSameOrigin
    reject :: (KnownHeader a, HeaderRenderingResultToNonEmpty (Cardinality a)) => HeaderFieldName -> a -> Expectation
    reject name value = do
      let fields = setRawHeader name ("previous" :| []) (headerMapFromList [])
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value fields `shouldBe` fields


tests :: Spec
tests =
  describe "CrossOrigin" $
    sequence_
      [ checkedPolicyWrites
      , unit_coep_parse
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
      , typedPolicies
      ]
