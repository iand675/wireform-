{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.MiscProvisional (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.AMPCacheTransform as AMP
import qualified Network.HTTP.Headers.ConfigurationContext as CC
import qualified Network.HTTP.Headers.EDIINTFeatures as ED
import qualified Network.HTTP.Headers.HTTP2Settings as H2
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- Helpers ---------------------------------------------------------------------

trimmed :: ByteString -> Bool
trimmed = BS.null . BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseAMP :: ByteString -> Either String AMP.AMPCacheTransform
parseAMP bs = case runParser AMP.ampCacheTransformParser bs of
  OK v leftover
    | trimmed leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


parseCC :: ByteString -> Either String CC.ConfigurationContext
parseCC bs = case runParser CC.configurationContextParser bs of
  OK v leftover
    | trimmed leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


parseED :: ByteString -> Either String ED.EDIINTFeatures
parseED bs = case runParser ED.ediintFeaturesParser bs of
  OK v leftover
    | trimmed leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


parseH2 :: ByteString -> Either String H2.HTTP2Settings
parseH2 bs = case runParser H2.http2SettingsParser bs of
  OK v leftover
    | trimmed leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err e -> Left e


renderAMP :: AMP.AMPCacheTransform -> ByteString
renderAMP = M.toStrictByteString . AMP.renderAMPCacheTransform


renderCC :: CC.ConfigurationContext -> ByteString
renderCC = M.toStrictByteString . CC.renderConfigurationContext


renderED :: ED.EDIINTFeatures -> ByteString
renderED = M.toStrictByteString . ED.renderEDIINTFeatures


renderH2 :: H2.HTTP2Settings -> ByteString
renderH2 = M.toStrictByteString . H2.renderHTTP2Settings


-- Unit tests ------------------------------------------------------------------

unit_amp_parse :: Spec
unit_amp_parse = it "parses an AMP cache transform with version range" $
  case parseAMP "google;v=\"1..100\"" of
    Right v -> AMP.ampCacheTransformValue v `shouldBe` ST.fromString "google;v=\"1..100\""
    other -> error (show other)


unit_amp_render :: Spec
unit_amp_render =
  it "renders AMP cache transform verbatim" $
    renderAMP (AMP.AMPCacheTransform (ST.fromString "google;v=\"1..100\""))
      `shouldBe` "google;v=\"1..100\""


unit_cc_parse :: Spec
unit_cc_parse = it "parses an opaque configuration context token" $
  case parseCC "ctx-7f3a;profile=edge" of
    Right v -> CC.configurationContextValue v `shouldBe` ST.fromString "ctx-7f3a;profile=edge"
    other -> error (show other)


unit_cc_render :: Spec
unit_cc_render =
  it "renders configuration context verbatim" $
    renderCC (CC.ConfigurationContext (ST.fromString "ctx-7f3a;profile=edge"))
      `shouldBe` "ctx-7f3a;profile=edge"


unit_ediint_parse :: Spec
unit_ediint_parse = it "parses a comma-separated feature list" $
  case parseED "CEM, multiple-attachments" of
    Right (ED.EDIINTFeatures (a :| [b])) -> do
      a `shouldBe` ST.fromString "CEM"
      b `shouldBe` ST.fromString "multiple-attachments"
    other -> error (show other)


unit_ediint_render :: Spec
unit_ediint_render =
  it "renders the feature list comma-separated" $
    renderED (ED.EDIINTFeatures (ST.fromString "CEM" :| [ST.fromString "multiple-attachments"]))
      `shouldBe` "CEM, multiple-attachments"


unit_http2_parse :: Spec
unit_http2_parse = it "decodes the base64url SETTINGS payload" $
  case parseH2 "SXMgMyA-IDI_" of
    Right v -> H2.http2SettingsPayload v `shouldBe` "Is 3 > 2?"
    other -> error (show other)


unit_http2_render :: Spec
unit_http2_render =
  it "encodes the payload as unpadded base64url" $
    renderH2 (H2.HTTP2Settings "Is 3 > 2?") `shouldBe` "SXMgMyA-IDI_"


-- Round-trip properties -------------------------------------------------------

opaqueValueGen :: Gen ST.ShortText
opaqueValueGen =
  ST.fromString
    <$> Gen.string
      (Range.linear 1 24)
      (Gen.element (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-._;=\""))


tokenGen :: Gen ST.ShortText
tokenGen =
  ST.fromString
    <$> Gen.string
      (Range.linear 1 12)
      (Gen.element (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_.+"))


prop_amp_roundtrip :: Property
prop_amp_roundtrip = property $ do
  v <- AMP.AMPCacheTransform <$> forAll opaqueValueGen
  case parseAMP (renderAMP v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show v)


prop_cc_roundtrip :: Property
prop_cc_roundtrip = property $ do
  v <- CC.ConfigurationContext <$> forAll opaqueValueGen
  case parseCC (renderCC v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show v)


prop_ediint_roundtrip :: Property
prop_ediint_roundtrip = property $ do
  v <- ED.EDIINTFeatures <$> forAll (Gen.nonEmpty (Range.linear 1 5) tokenGen)
  case parseED (renderED v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show v)


prop_http2_roundtrip :: Property
prop_http2_roundtrip = property $ do
  v <- H2.HTTP2Settings <$> forAll (Gen.bytes (Range.linear 0 36))
  case parseH2 (renderH2 v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show v)


tests :: Spec
tests =
  describe "MiscProvisional" $
    sequence_
      [ unit_amp_parse
      , unit_amp_render
      , unit_cc_parse
      , unit_cc_render
      , unit_ediint_parse
      , unit_ediint_render
      , unit_http2_parse
      , unit_http2_render
      , it "AMP-Cache-Transform round-trip" prop_amp_roundtrip
      , it "Configuration-Context round-trip" prop_cc_roundtrip
      , it "EDIINT-Features round-trip" prop_ediint_roundtrip
      , it "HTTP2-Settings round-trip" prop_http2_roundtrip
      ]
