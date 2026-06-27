{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.AtomPub (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.Meter as Meter
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.Slug as Slug
import qualified Network.HTTP.Headers.SoapAction as SA
import Test.Syd
import Test.Syd.Hedgehog ()


dropTrailingOws :: ByteString -> ByteString
dropTrailingOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


-- SLUG ----------------------------------------------------------------------

parseSlug :: ByteString -> Either String Slug.Slug
parseSlug bs = case runParser Slug.slugParser bs of
  OK v leftover
    | BS.null leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderSlug :: Slug.Slug -> ByteString
renderSlug = M.toStrictByteString . Slug.renderSlug


unit_slug_parse :: Spec
unit_slug_parse = it "parses an opaque slug verbatim" $
  case parseSlug "The Beach at Vacation" of
    Right (Slug.Slug t) -> t `shouldBe` ST.fromString "The Beach at Vacation"
    other -> error (show other)


unit_slug_render :: Spec
unit_slug_render =
  it "renders a slug verbatim" $
    renderSlug (Slug.Slug (ST.fromString "Photo%20100.png")) `shouldBe` "Photo%20100.png"


slugGen :: Gen ST.ShortText
slugGen = ST.fromString <$> Gen.string (Range.linear 1 24) (Gen.element slugChars)
  where
    slugChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_.~% "


prop_slug_roundtrip :: Property
prop_slug_roundtrip = property $ do
  v <- Slug.Slug <$> forAll slugGen
  let bs = renderSlug v
  case parseSlug bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- SoapAction ----------------------------------------------------------------

parseSoap :: ByteString -> Either String SA.SoapAction
parseSoap bs = case runParser SA.soapActionParser bs of
  OK v leftover
    | BS.null leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderSoap :: SA.SoapAction -> ByteString
renderSoap = M.toStrictByteString . SA.renderSoapAction


unit_soap_parse :: Spec
unit_soap_parse = it "parses the URI out of the quoted SOAPAction value" $
  case parseSoap "\"http://example.org/Order\"" of
    Right (SA.SoapAction u) -> u `shouldBe` ST.fromString "http://example.org/Order"
    other -> error (show other)


unit_soap_render :: Spec
unit_soap_render =
  it "renders the URI inside double quotes" $
    renderSoap (SA.SoapAction (ST.fromString "urn:example:action")) `shouldBe` "\"urn:example:action\""


unit_soap_empty :: Spec
unit_soap_empty = it "parses the empty SOAPAction value" $
  case parseSoap "\"\"" of
    Right (SA.SoapAction u) -> u `shouldBe` ST.fromString ""
    other -> error (show other)


soapGen :: Gen ST.ShortText
soapGen = ST.fromString <$> Gen.string (Range.linear 0 30) (Gen.element uriChars)
  where
    uriChars =
      ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ":/?#[]@!$&'()*+,;=-._~ \"\\"


prop_soap_roundtrip :: Property
prop_soap_roundtrip = property $ do
  v <- SA.SoapAction <$> forAll soapGen
  let bs = renderSoap v
  case parseSoap bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Meter ---------------------------------------------------------------------

parseMeter :: ByteString -> Either String Meter.Meter
parseMeter bs = case runParser Meter.meterParser bs of
  OK v leftover
    | BS.null (dropTrailingOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderMeter :: Meter.Meter -> ByteString
renderMeter = M.toStrictByteString . Meter.renderMeter


unit_meter_parse :: Spec
unit_meter_parse = it "parses a comma list of meter-directives" $
  case parseMeter "count=3, do-report, wont-limit" of
    Right (Meter.Meter ds) ->
      ds
        `shouldBe` [ Meter.MeterDirective (ST.fromString "count") (Just (ST.fromString "3"))
                   , Meter.MeterDirective (ST.fromString "do-report") Nothing
                   , Meter.MeterDirective (ST.fromString "wont-limit") Nothing
                   ]
    other -> error (show other)


unit_meter_render :: Spec
unit_meter_render =
  it "renders directives separated by commas" $
    renderMeter
      ( Meter.Meter
          [ Meter.MeterDirective (ST.fromString "max-uses") (Just (ST.fromString "10"))
          , Meter.MeterDirective (ST.fromString "wont-report") Nothing
          ]
      )
      `shouldBe` "max-uses=10, wont-report"


meterNameGen :: Gen ST.ShortText
meterNameGen = ST.fromString <$> Gen.string (Range.linear 1 12) (Gen.element nameChars)
  where
    nameChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-"


meterValGen :: Gen ST.ShortText
meterValGen = ST.fromString <$> Gen.string (Range.linear 0 12) (Gen.element valChars)
  where
    valChars = ['a' .. 'z'] ++ ['0' .. '9'] ++ "-:/ \"\\"


meterDirGen :: Gen Meter.MeterDirective
meterDirGen = Meter.MeterDirective <$> meterNameGen <*> Gen.maybe meterValGen


meterGen :: Gen Meter.Meter
meterGen = Meter.Meter <$> Gen.list (Range.linear 0 5) meterDirGen


prop_meter_roundtrip :: Property
prop_meter_roundtrip = property $ do
  v <- forAll meterGen
  let bs = renderMeter v
  case parseMeter bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "AtomPub" $
    sequence_
      [ unit_slug_parse
      , unit_slug_render
      , it "slug round-trip" prop_slug_roundtrip
      , unit_soap_parse
      , unit_soap_render
      , unit_soap_empty
      , it "soap-action round-trip" prop_soap_roundtrip
      , unit_meter_parse
      , unit_meter_render
      , it "meter round-trip" prop_meter_roundtrip
      ]
