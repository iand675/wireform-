{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Reporting (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.NEL as NEL
import Network.HTTP.Headers.Parsing.Util (RFC8941String (..), Result (..), runParser)
import qualified Network.HTTP.Headers.ReportingEndpoints as RE
import Test.Syd
import Test.Syd.Hedgehog ()


parseNEL :: ByteString -> Either String NEL.NEL
parseNEL bs = case runParser NEL.nelParser bs of
  OK v leftover
    | BS.null leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderNEL :: NEL.NEL -> ByteString
renderNEL = M.toStrictByteString . NEL.renderNEL


parseRE :: ByteString -> Either String RE.ReportingEndpoints
parseRE bs = case runParser RE.reportingEndpointsParser bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderRE :: RE.ReportingEndpoints -> ByteString
renderRE = M.toStrictByteString . RE.renderReportingEndpoints


-- NEL ----------------------------------------------------------------------

unit_nel_parse :: Spec
unit_nel_parse = it "parses a NEL JSON object opaquely" $
  case parseNEL "{\"report_to\":\"default\",\"max_age\":86400}" of
    Right v -> NEL.nelPolicy v `shouldBe` ST.fromString "{\"report_to\":\"default\",\"max_age\":86400}"
    other -> error (show other)


unit_nel_render :: Spec
unit_nel_render =
  it "renders a NEL value verbatim" $
    renderNEL (NEL.NEL (ST.fromString "{\"max_age\":0}")) `shouldBe` "{\"max_age\":0}"


-- Reporting-Endpoints ------------------------------------------------------

unit_re_parse :: Spec
unit_re_parse = it "parses a Reporting-Endpoints dictionary" $
  case parseRE "default=\"https://e.example/r\", backup=\"https://b.example/r\"" of
    Right (RE.ReportingEndpoints [a, b]) -> do
      RE.reportingEndpointName a `shouldBe` ST.fromString "default"
      unsafeToRFC8941String (RE.reportingEndpointUrl a) `shouldBe` ST.fromString "https://e.example/r"
      RE.reportingEndpointName b `shouldBe` ST.fromString "backup"
      unsafeToRFC8941String (RE.reportingEndpointUrl b) `shouldBe` ST.fromString "https://b.example/r"
    other -> error (show other)


unit_re_render :: Spec
unit_re_render =
  it "renders a Reporting-Endpoints dictionary" $
    let v =
          RE.ReportingEndpoints
            [ RE.ReportingEndpoint (ST.fromString "default") (RFC8941String (ST.fromString "https://e.example/r"))
            , RE.ReportingEndpoint (ST.fromString "backup") (RFC8941String (ST.fromString "https://b.example/r"))
            ]
    in renderRE v `shouldBe` "default=\"https://e.example/r\", backup=\"https://b.example/r\""


keyGen :: Gen ST.ShortText
keyGen = do
  c <- Gen.element ['a' .. 'z']
  cs <- Gen.list (Range.linear 0 8) (Gen.element (['a' .. 'z'] <> ['0' .. '9'] <> "-_."))
  pure (ST.fromString (c : cs))


urlGen :: Gen RFC8941String
urlGen = do
  s <- Gen.list (Range.linear 1 30) (Gen.element urlChars)
  pure (RFC8941String (ST.fromString s))
  where
    urlChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ":/._-?=&%"


endpointGen :: Gen RE.ReportingEndpoint
endpointGen = RE.ReportingEndpoint <$> keyGen <*> urlGen


reGen :: Gen RE.ReportingEndpoints
reGen = RE.ReportingEndpoints <$> Gen.list (Range.linear 1 5) endpointGen


prop_re_roundtrip :: Property
prop_re_roundtrip = property $ do
  v <- forAll reGen
  let bs = renderRE v
  case parseRE bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Reporting" $
    sequence_
      [ unit_nel_parse
      , unit_nel_render
      , unit_re_parse
      , unit_re_render
      , it "Reporting-Endpoints round-trip" prop_re_roundtrip
      ]
