{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Reporting (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Function (on)
import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
import Network.HTTP.Headers.HeaderFieldName (hReportingEndpoints)
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
unit_nel_parse = it "parses a NEL registration and preserves its JSON spelling" $
  case parseNEL "{\"report_to\":\"default\",\"max_age\":86400}" of
    Right v -> do
      NEL.nelPolicy v `shouldBe` "{\"report_to\":\"default\",\"max_age\":86400}"
      NEL.nelPolicyDetails v `shouldBe` Right (NEL.NELRegister (NEL.NELRegistration 86400 "default" False 0 1 [] []))
    other -> error (show other)


unit_nel_render :: Spec
unit_nel_render =
  it "renders a NEL value verbatim" $
    renderNEL (NEL.NEL (ST.fromString "{\"max_age\":0}")) `shouldBe` "{\"max_age\":0}"


unit_nel_field_lines :: Spec
unit_nel_field_lines = it "selects the first valid NEL policy in field order and preserves extensions" $ do
  let first = "{\"max_age\":40,\"report_to\":null}"
      second = "{\"max_age\":20,\"report_to\":\"errors\",\"include_subdomains\":true,\"success_fraction\":0.25,\"failure_fraction\":0.75,\"request_headers\":[\"ETag\"],\"response_headers\":[\"X-Result\"],\"extension\":{\"comma\":\",\"}}"
      third = "{\"max_age\":0}"
      headers = headerMapFromList [("NEL", first), ("NEL", second <> ", " <> third)]
      expected = NEL.NELRegister (NEL.NELRegistration 20 "errors" True 0.25 0.75 ["ETag"] ["X-Result"])
  case lookupHeader @NEL.NEL headers of
    Right (Just value) -> do
      NEL.nelPolicyDetails value `shouldBe` Right expected
      lookupRawHeader "NEL" (setHeader value (headerMapFromList []))
        `shouldBe` Just ((first <> ", " <> second <> ", " <> third) :| [])
    other -> expectationFailure (show other)
  fmap NEL.nelPolicyDetails (parseNEL "{\"max_age\":0,\"report_to\":null}")
    `shouldBe` Right (Right NEL.NELRemove)


unit_nel_invalid :: Spec
unit_nel_invalid =
  it "rejects invalid JSON, malformed field lists, and invalid registration settings" $
    forM_
      [ ""
      , "{}"
      , "{\"max_age\":1}"
      , "{\"max_age\":-1,\"report_to\":\"default\"}"
      , "{\"max_age\":0.5,\"report_to\":\"default\"}"
      , "{\"max_age\":1e18446744073709551616,\"report_to\":\"default\"}"
      , "{\"max_age\":1,\"report_to\":true}"
      , "{\"max_age\":1,\"report_to\":\"r\",\"success_fraction\":null}"
      , "{\"max_age\":1,\"report_to\":\"r\",\"failure_fraction\":1.25}"
      , "{\"max_age\":1,\"report_to\":\"r\",\"success_fraction\":1e-18446744073709551616}"
      , "{\"max_age\":1,\"report_to\":\"r\",\"request_headers\":[\"ETag\",true]}"
      , "{\"max_age\":1,\"report_to\":\"r\",\"response_headers\":{}}"
      , "[{\"max_age\":0}]"
      , "{\"max_age\":0}, {"
      , "{\"max_age\":0, \"extension\":\"\xff\"}"
      , "{\"max_age\":0,\"extension\":\"\\u0040\x01\"}"
      , "{\"max_age\":0}\r\n"
      ]
      (\wire -> lookupHeader @NEL.NEL (headerMapFromList [("NEL", wire)]) `shouldSatisfy` isLeft)


unit_nel_authoring :: Spec
unit_nel_authoring = it "renders checked authored policies with JSON escapes and default omission" $ do
  let registration = NEL.NELRegistration 60 "é,\"𝄞" False 0 1 [] []
      policy = NEL.NELRegister registration
  case NEL.nelFromPolicy policy of
    Right value -> do
      let wire = renderNEL value
      BS.all (< 0x7f) wire `shouldBe` True
      parseNEL wire `shouldBe` Right value
      fmap NEL.nelPolicyDetails (parseNEL wire) `shouldBe` Right (Right policy)
    Left failure -> expectationFailure failure
  fmap renderNEL (NEL.nelFromPolicy NEL.NELRemove) `shouldBe` Right "{\"max_age\":0}"
  fmap renderNEL (parseNEL "{\"report_to\":\"r\",\"max_age\":100.0}" >>= NEL.nelPolicyDetails >>= NEL.nelFromPolicy)
    `shouldBe` Right "{\"report_to\":\"r\",\"max_age\":100}"
  NEL.nelFromPolicy (NEL.NELRegister (registration {NEL.nelMaxAge = 0})) `shouldSatisfy` isLeft
  NEL.nelFromPolicy (NEL.NELRegister (registration {NEL.nelMaxAge = 0.5})) `shouldSatisfy` isLeft
  NEL.nelFromPolicy (NEL.NELRegister (registration {NEL.nelSuccessFraction = 2})) `shouldSatisfy` isLeft
  let raw = NEL.NEL {NEL.nelPolicy = "{\"max_age\":0}"}
  NEL.nelPolicyDetails (raw {NEL.nelPolicy = "{\"max_age\":1,\"report_to\":\"default\"}"})
    `shouldBe` Right (NEL.NELRegister (NEL.NELRegistration 1 "default" False 0 1 [] []))


unit_nel_large_lifetime :: Spec
unit_nel_large_lifetime = it "round-trips integer lifetimes expressed with large exponents" $
  forM_ [10_000, maxBound - 200] $ \power -> do
    let wire = M.toStrictByteString ("{\"max_age\":10e" <> M.intDec power <> ",\"report_to\":\"errors\"}")
    case parseNEL wire >>= NEL.nelPolicyDetails of
      Right policy ->
        (NEL.nelFromPolicy policy >>= parseNEL . renderNEL >>= NEL.nelPolicyDetails) `shouldBe` Right policy
      Left failure -> expectationFailure failure


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
  prefix <- Gen.element ["/", "//reports.example/", "https://reports.example/"]
  s <- Gen.list (Range.linear 1 30) (Gen.element urlChars)
  pure (RFC8941String (ST.fromString prefix <> ST.fromString s))
  where
    urlChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ":/._-?=&"


endpointGen :: Gen RE.ReportingEndpoint
endpointGen = RE.ReportingEndpoint <$> keyGen <*> urlGen


reGen :: Gen RE.ReportingEndpoints
reGen = RE.ReportingEndpoints . nubBy ((==) `on` RE.reportingEndpointName) <$> Gen.list (Range.linear 0 5) endpointGen


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
      , unit_nel_field_lines
      , unit_nel_invalid
      , unit_nel_authoring
      , unit_nel_large_lifetime
      , unit_re_parse
      , unit_re_render
      , it "combines field lines and projects URI references after dictionary key replacement" $ do
          let fields =
                headerMapFromList
                  [ ("Reporting-Endpoints", "old=\"/old\", default=\"/first\"")
                  , ("Reporting-Endpoints", "backup=\"//reports.example/b%20c\";extension, default=\"../collect?type=nel\", old=?1")
                  ]
          case lookupHeader @RE.ReportingEndpoints fields of
            Right (Just endpoints) ->
              lookupRawHeader hReportingEndpoints (setHeader endpoints (headerMapFromList []))
                `shouldBe` Just ("default=\"../collect?type=nel\", backup=\"//reports.example/b%20c\"" :| [])
            other -> expectationFailure (show other)
      , it "skips non-String values and malformed URI references in a valid dictionary" $ do
          fmap renderRE (parseRE "bad=\"a b\", percent=\"/x%ZZ\", also=(\"/valid\"), good=\"?report=1#x\", no")
            `shouldBe` Right "good=\"?report=1#x\""
          parseRE "one=token, two=\"/bad%9\"" `shouldBe` Right (RE.ReportingEndpoints [])
          parseRE "one=\"/ok\", two=\"unterminated" `shouldSatisfy` isLeft
      , it "Reporting-Endpoints round-trip" prop_re_roundtrip
      ]
