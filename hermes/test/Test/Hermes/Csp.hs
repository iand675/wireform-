{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Csp (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.ContentSecurityPolicy as CSP
import qualified Network.HTTP.Headers.ContentSecurityPolicyReportOnly as CSPRO
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

dropTrailingOws :: ByteString -> ByteString
dropTrailingOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseCsp :: ByteString -> Either String CSP.ContentSecurityPolicy
parseCsp bs = case runParser CSP.contentSecurityPolicyParser bs of
  OK v leftover
    | BS.null (dropTrailingOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseCspro :: ByteString -> Either String CSPRO.ContentSecurityPolicyReportOnly
parseCspro bs = case runParser CSPRO.contentSecurityPolicyReportOnlyParser bs of
  OK v leftover
    | BS.null (dropTrailingOws leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderCsp :: CSP.ContentSecurityPolicy -> ByteString
renderCsp = M.toStrictByteString . CSP.renderContentSecurityPolicy


renderCspro :: CSPRO.ContentSecurityPolicyReportOnly -> ByteString
renderCspro = M.toStrictByteString . CSPRO.renderContentSecurityPolicyReportOnly


dirPair :: CSP.Directive -> (String, [String])
dirPair (CSP.Directive n vs) = (ST.toString n, map ST.toString vs)


-- ---------------------------------------------------------------------------
-- Content-Security-Policy units
-- ---------------------------------------------------------------------------

unit_parse_csp :: Spec
unit_parse_csp = it "parses a multi-directive policy preserving order and values" $
  case parseCsp "default-src 'self'; img-src 'self' https://img.example.com; upgrade-insecure-requests" of
    Right (CSP.ContentSecurityPolicy ds) ->
      map dirPair ds
        `shouldBe` [ ("default-src", ["'self'"])
                   , ("img-src", ["'self'", "https://img.example.com"])
                   , ("upgrade-insecure-requests", [])
                   ]
    other -> error (show other)


unit_parse_csp_trailing :: Spec
unit_parse_csp_trailing = it "tolerates a trailing semicolon" $
  case parseCsp "frame-ancestors 'none';" of
    Right (CSP.ContentSecurityPolicy ds) ->
      map dirPair ds `shouldBe` [("frame-ancestors", ["'none'"])]
    other -> error (show other)


unit_render_csp :: Spec
unit_render_csp =
  it "renders directives joined by '; ' and values by ' '" $
    let v =
          CSP.ContentSecurityPolicy
            [ CSP.Directive "default-src" ["'none'"]
            , CSP.Directive "script-src" ["'self'", "https://cdn.example.com"]
            ]
    in renderCsp v `shouldBe` "default-src 'none'; script-src 'self' https://cdn.example.com"


-- ---------------------------------------------------------------------------
-- Content-Security-Policy-Report-Only units
-- ---------------------------------------------------------------------------

unit_parse_cspro :: Spec
unit_parse_cspro = it "parses a report-only policy" $
  case parseCspro "default-src 'self'; report-uri /csp-reports" of
    Right (CSPRO.ContentSecurityPolicyReportOnly ds) ->
      map dirPair ds
        `shouldBe` [ ("default-src", ["'self'"])
                   , ("report-uri", ["/csp-reports"])
                   ]
    other -> error (show other)


unit_render_cspro :: Spec
unit_render_cspro =
  it "renders a report-only policy" $
    let v =
          CSPRO.ContentSecurityPolicyReportOnly
            [ CSP.Directive "default-src" ["'self'"]
            , CSP.Directive "report-uri" ["https://example.com/csp"]
            ]
    in renderCspro v `shouldBe` "default-src 'self'; report-uri https://example.com/csp"


-- ---------------------------------------------------------------------------
-- Generators + round-trip properties
-- ---------------------------------------------------------------------------

genName :: Gen ST.ShortText
genName = do
  c <- Gen.element ['a' .. 'z']
  cs <- Gen.list (Range.linear 0 10) (Gen.element nameRest)
  pure (ST.fromString (c : cs))
  where
    nameRest = ['a' .. 'z'] ++ ['0' .. '9'] ++ "-"


genValue :: Gen ST.ShortText
genValue = ST.fromString <$> Gen.list (Range.linear 1 12) (Gen.element valueChars)
  where
    valueChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "':/.*-"


genDirective :: Gen CSP.Directive
genDirective = CSP.Directive <$> genName <*> Gen.list (Range.linear 0 3) genValue


genDirectives :: Gen [CSP.Directive]
genDirectives = Gen.list (Range.linear 1 4) genDirective


prop_roundtrip_csp :: Property
prop_roundtrip_csp = property $ do
  ds <- forAll genDirectives
  let v = CSP.ContentSecurityPolicy ds
      bs = renderCsp v
  case parseCsp bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


prop_roundtrip_cspro :: Property
prop_roundtrip_cspro = property $ do
  ds <- forAll genDirectives
  let v = CSPRO.ContentSecurityPolicyReportOnly ds
      bs = renderCspro v
  case parseCspro bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Csp" $
    sequence_
      [ unit_parse_csp
      , unit_parse_csp_trailing
      , unit_render_csp
      , unit_parse_cspro
      , unit_render_cspro
      , it "Content-Security-Policy round-trips" prop_roundtrip_csp
      , it "Content-Security-Policy-Report-Only round-trips" prop_roundtrip_cspro
      ]
