{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Cors (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.AccessControlAllowCredentials as Cred
import qualified Network.HTTP.Headers.AccessControlAllowHeaders as AH
import qualified Network.HTTP.Headers.AccessControlAllowMethods as AM
import qualified Network.HTTP.Headers.AccessControlAllowOrigin as AO
import qualified Network.HTTP.Headers.AccessControlExposeHeaders as EH
import qualified Network.HTTP.Headers.AccessControlMaxAge as MA
import qualified Network.HTTP.Headers.AccessControlRequestHeaders as RH
import qualified Network.HTTP.Headers.AccessControlRequestMethod as RM
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- Parse a full value, treating any leftover input as failure.
parseFull :: ParserT st String a -> ByteString -> Either String a
parseFull p bs = case runParser p bs of
  OK v rest
    | BS.null rest -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


render :: (a -> M.Builder) -> a -> ByteString
render f = M.toStrictByteString . f


-- Generic render -> parse round-trip property.
roundtripProp :: (Eq a, Show a) => Gen a -> (a -> M.Builder) -> ParserT st String a -> Property
roundtripProp gen renderFn parser = property $ do
  v <- forAll gen
  let bs = render renderFn v
  case parseFull parser bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


-- Generators ---------------------------------------------------------------

genToken :: Gen ST.ShortText
genToken = ST.fromString <$> Gen.string (Range.linear 1 8) Gen.alphaNum


genTokenList :: Gen (NonEmpty ST.ShortText)
genTokenList = Gen.nonEmpty (Range.linear 1 4) genToken


genAllowOrigin :: Gen AO.AccessControlAllowOrigin
genAllowOrigin =
  Gen.choice
    [ pure AO.AllowOriginWildcard
    , pure AO.AllowOriginNull
    , AO.AllowOrigin
        <$> genToken
        <*> genToken
        <*> Gen.maybe (Gen.word16 (Range.linear 0 65535))
    ]


genCredentials :: Gen Cred.AccessControlAllowCredentials
genCredentials = Cred.AccessControlAllowCredentials <$> Gen.bool


genAllowHeaders :: Gen AH.AccessControlAllowHeaders
genAllowHeaders = Gen.choice [pure AH.AllowHeadersWildcard, AH.AllowHeaders <$> genTokenList]


genAllowMethods :: Gen AM.AccessControlAllowMethods
genAllowMethods = Gen.choice [pure AM.AllowMethodsWildcard, AM.AllowMethods <$> genTokenList]


genExposeHeaders :: Gen EH.AccessControlExposeHeaders
genExposeHeaders = Gen.choice [pure EH.ExposeHeadersWildcard, EH.ExposeHeaders <$> genTokenList]


genRequestHeaders :: Gen RH.AccessControlRequestHeaders
genRequestHeaders = Gen.choice [pure RH.RequestHeadersWildcard, RH.RequestHeaders <$> genTokenList]


genMaxAge :: Gen MA.AccessControlMaxAge
genMaxAge = MA.AccessControlMaxAge <$> Gen.int (Range.linear (-1) 86_400)


genRequestMethod :: Gen RM.AccessControlRequestMethod
genRequestMethod = RM.AccessControlRequestMethod <$> genToken


-- Access-Control-Allow-Origin ----------------------------------------------

unit_origin :: Spec
unit_origin = it "Access-Control-Allow-Origin parse" $ do
  parseFull AO.accessControlAllowOriginParser "*" `shouldBe` Right AO.AllowOriginWildcard
  parseFull AO.accessControlAllowOriginParser "null" `shouldBe` Right AO.AllowOriginNull
  parseFull AO.accessControlAllowOriginParser "https://example.com:8080"
    `shouldBe` Right (AO.AllowOrigin "https" "example.com" (Just 8080))


unit_origin_render :: Spec
unit_origin_render = it "Access-Control-Allow-Origin render" $ do
  render AO.renderAccessControlAllowOrigin AO.AllowOriginWildcard `shouldBe` "*"
  render AO.renderAccessControlAllowOrigin (AO.AllowOrigin "https" "example.com" Nothing)
    `shouldBe` "https://example.com"
  render AO.renderAccessControlAllowOrigin (AO.AllowOrigin "http" "localhost" (Just 3000))
    `shouldBe` "http://localhost:3000"


-- Access-Control-Allow-Credentials -----------------------------------------

unit_credentials :: Spec
unit_credentials = it "Access-Control-Allow-Credentials parse/render" $ do
  parseFull Cred.accessControlAllowCredentialsParser "true"
    `shouldBe` Right (Cred.AccessControlAllowCredentials True)
  render Cred.renderAccessControlAllowCredentials (Cred.AccessControlAllowCredentials True)
    `shouldBe` "true"
  render Cred.renderAccessControlAllowCredentials (Cred.AccessControlAllowCredentials False)
    `shouldBe` "false"


-- Access-Control-Allow-Headers ---------------------------------------------

unit_allow_headers :: Spec
unit_allow_headers = it "Access-Control-Allow-Headers parse/render" $ do
  parseFull AH.accessControlAllowHeadersParser "X-Custom-Header, Upgrade-Insecure-Requests"
    `shouldBe` Right (AH.AllowHeaders ("X-Custom-Header" :| ["Upgrade-Insecure-Requests"]))
  parseFull AH.accessControlAllowHeadersParser "*" `shouldBe` Right AH.AllowHeadersWildcard
  render AH.renderAccessControlAllowHeaders (AH.AllowHeaders ("X-Foo" :| ["X-Bar"]))
    `shouldBe` "X-Foo, X-Bar"


-- Access-Control-Allow-Methods ---------------------------------------------

unit_allow_methods :: Spec
unit_allow_methods = it "Access-Control-Allow-Methods parse/render" $ do
  parseFull AM.accessControlAllowMethodsParser "GET, POST, OPTIONS"
    `shouldBe` Right (AM.AllowMethods ("GET" :| ["POST", "OPTIONS"]))
  parseFull AM.accessControlAllowMethodsParser "*" `shouldBe` Right AM.AllowMethodsWildcard
  render AM.renderAccessControlAllowMethods (AM.AllowMethods ("GET" :| ["DELETE"]))
    `shouldBe` "GET, DELETE"


-- Access-Control-Expose-Headers --------------------------------------------

unit_expose_headers :: Spec
unit_expose_headers = it "Access-Control-Expose-Headers parse/render" $ do
  parseFull EH.accessControlExposeHeadersParser "Content-Length, X-Kuma-Revision"
    `shouldBe` Right (EH.ExposeHeaders ("Content-Length" :| ["X-Kuma-Revision"]))
  render EH.renderAccessControlExposeHeaders EH.ExposeHeadersWildcard `shouldBe` "*"


-- Access-Control-Max-Age ---------------------------------------------------

unit_max_age :: Spec
unit_max_age = it "Access-Control-Max-Age parse/render" $ do
  parseFull MA.accessControlMaxAgeParser "600" `shouldBe` Right (MA.AccessControlMaxAge 600)
  parseFull MA.accessControlMaxAgeParser "-1" `shouldBe` Right (MA.AccessControlMaxAge (-1))
  render MA.renderAccessControlMaxAge (MA.AccessControlMaxAge 86_400) `shouldBe` "86400"
  render MA.renderAccessControlMaxAge (MA.AccessControlMaxAge (-1)) `shouldBe` "-1"


-- Access-Control-Request-Headers -------------------------------------------

unit_request_headers :: Spec
unit_request_headers = it "Access-Control-Request-Headers parse/render" $ do
  parseFull RH.accessControlRequestHeadersParser "x-pingother, content-type"
    `shouldBe` Right (RH.RequestHeaders ("x-pingother" :| ["content-type"]))
  render RH.renderAccessControlRequestHeaders (RH.RequestHeaders ("x-foo" :| []))
    `shouldBe` "x-foo"


-- Access-Control-Request-Method --------------------------------------------

unit_request_method :: Spec
unit_request_method = it "Access-Control-Request-Method parse/render" $ do
  parseFull RM.accessControlRequestMethodParser "POST"
    `shouldBe` Right (RM.AccessControlRequestMethod "POST")
  render RM.renderAccessControlRequestMethod (RM.AccessControlRequestMethod "DELETE")
    `shouldBe` "DELETE"


tests :: Spec
tests =
  describe "Cors" $
    sequence_
      [ unit_origin
      , unit_origin_render
      , unit_credentials
      , unit_allow_headers
      , unit_allow_methods
      , unit_expose_headers
      , unit_max_age
      , unit_request_headers
      , unit_request_method
      , it "Access-Control-Allow-Origin round-trip" $
          roundtripProp genAllowOrigin AO.renderAccessControlAllowOrigin AO.accessControlAllowOriginParser
      , it "Access-Control-Allow-Credentials round-trip" $
          roundtripProp genCredentials Cred.renderAccessControlAllowCredentials Cred.accessControlAllowCredentialsParser
      , it "Access-Control-Allow-Headers round-trip" $
          roundtripProp genAllowHeaders AH.renderAccessControlAllowHeaders AH.accessControlAllowHeadersParser
      , it "Access-Control-Allow-Methods round-trip" $
          roundtripProp genAllowMethods AM.renderAccessControlAllowMethods AM.accessControlAllowMethodsParser
      , it "Access-Control-Expose-Headers round-trip" $
          roundtripProp genExposeHeaders EH.renderAccessControlExposeHeaders EH.accessControlExposeHeadersParser
      , it "Access-Control-Max-Age round-trip" $
          roundtripProp genMaxAge MA.renderAccessControlMaxAge MA.accessControlMaxAgeParser
      , it "Access-Control-Request-Headers round-trip" $
          roundtripProp genRequestHeaders RH.renderAccessControlRequestHeaders RH.accessControlRequestHeadersParser
      , it "Access-Control-Request-Method round-trip" $
          roundtripProp genRequestMethod RM.renderAccessControlRequestMethod RM.accessControlRequestMethodParser
      ]
