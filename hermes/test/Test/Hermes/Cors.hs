{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Cors (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Grammar.Authority (SerializedOrigin (..))
import Network.HTTP.Headers
import qualified Network.HTTP.Headers.AccessControlAllowCredentials as Cred
import qualified Network.HTTP.Headers.AccessControlAllowHeaders as AH
import qualified Network.HTTP.Headers.AccessControlAllowMethods as AM
import qualified Network.HTTP.Headers.AccessControlAllowOrigin as AO
import qualified Network.HTTP.Headers.AccessControlExposeHeaders as EH
import qualified Network.HTTP.Headers.AccessControlMaxAge as MA
import qualified Network.HTTP.Headers.AccessControlRequestHeaders as RH
import qualified Network.HTTP.Headers.AccessControlRequestMethod as RM
import Network.HTTP.Headers.HeaderFieldName (hAccessControlRequestMethod, hOrigin, headerNameFromShortByteString)
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.Origin as Origin
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.HTTP.Methods as HTTP
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
        <$> (("a" <>) <$> genToken)
        <*> genToken
        <*> Gen.maybe (Gen.word16 (Range.linear 0 65535))
    ]


genCredentials :: Gen Cred.AccessControlAllowCredentials
genCredentials = pure (Cred.AccessControlAllowCredentials True)


genAllowHeaders :: Gen AH.AccessControlAllowHeaders
genAllowHeaders = Gen.choice [pure AH.AllowHeadersEmpty, pure AH.AllowHeadersWildcard, AH.AllowHeaders <$> genTokenList]


genAllowMethods :: Gen AM.AccessControlAllowMethods
genAllowMethods = Gen.choice [pure AM.AllowMethodsEmpty, pure AM.AllowMethodsWildcard, AM.AllowMethods <$> genTokenList]


genExposeHeaders :: Gen EH.AccessControlExposeHeaders
genExposeHeaders = Gen.choice [pure EH.ExposeHeadersEmpty, pure EH.ExposeHeadersWildcard, EH.ExposeHeaders <$> genTokenList]


genRequestHeaders :: Gen RH.AccessControlRequestHeaders
genRequestHeaders = Gen.choice [pure RH.RequestHeadersWildcard, RH.RequestHeaders <$> genTokenList]


genMaxAge :: Gen MA.AccessControlMaxAge
genMaxAge = MA.AccessControlMaxAge <$> Gen.int (Range.linear 0 maxBound)


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


unit_origin_authority :: Spec
unit_origin_authority = it "reads origins with bracketed hosts and validates the whole tuple" $ do
  let single value = lookupHeader @Origin.Origin (headerMapFromList [("Origin", value)])
      allow value = lookupHeader @AO.AccessControlAllowOrigin (headerMapFromList [("Access-Control-Allow-Origin", value)])
  single "\thttps://[::1]:8443 " `shouldBe` Right (Just (Origin.OriginHeader (Origin.Origin "https" "[::1]" (Just 8443))))
  single "null" `shouldBe` Right (Just (Origin.OriginHeader Origin.OriginNull))
  allow "https://[2001:db8::1]:65535" `shouldBe` Right (Just (AO.AllowOrigin "https" "[2001:db8::1]" (Just 65535)))
  mapM_
    ( \wire -> do
        single wire `shouldSatisfy` isLeft
        allow wire `shouldSatisfy` isLeft
    )
    [ "1https://example.org"
    , "x!y://example.org"
    , "https://example.org:65536"
    , "https://example.org:"
    , "https://[::broken]"
    , "https://user@example.org"
    , "https://example.org/path"
    , "https://example.org?q"
    , "https://example.org#fragment"
    , "https://"
    , "NULL"
    ]
  lookupHeader @AO.AccessControlAllowOrigin (headerMapFromList [("Access-Control-Allow-Origin", "*"), ("Access-Control-Allow-Origin", "null")])
    `shouldSatisfy` isLeft


unit_origin_lists :: Spec
unit_origin_lists = it "preserves the RFC 6454 origin list while CORS accepts a single origin" $ do
  let wire = "http://example.net custom+app://[::1]:42 https://other.example"
      value =
        Origin.OriginHeader $
          Origin.OriginList
            (SerializedOrigin "http" "example.net" Nothing)
            (SerializedOrigin "custom+app" "[::1]" (Just 42) :| [SerializedOrigin "https" "other.example" Nothing])
      headers = headerMapFromList [("Origin", wire)]
  lookupHeader headers `shouldBe` Right (Just value)
  lookupRawHeader hOrigin (setHeader value headers) `shouldBe` Just (wire :| [])
  lookupHeader @AO.AccessControlAllowOrigin (headerMapFromList [("Access-Control-Allow-Origin", wire)])
    `shouldSatisfy` isLeft
  lookupHeader @Origin.Origin (headerMapFromList [("Origin", "null https://other.example")])
    `shouldSatisfy` isLeft
  lookupHeader @Origin.Origin (headerMapFromList [("Origin", "http://example.net"), ("Origin", "https://other.example")])
    `shouldSatisfy` isLeft


unit_fetch_origin_syntax :: Spec
unit_fetch_origin_syntax = it "accepts browser-serialized origin spellings and retains the request bytes" $ do
  let allow wire = lookupHeader @AO.AccessControlAllowOrigin (headerMapFromList [("Access-Control-Allow-Origin", wire)])
  mapM_
    ( \wire -> do
        fmap Origin.corsRequestOriginBytes (Origin.parseCorsRequestOrigin wire) `shouldBe` Right wire
        allow wire `shouldSatisfy` either (const False) (maybe False (const True))
    )
    [ "null"
    , "https://192.0.2.1"
    , "web+app://example-2.test:00042"
    , "https://[2001:db8::1]:65535"
    , "https://[2001:db8:0:0:0:0:0:1]"
    , "https://[::]"
    , "null://host"
    , "https://example.test."
    , "https://foo_bar.test"
    , "https://a\"b.test"
    , "https://a\x60\&b.test"
    , "https://a{b}.test"
    , "https://xn--bcher-kva.example"
    ]
  mapM_
    ( \wire -> do
        Origin.parseCorsRequestOrigin wire `shouldSatisfy` isLeft
        allow wire `shouldSatisfy` isLeft
    )
    [ "https://"
    , "https://user@example.test"
    , "https://example.test/path"
    , "https://example.test?query"
    , "https://example.test#fragment"
    , "https://example.test:65536"
    , "https://a|b.test"
    , "https://a b.test"
    , "https://[2001:db8:bad::broken]"
    ]
  Origin.parseCorsRequestOrigin "*" `shouldSatisfy` isLeft
  Origin.parseCorsRequestOrigin "null " `shouldSatisfy` isLeft
  Origin.parseCorsRequestOrigin "https://example.test https://other.test" `shouldSatisfy` isLeft
  let browserWire = "https://a{b}.test"
      browserOrigin = AO.AllowOrigin "https" "a{b}.test" Nothing
  allow browserWire `shouldBe` Right (Just browserOrigin)
  headerMapToList (setHeader browserOrigin (headerMapFromList []))
    `shouldBe` [("access-control-allow-origin", browserWire)]
  let request = Origin.OriginHeader (Origin.Origin "https" "a{b}.test" Nothing)
  lookupHeader @Origin.Origin (headerMapFromList [("Origin", browserWire)]) `shouldBe` Right (Just request)
  headerMapToList (setHeader request (headerMapFromList [])) `shouldBe` [("origin", browserWire)]
  lookupHeader @Origin.Origin (headerMapFromList [("Origin", "https://[2001:DB8::1]")])
    `shouldBe` Right (Just (Origin.OriginHeader (Origin.Origin "https" "[2001:DB8::1]" Nothing)))


unit_cors_response_check :: Spec
unit_cors_response_check = it "checks response syntax and matches the serialized origin byte for byte" $ do
  let check request mode fields = fmap (\parsed -> AO.corsCheck mode parsed (headerMapFromList fields)) (Origin.parseCorsRequestOrigin request)
      origin = "https://[2001:db8::1]:8443"
      allow wire = ("Access-Control-Allow-Origin", wire)
      credentials wire = ("Access-Control-Allow-Credentials", wire)
      response = [allow origin, credentials "\t true "]
  map (\mode -> check origin mode response) [Cred.CorsOmit, Cred.CorsSameOrigin, Cred.CorsInclude]
    `shouldBe` [Right True, Right True, Right True]
  check origin Cred.CorsInclude [allow ("\t " <> origin <> " "), credentials "true"] `shouldBe` Right True
  check origin Cred.CorsSameOrigin [allow " *\t", credentials "False"] `shouldBe` Right True
  check origin Cred.CorsInclude [allow "*", credentials "true"] `shouldBe` Right False
  check "null" Cred.CorsInclude [allow "null", credentials "true"] `shouldBe` Right True
  check "null" Cred.CorsOmit [] `shouldBe` Right False
  mapM_
    (\wire -> check origin Cred.CorsOmit [allow wire] `shouldBe` Right False)
    [ "https://[2001:db8::1]:08443"
    , "https://[2001:db8:0:0:0:0:0:1]:8443"
    , "https://[2001:DB8::1]:8443"
    , origin <> ", " <> origin
    , origin <> "\r"
    ]
  check "https://example.test" Cred.CorsOmit [allow "https://example.test:443"] `shouldBe` Right False
  check "https://example.test." Cred.CorsOmit [allow "https://example.test"] `shouldBe` Right False
  check "https://a\"b.test" Cred.CorsOmit [allow "https://a\"b.test"] `shouldBe` Right True
  check origin Cred.CorsOmit [allow "*", allow origin] `shouldBe` Right False
  check origin Cred.CorsInclude [allow origin, credentials "true", credentials "true"] `shouldBe` Right False
  check origin Cred.CorsInclude [allow origin] `shouldBe` Right False
  mapM_
    (\wire -> check origin Cred.CorsInclude [allow origin, credentials wire] `shouldBe` Right False)
    ["True", "true, true", "false", "true\r"]


-- Access-Control-Allow-Credentials -----------------------------------------

unit_credentials :: Spec
unit_credentials = it "Access-Control-Allow-Credentials parse/render" $ do
  parseFull Cred.accessControlAllowCredentialsParser "true"
    `shouldBe` Right (Cred.AccessControlAllowCredentials True)
  render Cred.renderAccessControlAllowCredentials (Cred.AccessControlAllowCredentials True)
    `shouldBe` "true"
  render Cred.renderAccessControlAllowCredentials (Cred.AccessControlAllowCredentials False)
    `shouldBe` ""
  mapM_
    (\wire -> lookupHeader @Cred.AccessControlAllowCredentials (headerMapFromList [("Access-Control-Allow-Credentials", wire)]) `shouldSatisfy` isLeft)
    ["false", "True", "true, true", "truex", ""]
  let existing = headerMapFromList [("Access-Control-Allow-Credentials", "true"), ("Access-Control-Allow-Origin", "https://example.test")]
  headerMapToList (setHeader (Cred.AccessControlAllowCredentials False) existing)
    `shouldBe` [("access-control-allow-origin", "https://example.test")]
  headerMapToList (setHeader (Cred.AccessControlAllowCredentials True) (headerMapFromList []))
    `shouldBe` [("access-control-allow-credentials", "true")]


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
  parseFull MA.accessControlMaxAgeParser "-1" `shouldSatisfy` isLeft
  render MA.renderAccessControlMaxAge (MA.AccessControlMaxAge 86_400) `shouldBe` "86400"
  render MA.renderAccessControlMaxAge (MA.AccessControlMaxAge (-1)) `shouldBe` "0"
  parseFull MA.accessControlMaxAgeParser (BS.replicate 200 57) `shouldBe` Right (MA.AccessControlMaxAge maxBound)
  parseFull MA.accessControlMaxAgeParser (BS.replicate 200 57 <> "x") `shouldSatisfy` isLeft
  parseFull MA.accessControlMaxAgeParser "+42" `shouldSatisfy` isLeft


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


unit_cors_lists :: Spec
unit_cors_lists = it "combines field lines in order and reads complete wildcard-prefixed tokens" $ do
  let inputs name = headerMapFromList [(name, " , *custom, "), (name, "\t* , "), (name, ",x-test ")]
      headers = "*custom" :| ["*", "x-test"]
      empty = headerMapFromList []
  lookupHeader @AH.AccessControlAllowHeaders (inputs "Access-Control-Allow-Headers") `shouldBe` Right (Just (AH.AllowHeaders headers))
  lookupHeader @AM.AccessControlAllowMethods (inputs "Access-Control-Allow-Methods") `shouldBe` Right (Just (AM.AllowMethods headers))
  lookupHeader @EH.AccessControlExposeHeaders (inputs "Access-Control-Expose-Headers") `shouldBe` Right (Just (EH.ExposeHeaders headers))
  lookupHeader @RH.AccessControlRequestHeaders (inputs "Access-Control-Request-Headers") `shouldBe` Right (Just (RH.RequestHeaders headers))
  parseFull RH.accessControlRequestHeadersParser "*" `shouldBe` Right (RH.RequestHeaders ("*" :| []))
  headerMapToList (setHeader (AM.AllowMethods headers) empty) `shouldBe` [("access-control-allow-methods", "*custom, *, x-test")]
  headerMapToList (setHeader AH.AllowHeadersEmpty empty) `shouldBe` [("access-control-allow-headers", "")]
  lookupHeader @AM.AccessControlAllowMethods (headerMapFromList [("Access-Control-Allow-Methods", ", ,"), ("Access-Control-Allow-Methods", "")])
    `shouldBe` Right (Just AM.AllowMethodsEmpty)
  lookupHeader @EH.AccessControlExposeHeaders (headerMapFromList [("Access-Control-Expose-Headers", " \t")])
    `shouldBe` Right (Just EH.ExposeHeadersEmpty)
  lookupHeader @RH.AccessControlRequestHeaders (headerMapFromList [("Access-Control-Request-Headers", ",, ")])
    `shouldSatisfy` isLeft
  lookupHeader @AH.AccessControlAllowHeaders (headerMapFromList [("Access-Control-Allow-Headers", "X-Foo"), ("Access-Control-Allow-Headers", "x:bad")])
    `shouldSatisfy` isLeft


unit_cors_singletons :: Spec
unit_cors_singletons = it "trims field-line whitespace and rejects repeated singleton values" $ do
  let single name value = headerMapFromList [(name, "\t " <> value <> " \t")]
      repeated name value = headerMapFromList [(name, value), (name, value)]
  lookupHeader @RM.AccessControlRequestMethod (single "Access-Control-Request-Method" "mIxEd")
    `shouldBe` Right (Just (RM.AccessControlRequestMethod "mIxEd"))
  lookupHeader @MA.AccessControlMaxAge (single "Access-Control-Max-Age" "00015")
    `shouldBe` Right (Just (MA.AccessControlMaxAge 15))
  lookupHeader @Cred.AccessControlAllowCredentials (single "Access-Control-Allow-Credentials" "true")
    `shouldBe` Right (Just (Cred.AccessControlAllowCredentials True))
  lookupHeader @RM.AccessControlRequestMethod (repeated "Access-Control-Request-Method" "PUT") `shouldSatisfy` isLeft
  lookupHeader @MA.AccessControlMaxAge (repeated "Access-Control-Max-Age" "60") `shouldSatisfy` isLeft
  lookupHeader @Cred.AccessControlAllowCredentials (repeated "Access-Control-Allow-Credentials" "true") `shouldSatisfy` isLeft


unit_cors_method_value :: Spec
unit_cors_method_value = it "projects and authors the shared case-sensitive HTTP method" $ do
  let empty = headerMapFromList []
      decode wire = lookupHeader @RM.AccessControlRequestMethod (headerMapFromList [("Access-Control-Request-Method", wire)])
  mapM_
    ( \wire -> case decode wire of
        Right (Just field) -> do
          RM.requestMethodAsMethod field `shouldBe` HTTP.mkMethod wire
          lookupRawHeader hAccessControlRequestMethod (setHeader field empty) `shouldBe` Just (wire :| [])
        other -> expectationFailure (show other)
    )
    ["PATCH", "patch", "*CUSTOM", "PROPFIND"]
  RM.requestMethodFromMethod HTTP.PATCH `shouldBe` Right (RM.AccessControlRequestMethod "PATCH")
  RM.requestMethodFromMethod (HTTP.methodFromBytes "invalid token") `shouldSatisfy` isLeft
  RM.requestMethodAsMethod (RM.AccessControlRequestMethod "") `shouldBe` Left HTTP.MethodEmpty


unit_invalid_request_method :: Spec
unit_invalid_request_method = it "rejects malformed authored Access-Control-Request-Method tokens" $ do
  let original = headerMapFromList [("Access-Control-Request-Method", "PATCH"), ("Access-Control-Request-Method", "GET")]
  mapM_
    ( \method -> do
        let value = RM.AccessControlRequestMethod method
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value original `shouldBe` original
    )
    ["", "two words", "GET,PATCH", "GÉT"]


unit_invalid_request_headers :: Spec
unit_invalid_request_headers = it "rejects malformed authored Access-Control-Request-Headers names" $ do
  let original = headerMapFromList [("Access-Control-Request-Headers", "authorization"), ("Access-Control-Request-Headers", "x-probe")]
  mapM_
    ( \name -> do
        let value = RH.RequestHeaders ("authorization" :| [name])
        renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
        setHeader value original `shouldBe` original
    )
    ["", "two words", "x-probe,x-other", "x-café"]
  renderHeaderLines defaultHeaderSettings RH.RequestHeadersWildcard `shouldBe` Just ("*" :| [])


unit_cors_allow_list_matching :: Spec
unit_cors_allow_list_matching = it "matches parsed request methods and names under each credentials mode" $ do
  let fields =
        headerMapFromList
          [ ("Access-Control-Request-Method", "PATCH")
          , ("Access-Control-Allow-Methods", "GET, patch")
          , ("Access-Control-Allow-Methods", "*")
          , ("Access-Control-Request-Headers", "AUTHORIZATION, x-request-id, x-other")
          , ("Access-Control-Allow-Headers", "X-Request-ID")
          , ("Access-Control-Allow-Headers", "*")
          ]
  case (lookupHeader fields, lookupHeader fields, lookupHeader fields, lookupHeader fields) of
    (Right (Just method), Right (Just methods), Right (Just (RH.RequestHeaders names)), Right (Just allowed)) -> do
      map (\mode -> AM.allowMethodsMatches mode method methods) modes `shouldBe` [True, True, False]
      AM.allowMethodsMatches Cred.CorsInclude (RM.AccessControlRequestMethod "patch") methods `shouldBe` True
      AM.allowMethodsMatches Cred.CorsInclude (RM.AccessControlRequestMethod "*") methods `shouldBe` True
      map (\mode -> fmap (\name -> AH.allowHeadersMatches mode name allowed) names) modes
        `shouldBe` [(False :| [True, True]), (False :| [True, True]), (False :| [True, False])]
    other -> expectationFailure (show other)
  AM.allowMethodsMatches Cred.CorsOmit (RM.AccessControlRequestMethod "GET") AM.AllowMethodsEmpty `shouldBe` False
  AM.allowMethodsMatches Cred.CorsSameOrigin (RM.AccessControlRequestMethod "PATCH") AM.AllowMethodsWildcard `shouldBe` True
  AM.allowMethodsMatches Cred.CorsInclude (RM.AccessControlRequestMethod "*") AM.AllowMethodsWildcard `shouldBe` True
  AH.allowHeadersMatches Cred.CorsOmit "authorization" AH.AllowHeadersWildcard `shouldBe` False
  AH.allowHeadersMatches Cred.CorsInclude "AUTHORIZATION" (AH.AllowHeaders ("Authorization" :| ["*"])) `shouldBe` True
  AH.allowHeadersMatches Cred.CorsInclude "*" AH.AllowHeadersWildcard `shouldBe` True
  AH.allowHeadersMatches Cred.CorsOmit "x-other" AH.AllowHeadersEmpty `shouldBe` False
  where
    modes = [Cred.CorsOmit, Cred.CorsSameOrigin, Cred.CorsInclude]


unit_cors_exposed_response_names :: Spec
unit_cors_exposed_response_names = it "applies the response safelist, cookie exclusions, and wildcard rules" $ do
  let fields = headerMapFromList [("Access-Control-Expose-Headers", "X-Trace, Set-Cookie2"), ("Access-Control-Expose-Headers", "*")]
  case lookupHeader fields of
    Right exposed@(Just _) -> do
      let names = ["Content-Type", "X-Trace", "X-Other", "*", "Set-Cookie", "Set-Cookie2"]
          visible mode = map (\name -> EH.exposesResponseHeader mode name exposed) names
      visible Cred.CorsOmit `shouldBe` [True, True, True, True, False, False]
      visible Cred.CorsSameOrigin `shouldBe` [True, True, True, True, False, False]
      visible Cred.CorsInclude `shouldBe` [True, True, False, True, False, False]
    other -> expectationFailure (show other)
  let safelisted = ["Cache-Control", "Content-Language", "Content-Length", "Content-Type", "Expires", "Last-Modified", "Pragma"]
      exposed mode name = EH.exposesResponseHeader mode name (Just EH.ExposeHeadersWildcard)
  all (\name -> EH.exposesResponseHeader Cred.CorsInclude name Nothing) safelisted `shouldBe` True
  EH.exposesResponseHeader Cred.CorsOmit "x-other" (Just EH.ExposeHeadersEmpty) `shouldBe` False
  exposed Cred.CorsInclude "*" `shouldBe` True
  exposed Cred.CorsOmit (headerNameFromShortByteString "SET-COOKIE") `shouldBe` False


unit_cors_short_max_age :: Spec
unit_cors_short_max_age = it "agrees across short, padded, and overflowed max-age field values" $ do
  let decode wire = lookupHeader @MA.AccessControlMaxAge (headerMapFromList [("Access-Control-Max-Age", wire)])
      expect wire seconds = decode wire `shouldBe` Right (Just (MA.AccessControlMaxAge seconds))
      sliced = BS.take 9 (BS.drop 1 "x000086400y")
      overflow = BS.replicate 70 57
  expect sliced 86_400
  expect "999999999" 999_999_999
  expect "1000000000" 1_000_000_000
  expect "\t0 \t" 0
  expect (" " <> overflow <> "\t") maxBound
  mapM_ (\wire -> decode wire `shouldSatisfy` isLeft) ["1 2", "-1", "1\255", "1,2", overflow <> "x"]


unit_cors_checked_response_lists :: Spec
unit_cors_checked_response_lists = do
  it "checks authored Access-Control-Allow-Methods entries" $ do
    let fields = headerMapFromList [("Access-Control-Allow-Methods", "GET"), ("Access-Control-Allow-Methods", "PATCH")]
    mapM_ (\entry -> do
      let value = AM.AllowMethods ("GET" :| [entry])
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value fields `shouldBe` fields) ["", "DELETE, PATCH", "GET METHOD", "X-\xff"]
    renderHeaderLines defaultHeaderSettings AM.AllowMethodsEmpty `shouldBe` Just ("" :| [])
    renderHeaderLines defaultHeaderSettings (AM.AllowMethods ("patch" :| ["*"])) `shouldBe` Just ("patch, *" :| [])
  it "checks authored Access-Control-Allow-Headers entries" $ do
    let fields = headerMapFromList [("Access-Control-Allow-Headers", "Content-Type"), ("Access-Control-Allow-Headers", "Authorization")]
    mapM_ (\entry -> do
      let value = AH.AllowHeaders ("Content-Type" :| [entry])
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value fields `shouldBe` fields) ["", "a:b", "A, B", "X-\xff"]
    renderHeaderLines defaultHeaderSettings AH.AllowHeadersWildcard `shouldBe` Just ("*" :| [])
    renderHeaderLines defaultHeaderSettings (AH.AllowHeaders ("*" :| ["Authorization"])) `shouldBe` Just ("*, Authorization" :| [])
  it "checks authored Access-Control-Expose-Headers entries" $ do
    let fields = headerMapFromList [("Access-Control-Expose-Headers", "X-Trace"), ("Access-Control-Expose-Headers", "Content-Length")]
    mapM_ (\entry -> do
      let value = EH.ExposeHeaders ("X-Trace" :| [entry])
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value fields `shouldBe` fields) ["", "a:b", "A, B", "X-\xff"]
    renderHeaderLines defaultHeaderSettings EH.ExposeHeadersWildcard `shouldBe` Just ("*" :| [])
    renderHeaderLines defaultHeaderSettings (EH.ExposeHeaders ("X-Trace" :| ["x!result"])) `shouldBe` Just ("X-Trace, x!result" :| [])


checkedOriginWrites :: Spec
checkedOriginWrites = do
  let candidates = [("1https","example.test"), ("https+",""), ("https","example.test/path"), ("https","user@host"), ("https","[:::1]"), ("https","%ZZ"), ("https","a b")]
  it "checks authored Access-Control-Allow-Origin components" $ do
    let fields = headerMapFromList [("Access-Control-Allow-Origin", "https://old.test")]
    mapM_ (\(scheme, host) -> do
      let value = AO.AllowOrigin scheme host Nothing
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value fields `shouldBe` fields) candidates
    let allowed = AO.AllowOrigin "http" "te\"s,t\x60{}.test" (Just 65535)
    renderHeaderLines defaultHeaderSettings allowed `shouldBe` Just ("http://te\"s,t\x60{}.test:65535" :| [])
  it "checks every authored member of an Origin list" $ do
    let fields = headerMapFromList [("Origin", "https://old.test")]
        good = SerializedOrigin "https" "example.test" Nothing
    mapM_ (\(scheme, host) -> do
      let value = Origin.OriginHeader (Origin.OriginList good (SerializedOrigin scheme host Nothing :| []))
      renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
      setHeader value fields `shouldBe` fields) candidates
    let allowed = Origin.OriginHeader (Origin.OriginList good (SerializedOrigin "custom+9" "[::1]" (Just 0) :| []))
    renderHeaderLines defaultHeaderSettings allowed `shouldBe` Just ("https://example.test custom+9://[::1]:0" :| [])


tests :: Spec
tests =
  describe "Cors" $
    sequence_
      [ unit_origin
      , unit_origin_render
      , unit_origin_authority
      , unit_origin_lists
      , unit_fetch_origin_syntax
      , unit_cors_response_check
      , unit_credentials
      , unit_cors_checked_response_lists
      , checkedOriginWrites
      , unit_allow_headers
      , unit_allow_methods
      , unit_expose_headers
      , unit_max_age
      , unit_request_headers
      , unit_request_method
      , unit_cors_lists
      , unit_cors_singletons
      , unit_cors_method_value
      , unit_invalid_request_method
      , unit_invalid_request_headers
      , unit_cors_allow_list_matching
      , unit_cors_exposed_response_names
      , unit_cors_short_max_age
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
