{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the Forwarding & proxy header family: @Forwarded@
(RFC 7239), @Proxy-Status@ (RFC 9209), @Authentication-Info@ /
@Proxy-Authentication-Info@ (RFC 7615), and @Authentication-Control@
(RFC 8053).
-}
module Test.Hermes.Forwarded (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.AuthenticationControl as AC
import qualified Network.HTTP.Headers.AuthenticationInfo as AI
import qualified Network.HTTP.Headers.Forwarded as F
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (
  ItemValue (..),
  ParserT,
  RFC8941String (..),
  RFC8941Token (..),
  Result (..),
  runParser,
 )
import qualified Network.HTTP.Headers.ProxyAuthenticationInfo as PAI
import qualified Network.HTTP.Headers.ProxyStatus as PS
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

runP :: ParserT () String a -> ByteString -> Either String a
runP p bs = case runParser p bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: (a -> M.Builder) -> a -> ByteString
render f = M.toStrictByteString . f


-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- | RFC 9110 token text (any tchar).
genTokenText :: Gen ST.ShortText
genTokenText = ST.fromString <$> Gen.string (Range.linear 1 8) (Gen.element tchar)
  where
    tchar = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_.~"


-- | RFC 8941 sf-token text (ALPHA first, then tchar / ":" / "/").
genSfTokenText :: Gen ST.ShortText
genSfTokenText = do
  c <- Gen.element (['a' .. 'z'] <> ['A' .. 'Z'])
  rest <- Gen.string (Range.linear 0 7) (Gen.element rchar)
  pure (ST.fromString (c : rest))
  where
    rchar = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_."


-- | Printable content for a quoted-string / sf-string (incl. escapables).
genQuotedText :: Gen ST.ShortText
genQuotedText = ST.fromString <$> Gen.string (Range.linear 1 8) (Gen.element qchar)
  where
    qchar = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " :[]./\"\\"


-- | RFC 8941 parameter key (lcalpha / "*" first, then key chars).
genParamKey :: Gen ST.ShortText
genParamKey = do
  c <- Gen.element (['a' .. 'z'] <> "*")
  rest <- Gen.string (Range.linear 0 6) (Gen.element kchar)
  pure (ST.fromString (c : rest))
  where
    kchar = ['a' .. 'z'] <> ['0' .. '9'] <> "_-.*"


-- | An @auth-param@ value (shared by the three RFC 7615/8053 headers).
genCredParam :: Gen AI.CredentialParam
genCredParam =
  Gen.choice
    [ AI.CredentialParamToken <$> genTokenText
    , AI.CredentialParamString . RFC8941String <$> genQuotedText
    ]


genAuthParam :: Gen (ST.ShortText, AI.CredentialParam)
genAuthParam = (,) <$> genTokenText <*> genCredParam


genForwardedValue :: Gen F.ForwardedValue
genForwardedValue =
  Gen.choice
    [ F.ForwardedToken <$> genTokenText
    , F.ForwardedQuoted <$> genQuotedText
    ]


genForwarded :: Gen F.Forwarded
genForwarded = F.Forwarded <$> Gen.list (Range.linear 1 3) genElement
  where
    genElement = F.ForwardedElement <$> Gen.list (Range.linear 1 3) genPair
    genPair = (,) <$> genTokenText <*> genForwardedValue


genItemValue :: Gen ItemValue
genItemValue =
  Gen.choice
    [ Integer <$> Gen.int (Range.linear 0 999_999)
    , Token . RFC8941Token <$> genSfTokenText
    , String . RFC8941String <$> genQuotedText
    ]


genProxyStatus :: Gen PS.ProxyStatus
genProxyStatus = PS.ProxyStatus <$> Gen.list (Range.linear 1 3) genInfo
  where
    genInfo = PS.ProxyInfo <$> genId <*> Gen.list (Range.linear 0 3) genParam
    genId =
      Gen.choice
        [ Token . RFC8941Token <$> genSfTokenText
        , String . RFC8941String <$> genQuotedText
        ]
    genParam = (,) <$> genParamKey <*> Gen.maybe genItemValue


genAuthInfo :: Gen AI.AuthenticationInfo
genAuthInfo = AI.AuthenticationInfo <$> Gen.list (Range.linear 1 3) genAuthParam


genProxyAuthInfo :: Gen PAI.ProxyAuthenticationInfo
genProxyAuthInfo = PAI.ProxyAuthenticationInfo <$> Gen.list (Range.linear 1 3) genAuthParam


genAuthControl :: Gen AC.AuthenticationControl
genAuthControl = AC.AuthenticationControl <$> Gen.list (Range.linear 1 3) genEntry
  where
    genEntry =
      AC.AuthControlEntry . AC.AuthScheme
        <$> genTokenText
        <*> Gen.list (Range.linear 1 3) genAuthParam


-- ---------------------------------------------------------------------------
-- Forwarded (RFC 7239)
-- ---------------------------------------------------------------------------

forwardedTests :: Spec
forwardedTests = describe "Forwarded" $ do
  it "parses a single element with for/proto/by" $
    runP F.forwardedParser "for=192.0.2.60;proto=http;by=203.0.113.43"
      `shouldBe` Right
        ( F.Forwarded
            [ F.ForwardedElement
                [ ("for", F.ForwardedToken "192.0.2.60")
                , ("proto", F.ForwardedToken "http")
                , ("by", F.ForwardedToken "203.0.113.43")
                ]
            ]
        )

  it "parses a quoted obfuscated node" $
    runP F.forwardedParser "for=\"[2001:db8:cafe::17]:4711\""
      `shouldBe` Right
        ( F.Forwarded
            [ F.ForwardedElement [("for", F.ForwardedQuoted "[2001:db8:cafe::17]:4711")]
            ]
        )

  it "parses multiple comma-separated elements" $
    runP F.forwardedParser "for=192.0.2.43, for=198.51.100.17"
      `shouldBe` Right
        ( F.Forwarded
            [ F.ForwardedElement [("for", F.ForwardedToken "192.0.2.43")]
            , F.ForwardedElement [("for", F.ForwardedToken "198.51.100.17")]
            ]
        )

  it "renders pairs joined by ';' and elements by ', '" $
    render
      F.renderForwarded
      ( F.Forwarded
          [ F.ForwardedElement
              [("for", F.ForwardedToken "192.0.2.43"), ("proto", F.ForwardedToken "http")]
          , F.ForwardedElement [("for", F.ForwardedQuoted "[2001:db8::1]")]
          ]
      )
      `shouldBe` "for=192.0.2.43;proto=http, for=\"[2001:db8::1]\""

  it "round-trips" prop_forwarded


prop_forwarded :: Property
prop_forwarded = property $ do
  v <- forAll genForwarded
  let bs = render F.renderForwarded v
  case runP F.forwardedParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Proxy-Status (RFC 9209)
-- ---------------------------------------------------------------------------

proxyStatusTests :: Spec
proxyStatusTests = describe "Proxy-Status" $ do
  it "parses a bare proxy identifier" $
    runP PS.proxyStatusParser "ExampleProxy"
      `shouldBe` Right (PS.ProxyStatus [PS.ProxyInfo (Token (RFC8941Token "ExampleProxy")) []])

  it "parses an identifier with parameters" $
    runP PS.proxyStatusParser "ExampleProxy; received-status=504; error=connect_timeout"
      `shouldBe` Right
        ( PS.ProxyStatus
            [ PS.ProxyInfo
                (Token (RFC8941Token "ExampleProxy"))
                [ ("received-status", Just (Integer 504))
                , ("error", Just (Token (RFC8941Token "connect_timeout")))
                ]
            ]
        )

  it "renders identifier and parameters with no space before ';'" $
    render
      PS.renderProxyStatus
      ( PS.ProxyStatus
          [ PS.ProxyInfo
              (Token (RFC8941Token "ExampleProxy"))
              [("received-status", Just (Integer 504))]
          ]
      )
      `shouldBe` "ExampleProxy;received-status=504"

  it "round-trips" prop_proxyStatus


prop_proxyStatus :: Property
prop_proxyStatus = property $ do
  v <- forAll genProxyStatus
  let bs = render PS.renderProxyStatus v
  case runP PS.proxyStatusParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Authentication-Info (RFC 7615)
-- ---------------------------------------------------------------------------

authenticationInfoTests :: Spec
authenticationInfoTests = describe "Authentication-Info" $ do
  it "parses a Digest-style parameter list" $
    runP AI.authenticationInfoParser "qop=auth, rspauth=\"6629fae4\""
      `shouldBe` Right
        ( AI.AuthenticationInfo
            [ ("qop", AI.CredentialParamToken "auth")
            , ("rspauth", AI.CredentialParamString (RFC8941String "6629fae4"))
            ]
        )

  it "renders token and quoted parameters" $
    render
      AI.renderAuthenticationInfo
      ( AI.AuthenticationInfo
          [ ("nextnonce", AI.CredentialParamString (RFC8941String "abc123"))
          , ("qop", AI.CredentialParamToken "auth")
          ]
      )
      `shouldBe` "nextnonce=\"abc123\", qop=auth"

  it "round-trips" prop_authInfo


prop_authInfo :: Property
prop_authInfo = property $ do
  v <- forAll genAuthInfo
  let bs = render AI.renderAuthenticationInfo v
  case runP AI.authenticationInfoParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Proxy-Authentication-Info (RFC 7615)
-- ---------------------------------------------------------------------------

proxyAuthenticationInfoTests :: Spec
proxyAuthenticationInfoTests = describe "Proxy-Authentication-Info" $ do
  it "parses a parameter list" $
    runP PAI.proxyAuthenticationInfoParser "rspauth=\"abc\", qop=auth-int"
      `shouldBe` Right
        ( PAI.ProxyAuthenticationInfo
            [ ("rspauth", PAI.CredentialParamString (RFC8941String "abc"))
            , ("qop", PAI.CredentialParamToken "auth-int")
            ]
        )

  it "renders token and quoted parameters" $
    render
      PAI.renderProxyAuthenticationInfo
      ( PAI.ProxyAuthenticationInfo
          [("cnonce", PAI.CredentialParamString (RFC8941String "0a4f113b"))]
      )
      `shouldBe` "cnonce=\"0a4f113b\""

  it "round-trips" prop_proxyAuthInfo


prop_proxyAuthInfo :: Property
prop_proxyAuthInfo = property $ do
  v <- forAll genProxyAuthInfo
  let bs = render PAI.renderProxyAuthenticationInfo v
  case runP PAI.proxyAuthenticationInfoParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Authentication-Control (RFC 8053)
-- ---------------------------------------------------------------------------

authenticationControlTests :: Spec
authenticationControlTests = describe "Authentication-Control" $ do
  it "parses one scheme entry with parameters" $
    runP AC.authenticationControlParser "Digest realm=\"protected space\", auth-style=modal"
      `shouldBe` Right
        ( AC.AuthenticationControl
            [ AC.AuthControlEntry
                (AC.AuthScheme "Digest")
                [ ("realm", AC.CredentialParamString (RFC8941String "protected space"))
                , ("auth-style", AC.CredentialParamToken "modal")
                ]
            ]
        )

  it "splits comma list into separate scheme entries" $
    runP AC.authenticationControlParser "Basic realm=\"entrance\", no-auth=true, Digest realm=\"x\""
      `shouldBe` Right
        ( AC.AuthenticationControl
            [ AC.AuthControlEntry
                (AC.AuthScheme "Basic")
                [ ("realm", AC.CredentialParamString (RFC8941String "entrance"))
                , ("no-auth", AC.CredentialParamToken "true")
                ]
            , AC.AuthControlEntry
                (AC.AuthScheme "Digest")
                [("realm", AC.CredentialParamString (RFC8941String "x"))]
            ]
        )

  it "renders scheme and parameters" $
    render
      AC.renderAuthenticationControl
      ( AC.AuthenticationControl
          [ AC.AuthControlEntry
              (AC.AuthScheme "Basic")
              [("realm", AC.CredentialParamString (RFC8941String "entrance")), ("no-auth", AC.CredentialParamToken "true")]
          ]
      )
      `shouldBe` "Basic realm=\"entrance\", no-auth=true"

  it "round-trips" prop_authControl


prop_authControl :: Property
prop_authControl = property $ do
  v <- forAll genAuthControl
  let bs = render AC.renderAuthenticationControl v
  case runP AC.authenticationControlParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- Suite
-- ---------------------------------------------------------------------------

tests :: Spec
tests = describe "Forwarded family" $ do
  forwardedTests
  proxyStatusTests
  authenticationInfoTests
  proxyAuthenticationInfoTests
  authenticationControlTests
