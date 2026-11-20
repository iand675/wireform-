{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the Forwarding & proxy header family: @Forwarded@
(RFC 7239), @Proxy-Status@ (RFC 9209), @Authentication-Info@ /
@Proxy-Authentication-Info@ (RFC 7615), and @Authentication-Control@
(RFC 8053).
-}
module Test.Hermes.Forwarded (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Function (on)
import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, headerMapToList, lookupHeader, setHeader, setRawHeader)
import qualified Network.HTTP.Headers.AuthenticationControl as AC
import qualified Network.HTTP.Headers.AuthenticationInfo as AI
import qualified Network.HTTP.Headers.Forwarded as F
import Network.HTTP.Headers.HeaderFieldName (hAuthenticationControl)
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
    genElement = F.ForwardedElement . nubBy ((==) `on` (T.toCaseFold . ST.toText . fst)) <$> Gen.list (Range.linear 1 3) genPair
    genPair = (,) <$> (("x-" <>) <$> genTokenText) <*> genForwardedValue


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
    genInfo = PS.ProxyInfo <$> genId <*> (nubBy ((==) `on` fst) <$> Gen.list (Range.linear 0 3) genParam)
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
        <*> genParams
    genParams = do
      count <- Gen.int (Range.linear 1 3)
      names <- take count <$> Gen.shuffle ["realm", "no-auth", "-label.example.org"]
      traverse (\name -> (,) name <$> genCredParam) names


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

  it "parses a quoted IPv6 node and port" $
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

  it "keeps hops in wire order across field lines and skips empty list slots" $ do
    let typed = lookupHeader @F.Forwarded
        fields = headerMapFromList [("Forwarded", " ,;FoR=UNKNOWN;;proto=hTtPs,;,"), ("Forwarded", "By=\"_edge:_local\";host=\"[::1]:443\"")]
        value =
          F.Forwarded
            [ F.ForwardedElement [("FoR", F.ForwardedToken "UNKNOWN"), ("proto", F.ForwardedToken "hTtPs")]
            , F.ForwardedElement []
            , F.ForwardedElement [("By", F.ForwardedQuoted "_edge:_local"), ("host", F.ForwardedQuoted "[::1]:443")]
            ]
    typed fields `shouldBe` Right (Just value)
    headerMapToList (setHeader value (headerMapFromList []))
      `shouldBe` [("forwarded", "FoR=UNKNOWN;proto=hTtPs, ;, By=\"_edge:_local\";host=\"[::1]:443\"")]

  it "validates known parameter grammar and uniqueness per hop" $ do
    let readField wire = lookupHeader @F.Forwarded (headerMapFromList [("Forwarded", wire)])
    readField "for=\"192.0.2.1:00001\",for=\"\\_edge:_local\""
      `shouldBe` Right (Just (F.Forwarded [F.ForwardedElement [("for", F.ForwardedQuoted "192.0.2.1:00001")], F.ForwardedElement [("for", F.ForwardedQuoted "_edge:_local")]]))
    readField "FOR=unknown;For-Node=one;proto=https,for-node=two;PROTO=http"
      `shouldBe` Right (Just (F.Forwarded
        [ F.ForwardedElement [("FOR", F.ForwardedToken "unknown"), ("For-Node", F.ForwardedToken "one"), ("proto", F.ForwardedToken "https")]
        , F.ForwardedElement [("for-node", F.ForwardedToken "two"), ("PROTO", F.ForwardedToken "http")]
        ]))
    mapM_
      (\wire -> readField wire `shouldSatisfy` isLeft)
      [ "for=192.0.2.1;FOR=192.0.2.2"
      , "x=one;X=two"
      , "for=unknown;For-Node=one;proto=https;for-node=two"
      , "for=01.0.2.3"
      , "for=256.0.2.3"
      , "for=hostname"
      , "for=\"2001:db8::1\""
      , "for=\"[v1.xyz]\""
      , "for=\"[::1]:123456\""
      , "for=\"_x:_\""
      , "for=\"unknown:\""
      , "by=_"
      , "by=_foo+bar"
      , "host=\"user@example.com\""
      , "host=\"example.com/path\""
      , "proto=9http"
      , "proto=\"https://example.com\""
      , "for =unknown"
      , "for=unknown; proto=https"
      ]

  it "preserves opaque quoted extension bytes and escapes delimiters on rendering" $ do
    let typed = lookupHeader @F.Forwarded
        wire = "x=\"comma,semi;quotes\\\"and\\\\slashes\t" <> BS.pack [128, 255] <> "\""
        fields = headerMapFromList [("Forwarded", wire)]
    case typed fields of
      Right (Just value) ->
        headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("forwarded", wire)]
      other -> expectationFailure (show other)

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

  it "keeps member order across lines and parameter positions across replacements" $ do
    let validFields =
          headerMapFromList
            [ ("Proxy-Status", " origin;ext=@1700; received-status=bad;again=?1;received-status=204")
            , ("Proxy-Status", "\"edge, west\";next-hop=backend.example:8443;next-protocol=:AA==:;error=connection_timeout\t")
            ]
    case lookupHeader @PS.ProxyStatus validFields of
      Right (Just value@(PS.ProxyStatus [origin, edge])) -> do
        PS.proxyParams origin `shouldBe` [("ext", Just (DateValue 1700)), ("received-status", Just (Integer 204)), ("again", Nothing)]
        fmap PS.proxyReceivedStatus (PS.proxyInfoParameters origin) `shouldBe` Right (Just 204)
        PS.proxyInfoParameters edge
          `shouldBe` Right
            (PS.ProxyStatusParameters (Just (RFC8941Token "connection_timeout")) (Just (Left (RFC8941Token "backend.example:8443"))) (Just (Right "\0")) Nothing Nothing)
        headerMapToList (setHeader value (headerMapFromList []))
          `shouldBe` [("proxy-status", "origin;ext=@1700;received-status=204;again, \"edge, west\";next-hop=backend.example:8443;next-protocol=:AA==:;error=connection_timeout")]
      result -> expectationFailure (show result)
    lookupHeader @PS.ProxyStatus (headerMapFromList [("Proxy-Status", "origin"), ("Proxy-Status", "")])
      `shouldSatisfy` isLeft
    headerMapToList (setHeader (PS.ProxyStatus []) (headerMapFromList [])) `shouldBe` []

  it "reports invalid registered parameters through the typed view" $
    mapM_
      (\parameters -> PS.proxyInfoParameters (PS.ProxyInfo (Token (RFC8941Token "edge")) parameters) `shouldSatisfy` isLeft)
      [ [("received-status", Just (Integer 600))]
      , [("next-protocol", Just (Binary ""))]
      , [("next-protocol", Just (Binary (BS.replicate 256 0)))]
      , [("details", Nothing)]
      , [("error", Just (String (RFC8941String "http_protocol_error")))]
      ]

  it "uses the token form for ALPN identifiers that fit the Structured Fields token grammar" $ do
    let fields =
          headerMapFromList
            [ ("Proxy-Status", "edge;next-protocol=:aDI=:;ext=:aDI=:, numeric;next-protocol=:MWg=:")
            , ("Proxy-Status", "binary;next-protocol=:aC8/AA==:")
            ]
    case lookupHeader @PS.ProxyStatus fields of
      Right (Just value@(PS.ProxyStatus [edge, numeric, binary])) -> do
        PS.proxyParams edge `shouldBe` [("next-protocol", Just (Binary "h2")), ("ext", Just (Binary "h2"))]
        fmap (fmap PS.proxyNextProtocol . PS.proxyInfoParameters) [edge, numeric, binary]
          `shouldBe` [Right (Just (Left (RFC8941Token "h2"))), Right (Just (Right "1h")), Right (Just (Right "h/?\0"))]
        let expected = "edge;next-protocol=h2;ext=:aDI=:, numeric;next-protocol=:MWg=:, binary;next-protocol=:aC8/AA==:"
        render PS.renderProxyStatus value `shouldBe` expected
        headerMapToList (setHeader value fields) `shouldBe` [("proxy-status", expected)]
      result -> expectationFailure (show result)


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

  it "combines repeated fields across parameters and entries, skipping empty elements" $ do
    let fields =
          ", Basic  , Realm\t=\tentrance,"
            :| ["-label.example.org*=UTF-8''caf%C3%A9", "", "Basic realm=other, "]
        combined = BS.intercalate "," (case fields of first :| rest -> first : rest)
        expected =
          AC.AuthenticationControl
            [ AC.AuthControlEntry
                (AC.AuthScheme "Basic")
                [ ("Realm", AC.CredentialParamToken "entrance")
                , ("-label.example.org*", AC.CredentialParamToken "UTF-8''caf%C3%A9")
                ]
            , AC.AuthControlEntry (AC.AuthScheme "Basic") [("realm", AC.CredentialParamToken "other")]
            ]
    lookupHeader @AC.AuthenticationControl (setRawHeader hAuthenticationControl fields (headerMapFromList []))
      `shouldBe` Right (Just expected)
    runP AC.authenticationControlParser combined `shouldBe` Right expected
    lookupHeader @AC.AuthenticationControl (setHeader expected (headerMapFromList []))
      `shouldBe` Right (Just expected)

  it "round-trips opaque quoted octets, escapes, and horizontal tab" $ do
    let wire = "Basic realm=\"\t,\\\\" <> BS.pack [0x80, 0xff] <> "\""
        expected =
          AC.AuthenticationControl
            [AC.AuthControlEntry (AC.AuthScheme "Basic") [("realm", AC.CredentialParamOpaque ("\t,\\" <> BS.pack [0x80, 0xff]))]]
    runP AC.authenticationControlParser wire `shouldBe` Right expected
    render AC.renderAuthenticationControl expected `shouldBe` wire

  it "rejects duplicate parameter names, including plain and extended spellings" $ do
    let header value = headerMapFromList [("Authentication-Control", value)]
    lookupHeader @AC.AuthenticationControl (header "Basic realm=one, Realm=two")
      `shouldBe` Left "Duplicate Authentication-Control parameter"
    lookupHeader @AC.AuthenticationControl (header "Basic Username=one, username*=UTF-8''caf%C3%A9")
      `shouldBe` Left "Duplicate Authentication-Control parameter"

  it "rejects invalid names, extended values and list boundaries" $
    mapM_
      (\value -> runP AC.authenticationControlParser value `shouldSatisfy` isLeft)
      [ " , \t,"
      , "Basic"
      , "Basic\trealm=one"
      , "Basic realm=one Digest realm=two"
      , "Basic realm=one, Digest"
      , "Basic _name=one"
      , "Basic a.b=one"
      , "Basic -name=one"
      , "Basic -name..org=one"
      , "Basic title**=UTF-8''caf%C3%A9"
      , "Basic title*=\"UTF-8''caf%C3%A9\""
      , "Basic title*=UTF-8'en--US'caf%C3%A9"
      , "Basic title*=UTF-8''caf%C3%ZZ"
      , "Basic title*=UTF-8''caf%C3,%A9"
      ]

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
