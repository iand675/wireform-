{-# LANGUAGE OverloadedStrings #-}

{- |
Tests for the RFC 6265bis ingest hardening added to
"Network.HTTP.Client.Cookies": Domain acceptance, @SameSite=None@
requires @Secure@, the @__Host-@ / @__Secure-@ name-prefix rules,
and the per-cookie size limit.
-}
module Test.Cookies (tests) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Network.HTTP.Client.BodyStream (BodyStream, emptyStream)
import Network.HTTP.Client.Cookies
import Network.HTTP.Client.Protocol (ProtocolInfo (HTTP1_1))
import Network.HTTP.Client.Request qualified as Req
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.Transport (Transport (..))
import Network.HTTP.Client.URI (staticURI)
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method (mGet)
import Network.HTTP.Types.Status (status200)
import Test.Syd


baseCookie :: Cookie
baseCookie =
  Cookie
    { cookieName = "session"
    , cookieValue = "abc"
    , cookieDomain = "example.com"
    , cookieDomainExplicit = False
    , cookiePath = "/"
    , cookieExpires = Nothing
    , cookieSecure = True
    , cookieHttpOnly = False
    , cookieSameSite = SameSiteLax
    }


withJarThat :: (CookieJar -> IO a) -> IO a
withJarThat k = do
  jar <- newCookieJar
  k jar


tests :: Spec
tests =
  describe "Network.HTTP.Client.Cookies" $
    sequence_
      [ describe "validateCookieName / validateCookieValue" $
          sequence_
            [ it "valid token name accepted" $
                validateCookieName "session" `shouldBe` Right ()
            , it "empty name rejected" $
                validateCookieName "" `shouldBe` Left (CookieNameInvalid "")
            , it "name with whitespace rejected" $
                validateCookieName "bad name"
                  `shouldBe` Left (CookieNameInvalid "bad name")
            , it "valid cookie-octet value accepted" $
                validateCookieValue "opaque-token.123" `shouldBe` Right ()
            , it "value with comma rejected" $
                validateCookieValue "has,comma"
                  `shouldBe` Left (CookieValueInvalid "has,comma")
            , it "quoted cookie value is accepted without changing its bytes" $
                validateCookieValue "\"abc/def==\"" `shouldBe` Right ()
            ]
      , describe "insertCookieChecked" $
          sequence_
            [ it "SameSite=None without Secure is rejected" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieSecure = False
                        , cookieSameSite = SameSiteNone
                        }
                r <- insertCookieChecked jar c
                r `shouldBe` Left (CookieSameSiteNoneRequiresSecure "session")
            , it "SameSite=None with Secure is accepted" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieSecure = True
                        , cookieSameSite = SameSiteNone
                        }
                r <- insertCookieChecked jar c
                (r == Right ()) `shouldBe` True
            , it "__Secure- prefix without Secure flag is rejected" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieName = "__Secure-token"
                        , cookieSecure = False
                        }
                r <- insertCookieChecked jar c
                r `shouldBe` Left (CookieSecurePrefixWithoutSecure "__Secure-token")
            , it "__Secure- prefix with Secure flag is accepted" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieName = "__Secure-token"
                        , cookieSecure = True
                        }
                r <- insertCookieChecked jar c
                (r == Right ()) `shouldBe` True
            , it "__Host- prefix requires Secure + Path=/ + no Domain" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieName = "__Host-id"
                        , cookieDomainExplicit = True
                        , cookieSecure = True
                        , cookiePath = "/"
                        }
                r <- insertCookieChecked jar c
                r `shouldBe` Left (CookieHostPrefixViolation "__Host-id")
            , it "__Host- with no Domain, Path=/, Secure is accepted" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieName = "__Host-id"
                        , cookieDomainExplicit = False
                        , cookieSecure = True
                        , cookiePath = "/"
                        }
                r <- insertCookieChecked jar c
                (r == Right ()) `shouldBe` True
            , it "size limit rejects oversize cookies" $ withJarThat $ \jar -> do
                let c = baseCookie {cookieValue = BS.replicate 5000 0x78}
                r <- insertCookieChecked jar c
                case r of
                  Left CookieTooLarge {} -> pure ()
                  other -> error (show other)
            ]
      , it "stores, scopes, sends and expires cookies through middleware" $ withJarThat $ \jar -> do
          let requestAt url = Req.request mGet (staticURI url) emptyStream
              respond fields = Resp.RawResponse status200 fields (pure "") HTTP1_1
              receive fields url =
                sendRaw (withCookies jar (Transport (\_ -> pure (respond fields)))) (requestAt url)
              echo url = do
                response <- sendRaw (withCookies jar (Transport (pure . respond . Req.headers))) (requestAt url)
                pure (H.lookupHeader H.hCookie (Resp.headers response))
              origin = "https://example.com/account/login"
          _ <- receive
            [ (H.hSetCookie, "session=\"ab+c/def==\"; path=/account/; secure; samesite=lax")
            , (H.hSetCookie, "malformed=value with whitespace; Path=/")
            , (H.hSetCookie, "unrelated=value; Domain=other.example; Path=/")
            ]
            origin
          map cookieValue <$> getCookies jar `shouldReturn` ["\"ab+c/def==\""]
          echo "https://example.com/account/profile" `shouldReturn` Just "session=\"ab+c/def==\""
          echo "https://example.com/accounts/profile" `shouldReturn` Nothing
          echo "https://sub.example.com/account/profile" `shouldReturn` Nothing
          echo "https://other.example/account/profile" `shouldReturn` Nothing
          _ <- receive [(H.hSetCookie, "ip=value; Domain=0.0.1; Path=/")] "https://127.0.0.1/"
          echo "https://127.0.0.1/" `shouldReturn` Nothing
          _ <- receive [(H.hSetCookie, "session=; Path=/account/; Max-Age=0")] origin
          getCookies jar `shouldReturn` []
          echo "https://example.com/account/profile" `shouldReturn` Nothing
      , it "sends same-name cookies by longest path, then by creation order" $ withJarThat $ \jar -> do
          let send fields = sendRaw (withCookies jar (Transport (\_ -> pure (cookieResponse fields)))) cookieRequest
              set value = (H.hSetCookie, value)
          _ <- send
            [ set "session=root; Path=/"
            , set "z=first; Path=/account"
            , set "a=second; Path=/account"
            , set "session=scoped; Path=/account"
            ]
          echoCookies jar `shouldReturn` ["z=first; a=second; session=scoped; session=root"]
          _ <- send [set "z=replaced; Path=/account"]
          echoCookies jar `shouldReturn` ["z=replaced; a=second; session=scoped; session=root"]
          _ <- send [set "z=; Path=/account; Max-Age=0", set "z=recreated; Path=/account"]
          echoCookies jar `shouldReturn` ["a=second; session=scoped; z=recreated; session=root"]
      , it "gives a new creation order to a replacement of an expired cookie" $ withJarThat $ \jar -> do
          let expired = UTCTime (fromGregorian 1994 11 6) 0
          insertCookie jar baseCookie{cookieName = "a", cookieExpires = Just expired}
          insertCookieChecked jar baseCookie{cookieName = "z"} `shouldReturn` Right ()
          echoCookies jar `shouldReturn` ["z=abc"]
          insertCookie jar baseCookie{cookieName = "a", cookieValue = "new"}
          echoCookies jar `shouldReturn` ["z=abc; a=new"]
          insertCookieChecked jar baseCookie{cookieName = "z", cookieValue = "updated"} `shouldReturn` Right ()
          echoCookies jar `shouldReturn` ["z=updated; a=new"]
      , it "applies the configured size limits to response cookie pairs and attributes" $ do
          let limits = CookieJarLimits{cjlMaxNameValueBytes = 6, cjlMaxAttributeBytes = 8}
              set value = (H.hSetCookie, value)
          jar <- newCookieJarWith noPublicSuffix limits
          let fields =
                [ set "a=12345"
                , set "ok=123; Path=/account; Path=/oversized"
                , set "b=x; Path=/oversized"
                ]
          _ <- sendRaw (withCookies jar (Transport (const (pure (cookieResponse fields))))) cookieRequest
          map cookieName <$> getCookies jar `shouldReturn` ["b", "ok"]
          map cookiePath <$> getCookies jar `shouldReturn` ["/account", "/account"]
          echoCookies jar `shouldReturn` ["ok=123; b=x"]
      , it "applies cookie prefix, SameSite and secure origin rules to responses" $ withJarThat $ \jar -> do
          let receive target fields =
                sendRaw (withCookies jar (Transport (const (pure (cookieResponse (map (\value -> (H.hSetCookie, value)) fields))))))
                  (Req.request mGet (staticURI target) emptyStream)
          _ <- receive "https://example.com/login"
            [ "__Host-accepted=x; Secure; Path=/"
            , "__Host-default=x; Secure"
            , "__hOsT-default=x; Secure"
            , "__Host-domain=x; Secure; Path=/; Domain=example.com"
            , "__secure-missing=x; Path=/"
            , "site-accepted=x; SameSite=None; Secure; Path=/"
            , "site-missing=x; SameSite=None; Path=/"
            ]
          _ <- receive "http://example.com/login" ["plain=x; Path=/", "tls-only=x; Secure; Path=/"]
          map cookieName <$> getCookies jar `shouldReturn` ["__Host-accepted", "plain", "site-accepted"]
          echoCookies jar `shouldReturn` ["__Host-accepted=x; site-accepted=x; plain=x"]
      , it "stores and deletes host-only and domain cookies independently" $ withJarThat $ \jar -> do
          let subdomain = requestAt "https://www.example.com/account/profile"
          receiveCookies jar cookieRequest
            ["state=domain; Domain=.ExAMPle.com; Path=/account", "state=host; Path=/account"]
          echoCookies jar `shouldReturn` ["state=domain; state=host"]
          echoCookiesAt jar subdomain `shouldReturn` ["state=domain"]
          receiveCookies jar cookieRequest ["state=; Path=/account; Max-Age=0"]
          echoCookies jar `shouldReturn` ["state=domain"]
          receiveCookies jar cookieRequest
            ["state=host; Path=/account", "state=; Domain=example.com; Path=/account; Max-Age=0"]
          echoCookies jar `shouldReturn` ["state=host"]
          echoCookiesAt jar subdomain `shouldReturn` []
      , it "limits cookies on a configured public suffix to their exact host" $ do
          jar <- newCookieJarWithPSL (== "example.com")
          let subdomain = requestAt "https://www.example.com/account/profile"
          receiveCookies jar cookieRequest
            ["site=implicit; Path=/", "fallback=explicit; Domain=EXAMPLE.COM; Path=/"]
          receiveCookies jar subdomain ["child=local; Path=/", "parent=ignored; Domain=example.com; Path=/"]
          echoCookies jar `shouldReturn` ["site=implicit; fallback=explicit"]
          echoCookiesAt jar subdomain `shouldReturn` ["child=local"]
          map cookieDomainExplicit <$> getCookies jar `shouldReturn` [False, False, False]
          insertCookieChecked jar baseCookie `shouldReturn` Right ()
          insertCookieChecked jar baseCookie{cookieDomainExplicit = True}
            `shouldReturn` Left (CookieDomainIsPublicSuffix "example.com")
      , it "preserves secure cookie scope when processing cookies from HTTP" $ withJarThat $ \jar -> do
          let plain = requestAt "http://example.com/account/profile"
          insertCookie jar baseCookie{cookiePath = "/account", cookieValue = "secure"}
          receiveCookies jar plain
            [ "session=local; Path=/account/profile"
            , "session=wide; Path=/"
            , "session=; Path=/account; Max-Age=0"
            ]
          echoCookies jar `shouldReturn` ["session=secure; session=wide"]
          echoCookiesAt jar plain `shouldReturn` ["session=wide"]
          receiveCookies jar cookieRequest ["session=updated; Path=/account; Secure"]
          echoCookies jar `shouldReturn` ["session=updated; session=wide"]
      ]


cookieRequest :: Req.Request BodyStream
cookieRequest = requestAt "https://example.com/account/profile"


requestAt :: Text -> Req.Request BodyStream
requestAt target = Req.request mGet (staticURI target) emptyStream


cookieResponse :: H.Headers -> Resp.RawResponse
cookieResponse fields = Resp.RawResponse status200 fields (pure "") HTTP1_1


echoCookies :: CookieJar -> IO [BS.ByteString]
echoCookies jar = echoCookiesAt jar cookieRequest


echoCookiesAt :: CookieJar -> Req.Request BodyStream -> IO [BS.ByteString]
echoCookiesAt jar req = do
  raw <- sendRaw (withCookies jar (Transport (pure . cookieResponse . Req.headers))) req
  pure (H.lookupHeaders H.hCookie (Resp.headers raw))


receiveCookies :: CookieJar -> Req.Request BodyStream -> [BS.ByteString] -> IO ()
receiveCookies jar req values = do
  let fields = map (\value -> (H.hSetCookie, value)) values
  _ <- sendRaw (withCookies jar (Transport (const (pure (cookieResponse fields))))) req
  pure ()
