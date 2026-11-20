{-# LANGUAGE OverloadedStrings #-}

{- |
Tests for the RFC 6265bis ingest hardening added to
"Network.HTTP.Client.Cookies": Domain acceptance, @SameSite=None@
requires @Secure@, the @__Host-@ / @__Secure-@ name-prefix rules,
and the per-cookie size limit.
-}
module Test.Cookies (tests) where

import Data.ByteString qualified as BS
import Network.HTTP.Client.BodyStream (emptyStream)
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
      ]
