module Test.Hermes.Cookie (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Cookie (Cookie (..), CookiePair (..))
import Network.HTTP.Headers.HeaderFieldName (hCookie, hSetCookie)
import Network.HTTP.Headers.SetCookie
import Test.Syd


tests :: Spec
tests = do
  requestCookies
  responseCookies


requestCookies :: Spec
requestCookies = describe "request Cookie" $ do
  it "accepts empty values and cookie punctuation, preserving quotes" $ do
    let wire = "empty=; b64=ab+c/def==; quoted=\"a:b[1]@example\"; quotes=\"\"; encoded=%2f"
        value =
          Cookie
            [ CookiePair "empty" ""
            , CookiePair "b64" "ab+c/def=="
            , CookiePair "quoted" "\"a:b[1]@example\""
            , CookiePair "quotes" "\"\""
            , CookiePair "encoded" "%2f"
            ]
    parseFromHeaders defaultHeaderSettings (wire :| []) `shouldBe` Right value
    headerMapToList (setHeader value (headerMapFromList [])) `shouldBe` [("cookie", wire)]
  it "combines split HTTP/2 and HTTP/3 cookies in wire order, including duplicate names" $ do
    let fields =
          headerMapFromList
            [ ("cookie", " name=first;\tflag= ")
            , ("Cookie", "\tname=second")
            ]
        pairs = Cookie [CookiePair "name" "first", CookiePair "flag" "", CookiePair "name" "second"]
    lookupHeader @Cookie fields `shouldBe` Right (Just pairs)
    lookupRawHeader hCookie (setHeader pairs fields) `shouldBe` Just ("name=first; flag=; name=second" :| [])
  it "distinguishes an absent field, an empty field and an empty value" $ do
    lookupHeader @Cookie (headerMapFromList []) `shouldBe` Right Nothing
    lookupHeader @Cookie (headerMapFromList [("cookie", "\t ")]) `shouldBe` Right (Just (Cookie []))
    lookupHeader @Cookie (headerMapFromList [("cookie", "name=")])
      `shouldBe` Right (Just (Cookie [CookiePair "name" ""]))
  it "rejects invalid pair syntax and malformed later field lines" $ do
    forM_ ["=a", "a", "a =b", "a=b, c=d", "a=b;", "; a=b", "a=b;; c=d", "a=\"unclosed", "a=\"b\"c"] $
      \wire -> isLeft (parseCookieLine wire) `shouldBe` True
    forM_ ["broken", "", "; next=2", "next=2;"] $ \second ->
      isLeft (lookupHeader @Cookie (headerMapFromList [("cookie", "valid=1"), ("cookie", second)])) `shouldBe` True
    isLeft (lookupHeader @Cookie (headerMapFromList [("cookie", "split=\"a"), ("cookie", "b\"")])) `shouldBe` True
  it "validates every authored request pair before replacing existing fields" $ do
    let existing = headerMapFromList [("Cookie", "already=1"), ("Cookie", "second=2")]
        check bad = do
          let value = Cookie [CookiePair "first" "ok", bad]
          renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
          setHeader value existing `shouldBe` existing
    mapM_ (\bad -> check (CookiePair bad "ok")) ["", "bad name", "bad=name", "bad;name"]
    mapM_ (check . CookiePair "name") ["with space", "x;y", "\"closed\"rest", "\"unclosed", "a,b", "a\\b", "é"]
    renderHeaderLines defaultHeaderSettings (Cookie []) `shouldBe` Just ("" :| [])
  it "implements the RFC 6265 cookie-octet class for every octet" $
    forM_ [0 .. 255] $ \octet -> do
      let payload = "prefix" <> BS.singleton octet <> "suffix"
          isCookieOctet = octet == 0x21 || inside 0x23 0x2b || inside 0x2d 0x3a || inside 0x3c 0x5b || inside 0x5d 0x7e
          inside start end = start <= octet && octet <= end
          check wireValue
            | isCookieOctet =
                case ST.fromByteString wireValue of
                  Just value -> parseCookieLine ("key=" <> wireValue) `shouldBe` Right (Cookie [CookiePair "key" value])
                  Nothing -> expectationFailure "RFC cookie-octet must be ASCII"
            | otherwise = isLeft (parseCookieLine ("key=" <> wireValue)) `shouldBe` True
      check payload
      check ("\"" <> payload <> "\"")


parseCookieLine :: ByteString -> Either String Cookie
parseCookieLine value = parseFromHeaders defaultHeaderSettings (value :| [])


responseCookies :: Spec
responseCookies = describe "response Set-Cookie" $ do
  it "decodes and renders case-insensitive attributes with literal cookie quotes" $ do
    let wire =
          " \tsession = \"ab+c/def==\"; path = /account/; domain = .EXAMPLE.com; "
            <> "expires=Sun, 06 Nov 1994 08:49:37 GMT; max-age=-6; sEcUrE; httponly; samesite=lAx"
        parsed =
          fixture
            { setCookieDomain = Just ".EXAMPLE.com"
            , setCookiePath = Just "/account/"
            , setCookieExpires = Just (UTCTime (fromGregorian 1994 11 6) (secondsToDiffTime 31_777))
            , setCookieMaxAge = Just (-6)
            , setCookieSecure = True
            , setCookieHttpOnly = True
            , setCookieSameSite = Just SameSiteLax
            }
        canonical = "session=\"ab+c/def==\"; Expires=Sun, 06 Nov 1994 08:49:37 GMT; Max-Age=-6; Domain=.EXAMPLE.com; Path=/account/; Secure; HttpOnly; SameSite=Lax"
    parseSetCookieField wire `shouldBe` Right parsed
    let fields = setHeader (SetCookies [parsed]) (headerMapFromList [])
    lookupRawHeader hSetCookie fields `shouldBe` Just (canonical :| [])
    lookupHeader @SetCookies fields `shouldBe` Right (Just (SetCookies [parsed]))
  it "preserves extension attributes and their order when editing a known attribute" $ do
    let wire = "session=abc; Priority=High; Secure; pArTiTiOnEd; Path=/first; future = one two; Priority=Low; Max-Age=12garbage"
        expected = "session=abc; Path=/next; Secure; Priority=High; pArTiTiOnEd; future = one two; Priority=Low"
    case parseSetCookieField wire of
      Right cookie -> do
        map setCookieExtensionText (setCookieExtensions cookie)
          `shouldBe` ["Priority=High", "pArTiTiOnEd", "future = one two", "Priority=Low"]
        let value = SetCookies [cookie {setCookiePath = Just "/next"}]
            fields = setHeader value (headerMapFromList [])
        lookupRawHeader hSetCookie fields `shouldBe` Just (expected :| [])
        lookupHeader @SetCookies fields `shouldBe` Right (Just value)
      result -> expectationFailure (show result)
  it "writes authored extension attributes and clears them independently of known attributes" $ do
    let base = fixture {setCookieSecure = True}
    case traverse mkSetCookieExtension ["\t Partitioned ", "Priority=High", "future extension"] of
      Just extensions -> do
        let cookie = withSetCookieExtensions extensions base
            fields = setHeader (SetCookies [cookie]) (headerMapFromList [])
        lookupRawHeader hSetCookie fields
          `shouldBe` Just ("session=\"ab+c/def==\"; Secure; Partitioned; Priority=High; future extension" :| [])
        withSetCookieExtensions [] cookie `shouldBe` base
      Nothing -> expectationFailure "Valid extension attributes"
    forM_ ["", "  ", "Path=/admin", "sEcUrE", "Max-Age = 1", "a;b", "a\r\nb", "a\tb", "a=\xff"] $ \wire ->
      mkSetCookieExtension wire `shouldBe` Nothing
  it "takes the last applicable attribute without treating invalid prefixes as valid values" $ do
    let wire = "session=\"ab+c/def==\"; Max-Age=6; Max-Age=0; Max-Age=12garbage; Expires=bad; SameSite=Strict; SameSite=unknown; Path=/first; Path=/last; Secure-Extension"
    case parseSetCookieField wire of
      Right cookie -> do
        withSetCookieExtensions [] cookie `shouldBe` fixture {setCookieMaxAge = Just 0, setCookiePath = Just "/last"}
        map setCookieExtensionText (setCookieExtensions cookie) `shouldBe` ["Secure-Extension"]
      result -> expectationFailure (show result)
  it "saturates both Max-Age bounds without wrapping or accepting a numeric prefix" $ do
    let value age = fmap setCookieMaxAge (parseSetCookieField ("session=; Max-Age=" <> age))
    value "999999999999999999999999999999999" `shouldBe` Right (Just maxBound)
    value "-999999999999999999999999999999999" `shouldBe` Right (Just minBound)
    value "999999999999999999999999999999999bad" `shouldBe` Right Nothing
    value "-0" `shouldBe` Right (Just 0)
  it "bounds the cookie pair and ignores each oversized attribute before applying it" $ do
    let wire = " id = \"x\" ; Path = /ok\t; Path=/long; Max-Age=0007; Max-Age=00000; Secure=value; HttpOnly; Priority = high ; X=12345"
    isLeft (parseSetCookieFieldWithLimits 5 4 wire) `shouldBe` True
    isLeft (parseSetCookieFieldWithLimits 6 4 ("id=" <> BS.replicate 4096 120)) `shouldBe` True
    isLeft (parseSetCookieFieldWithLimits 6 4 "id=x; Path=/long\r\n; Secure") `shouldBe` True
    case parseSetCookieFieldWithLimits 6 4 wire of
      Right cookie -> do
        setCookieValue cookie `shouldBe` "\"x\""
        setCookiePath cookie `shouldBe` Just "/ok"
        setCookieMaxAge cookie `shouldBe` Just 7
        setCookieSecure cookie `shouldBe` False
        setCookieHttpOnly cookie `shouldBe` True
        map setCookieExtensionText (setCookieExtensions cookie) `shouldBe` ["Priority = high"]
      result -> expectationFailure (show result)
    fmap setCookiePath (parseSetCookieField wire) `shouldBe` Right (Just "/long")
    parseSetCookieFieldWithLimits 4096 1024 wire `shouldBe` parseSetCookieField wire
  it "uses the default policy and path when the last attributes have no value" $
    parseSetCookieField "session=\"ab+c/def==\"; SameSite=Strict; SameSite; Path=/first; Path"
      `shouldBe` Right fixture {setCookiePath = Just ""}
  it "keeps multiple physical lines and empty values without splitting an Expires date" $ do
    let first = "session=\"ab+c/def==\"; Expires=Sun, 06 Nov 1994 08:49:37 GMT"
        second = "state="
        fields = headerMapFromList [("set-cookie", first), ("Set-Cookie", second)]
    case lookupHeader @SetCookies fields of
      Right (Just value@(SetCookies [_, state])) -> do
        setCookieValue state `shouldBe` ""
        lookupRawHeader hSetCookie (setHeader value (headerMapFromList [])) `shouldBe` Just (first :| [second])
      failure -> expectationFailure (show failure)
  it "checks the complete cookie pair and rejects invalid later lines" $ do
    forM_ ["session=one, other=two", "session=\"one\"two", "session=one\r\n", "session=\xff"] $
      \wire -> isLeft (parseSetCookieField wire) `shouldBe` True
    isLeft (lookupHeader @SetCookies (headerMapFromList [("set-cookie", "valid=2"), ("set-cookie", "invalid")]))
      `shouldBe` True
  it "validates every authored response cookie before replacing existing fields" $ do
    let existing = headerMapFromList [("Set-Cookie", "already=1"), ("Set-Cookie", "second=2")]
        check bad = do
          let value = SetCookies [fixture, bad]
          renderHeaderLines defaultHeaderSettings value `shouldBe` Nothing
          setHeader value existing `shouldBe` existing
    check fixture {setCookieName = "bad=name"}
    check fixture {setCookieValue = "\"one;two\""}
    mapM_ (\bad -> check fixture {setCookieDomain = Just bad}) ["", "bad_name", "bad..test", "-bad.test", ".test.", "bad; Secure"]
    mapM_ (\bad -> check fixture {setCookiePath = Just bad}) [" /trimmed", "/trimmed ", "/one; Secure", "/tab\tx", "/é"]
    check fixture {setCookieExpires = Just (UTCTime (fromGregorian 1600 12 31) 0)}
    check fixture {setCookieExpires = Just (UTCTime (fromGregorian 10000 1 1) 0)}
    check fixture {setCookieExpires = Just (UTCTime (fromGregorian 2026 11 21) (secondsToDiffTime (-1)))}
    check fixture {setCookieExpires = Just (UTCTime (fromGregorian 2026 11 21) (secondsToDiffTime 86_400))}
    let minimumDate = UTCTime (fromGregorian 1601 1 1) 0
        accepted = SetCookies [fixture {setCookieExpires = Just minimumDate, setCookiePath = Just "/one two", setCookieMaxAge = Just minBound}]
    lookupHeader @SetCookies (setHeader accepted existing) `shouldBe` Right (Just accepted)
  where
    fixture = SetCookie "session" "\"ab+c/def==\"" Nothing Nothing Nothing Nothing False False Nothing
