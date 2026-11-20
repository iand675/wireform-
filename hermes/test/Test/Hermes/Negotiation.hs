module Test.Hermes.Negotiation (tests) where

import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.ContentCoding (ContentCoding (..))
import Network.HTTP.ContentNegotiation (MediaRange (..), MediaType (..), WeightedMediaRange (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.Accept (Accept (..), acceptedContentQuality, preferredContentType)
import Network.HTTP.Headers.AcceptCharset (AcceptCharset (..), WeightedCharset (..))
import Network.HTTP.Headers.AcceptEncoding (AcceptEncoding (..), EncodingTag (..), WeightedEncoding (..))
import Network.HTTP.Headers.AcceptLanguage (AcceptLanguage (..), WeightedLanguage (..), acceptedLanguageQuality, preferredLanguage)
import Network.HTTP.Headers.CacheControl (CacheControl (..), CacheControlDirective (..))
import Network.HTTP.Headers.ContentLanguage (LanguageTag (..))
import Network.HTTP.Headers.ContentType (ContentType (..))
import Network.HTTP.Headers.HeaderFieldName (
  cachedHeaderFieldName,
  hCacheControl,
  headerFieldName,
  headerNameFromShortByteString,
  unsafeCachedHeaderFromBytestring,
  unsafeUnknownHeaderFromBytestring,
 )
import Test.Syd


tests :: Spec
tests = do
  describe "HeaderMap" $ do
    let fields = headerMapFromList [("Cache-Control", "max-age=10"), ("cache-control", "max-age=20")]
        directives = CacheControl (MaxAge 10 :| [MaxAge 20])
    it "preserves repeated field lines when reading or serializing a map" $ do
      lookupRawHeader hCacheControl fields `shouldBe` Just ("max-age=10" :| ["max-age=20"])
      headerMapToList fields `shouldBe` [("cache-control", "max-age=10"), ("cache-control", "max-age=20")]
    it "preserves typed order for both list and insertion construction" $ do
      lookupHeader @CacheControl fields `shouldBe` Right (Just directives)
      let inserted = insertRawHeader hCacheControl "max-age=20" (headerMapFromList [("cache-control", "max-age=10")])
      lookupHeader @CacheControl inserted `shouldBe` Right (Just directives)
    it "uses the same map key for parsed, cached and literal field names" $ do
      let names =
            [ hCacheControl
            , cachedHeaderFieldName "Cache-Control"
            , headerFieldName "cache-control"
            , headerNameFromShortByteString "cache-control"
            , unsafeCachedHeaderFromBytestring "cache-control"
            , unsafeUnknownHeaderFromBytestring "cache-control"
            ]
          combined = foldr (\name -> insertRawHeader name "max-age=10") (headerMapFromList []) names
      names `shouldBe` replicate (length names) hCacheControl
      headerMapToList combined `shouldBe` replicate (length names) ("cache-control", "max-age=10")
      lookupRawHeader (unsafeUnknownHeaderFromBytestring "cache-contro") combined `shouldBe` Nothing
    it "passes ordered field lines to alterations and appends subsequent inserts" $ do
      let altered = alterRawHeader hCacheControl (fmap (<> ("max-age=30" :| []))) fields
          inserted = insertRawHeader hCacheControl "max-age=40" altered
      lookupHeader @CacheControl inserted `shouldBe` Right (Just (CacheControl (MaxAge 10 :| [MaxAge 20, MaxAge 30, MaxAge 40])))
  describe "Accept negotiation" $ do
    it "keeps field order and treats q as a weight in any position" $ do
      let fields = headerMapFromList [("Accept", "text/plain;Q=0;format=flowed, ,"), ("accept", " application/json;q=1.")]
          text = WeightedMediaRange (MediaRange (MediaType "text" "plain") [("format", "flowed")]) 0
          json = WeightedMediaRange (MediaRange (MediaType "application" "json") []) 1
      lookupHeader @Accept fields `shouldBe` Right (Just (Accept [text, json]))
    it "quotes media parameter values when rendering delimiters, escapes or empty values" $ do
      let value = Accept [WeightedMediaRange (MediaRange (MediaType "text" "plain") [("a", "x, y\"\\"), ("b", "")]) 0.001]
          wire = "text/plain;a=\"x, y\\\"\\\\\";b=\"\";q=0.001"
      renderToHeaders defaultHeaderSettings value `shouldBe` wire
      parseFromHeaders defaultHeaderSettings (wire :| []) `shouldBe` Right value
    it "preserves legal wildcard ranges and their parameters and weights" $ do
      let wire = "text/*;charset=utf-8;q=0.4, */*;q=0.1, application/vnd.example+json"
          fields = headerMapFromList [("Accept", wire)]
          value =
            Accept
              [ WeightedMediaRange (MediaRange (MediaType "text" "") [("charset", "utf-8")]) 0.4
              , WeightedMediaRange (MediaRange (MediaType "" "") []) 0.1
              , WeightedMediaRange (MediaRange (MediaType "application" "vnd.example+json") []) 1
              ]
      lookupHeader @Accept fields `shouldBe` Right (Just value)
      renderToHeaders defaultHeaderSettings value `shouldBe` wire
    it "rejects partial, suffix and misplaced wildcards as media ranges" $ do
      let parse value = lookupHeader @Accept (headerMapFromList [("accept", "application/json"), ("accept", value)])
      map (isLeft . parse) ["*/json", "*", "application/*+json", "text/pla*in", "application*/json", "*/*+json"]
        `shouldBe` replicate 6 True
    it "allows empty list elements and handles repeated lines for all Accept fields" $ do
      let fields = headerMapFromList [("accept-encoding", " , gzip;Q=0., "), ("accept-encoding", "br;q=0.125"), ("accept-language", "en-US;Q=1. , ,"), ("accept-language", "fr;q=0.5"), ("accept-charset", "utf-8;Q=0. ,"), ("accept-charset", "*")]
      lookupHeader @AcceptEncoding fields `shouldBe` Right (Just (AcceptEncoding [WeightedEncoding (NamedEncoding GZip) 0, WeightedEncoding (NamedEncoding Brotli) 0.125]))
      lookupHeader @AcceptLanguage fields `shouldBe` Right (Just (AcceptLanguage [WeightedLanguage "en-US" 1, WeightedLanguage "fr" 0.5]))
      lookupHeader @AcceptCharset fields `shouldBe` Right (Just (AcceptCharset [WeightedCharset "utf-8" 0, WeightedCharset "*" 1]))
    it "distinguishes absent from present but empty Accept-Encoding" $ do
      lookupHeader @AcceptEncoding (headerMapFromList []) `shouldBe` Right Nothing
      lookupHeader @AcceptEncoding (headerMapFromList [("accept-encoding", " \t")]) `shouldBe` Right (Just (AcceptEncoding []))
    it "preserves the spelling and order of basic Accept-Language ranges" $ do
      let fields = headerMapFromList [("accept-language", "EN-us, es-419;q=0.8"), ("Accept-Language", "ABCDEFGH-a1b2c3d4, *;q=0")]
          expected =
            AcceptLanguage [WeightedLanguage "EN-us" 1, WeightedLanguage "es-419" 0.8, WeightedLanguage "ABCDEFGH-a1b2c3d4" 1, WeightedLanguage "*" 0]
      lookupHeader @AcceptLanguage fields `shouldBe` Right (Just expected)
      renderToHeaders defaultHeaderSettings expected `shouldBe` "EN-us, es-419;q=0.8, ABCDEFGH-a1b2c3d4, *;q=0"
    it "rejects extended wildcards, overlong or empty subtags, and invalid first subtags" $ do
      let parse value = lookupHeader @AcceptLanguage (headerMapFromList [("accept-language", "en-US"), ("accept-language", value)])
      map (isLeft . parse) ["de-*-DE", "*-CH", "**", "en--US", "en-", "en_US", "123", "e1-US", "abcdefghi", "en-abcdefghi"]
        `shouldBe` replicate 10 True
    it "rejects malformed, out-of-range and duplicate weights" $ do
      let parse value = lookupHeader @Accept (headerMapFromList [("accept", value)])
      map (isLeft . parse) ["text/plain;q=0.1234", "text/plain;q=1.001", "text/plain;q=2", "text/plain;q=\"0.5\"", "text/plain;q=0;Q=1"]
        `shouldBe` replicate 5 True
    it "rejects a malformed later field line" $
      isLeft (lookupHeader @AcceptEncoding (headerMapFromList [("accept-encoding", "gzip"), ("accept-encoding", "br;q=2")])) `shouldBe` True
    it "renders quality weights as decimals with at most three fractional digits" $ do
      let value = AcceptLanguage [WeightedLanguage "en" 0.12349, WeightedLanguage "fr" 0.009, WeightedLanguage "de" 0.05]
      renderToHeaders defaultHeaderSettings value `shouldBe` "en;q=0.123, fr;q=0.009, de;q=0.05"
    it "assigns media qualities using the RFC 9110 precedence example" $ do
      let fields = headerMapFromList
            [ ("Accept", "text/*;q=0.3, text/plain;q=0.7, text/plain;format=flowed")
            , ("accept", "text/plain;format=fixed;q=0.4, */*;q=0.5")
            ]
          plain = MediaType "text" "plain"
          flowed = ParameterizedContentType plain (("format", "flowed") :| [])
          fixed = ParameterizedContentType plain (("format", "fixed") :| [])
          available = [ContentType plain, ContentType (MediaType "text" "html"), ContentType (MediaType "image" "jpeg"), fixed, flowed]
      preferences <- either expectationFailure pure (lookupHeader @Accept fields)
      map (acceptedContentQuality preferences) available `shouldBe` [0.7, 0.3, 0.5, 0.4, 1]
      preferredContentType preferences available `shouldBe` Just flowed
    it "lets a specific exclusion override a wildcard across repeated lines" $ do
      let fields = headerMapFromList
            [ ("Accept", "*/*;q=0.8, text/*;q=0.9")
            , ("accept", "TEXT/PLAIN;CHARSET=\"UTF-8\";q=0, text/plain;q=0.4")
            ]
          utf8 = ParameterizedContentType (MediaType "text" "PLAIN") (("Charset", "utf-8") :| [("level", "2")])
          plain = ContentType (MediaType "text" "plain")
          json = ContentType (MediaType "application" "json")
      preferences <- either expectationFailure pure (lookupHeader @Accept fields)
      acceptedContentQuality preferences utf8 `shouldBe` 0
      acceptedContentQuality preferences plain `shouldBe` 0.4
      preferredContentType preferences [utf8, plain, json] `shouldBe` Just json
      preferredContentType preferences [utf8] `shouldBe` Nothing
    it "matches decoded parameters by name and keeps specificity ties in wire order" $ do
      let fields = headerMapFromList [("Accept",
            "TEXT/plain;profile=\"Case\";q=0.9, text/plain;PROFILE=Case;profile=Case;q=0, text/plain;profile=case;q=0.3, text/plain;level=1;profile=Case;q=0.7")]
          plain = MediaType "text" "plain"
          exact = ParameterizedContentType plain (("PROFILE", "Case") :| [])
          other = ParameterizedContentType plain (("profile", "case") :| [])
          narrowed = ParameterizedContentType plain (("profile", "Case") :| [("Level", "1")])
      preferences <- either expectationFailure pure (lookupHeader @Accept fields)
      map (acceptedContentQuality preferences) [exact, other, narrowed] `shouldBe` [0.9, 0.3, 0.7]
    it "handles absent and empty fields, equal qualities and authored weights" $ do
      let plain = ContentType (MediaType "text" "plain")
          html = ContentType (MediaType "text" "html")
          noMatch = Just (Accept [WeightedMediaRange (MediaRange (MediaType "image" "") []) 1])
          wildcard q = Just (Accept [WeightedMediaRange (MediaRange (MediaType "" "") []) q])
      preferredContentType Nothing [plain, html] `shouldBe` Just plain
      preferredContentType Nothing [] `shouldBe` Nothing
      preferredContentType (Just (Accept [])) [plain] `shouldBe` Nothing
      preferredContentType noMatch [plain, html] `shouldBe` Nothing
      preferredContentType (wildcard 0.5) [html, plain] `shouldBe` Just html
      map (\q -> acceptedContentQuality (wildcard q) plain) [-2, 2, 0 / 0] `shouldBe` [0, 1, 0]
  describe "Accept-Language selection" $ do
    it "matches case-insensitively at subtag boundaries" $ do
      let fields = headerMapFromList [("Accept-Language", "dE-De;q=0.8")]
          tags = map LanguageTag ["de-DE-1996", "de-Deva", "de-Latn-DE", "de", "DE-de"]
      preferences <- either expectationFailure pure (lookupHeader @AcceptLanguage fields)
      map (acceptedLanguageQuality preferences) tags `shouldBe` [0.8, 0, 0, 0, 0.8]
      preferredLanguage preferences tags `shouldBe` Just (LanguageTag "de-DE-1996")
    it "uses the longest range before weight, including an explicit exclusion" $ do
      let fields = headerMapFromList
            [ ("Accept-Language", "*;q=0.9, zh;q=0.2, en;q=0.7")
            , ("accept-language", "ZH-hant;q=0.6, zh-HANT-TW;q=0, en-US;q=0")
            ]
          tags = map LanguageTag ["zh-Hans-CN", "zh-Hant-HK", "zh-Hant-TW", "en-US", "en-GB", "fr"]
      preferences <- either expectationFailure pure (lookupHeader @AcceptLanguage fields)
      map (acceptedLanguageQuality preferences) tags `shouldBe` [0.2, 0.6, 0, 0, 0.7, 0.9]
      preferredLanguage preferences (map LanguageTag ["en-US", "zh-Hant-TW"]) `shouldBe` Nothing
      preferredLanguage preferences tags `shouldBe` Just (LanguageTag "fr")
    it "keeps wire order for repeated ranges and server order for equally rated tags" $ do
      let fields = headerMapFromList [("Accept-Language", "EN;q=0.4, fr;q=0.4"), ("accept-language", "en;q=1, *;q=0")]
          english = LanguageTag "en-GB"
          french = LanguageTag "fr-FR"
      preferences <- either expectationFailure pure (lookupHeader @AcceptLanguage fields)
      acceptedLanguageQuality preferences english `shouldBe` 0.4
      preferredLanguage preferences [french, english] `shouldBe` Just french
    it "distinguishes absent, empty, and wildcard-only preferences" $ do
      let offered = [LanguageTag "es-419", LanguageTag "en"]
          emptyFields = headerMapFromList [("Accept-Language", " , ")]
          wildcard q = Just (AcceptLanguage [WeightedLanguage "*" q])
      empty <- either expectationFailure pure (lookupHeader @AcceptLanguage emptyFields)
      empty `shouldBe` Just (AcceptLanguage [])
      preferredLanguage empty offered `shouldBe` Nothing
      preferredLanguage Nothing offered `shouldBe` Just (LanguageTag "es-419")
      preferredLanguage (wildcard 0) offered `shouldBe` Nothing
      preferredLanguage (wildcard 1) offered `shouldBe` Just (LanguageTag "es-419")
