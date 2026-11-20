module Test.Hermes.Negotiation (tests) where

import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.ContentCoding (ContentCoding (..))
import Network.HTTP.ContentNegotiation (MediaRange (..), MediaType (..), WeightedMediaRange (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.Accept (Accept (..))
import Network.HTTP.Headers.AcceptCharset (AcceptCharset (..), WeightedCharset (..))
import Network.HTTP.Headers.AcceptEncoding (AcceptEncoding (..), EncodingTag (..), WeightedEncoding (..))
import Network.HTTP.Headers.AcceptLanguage (AcceptLanguage (..), WeightedLanguage (..))
import Network.HTTP.Headers.CacheControl (CacheControl (..), CacheControlDirective (..))
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
