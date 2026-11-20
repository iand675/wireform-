{-# LANGUAGE OverloadedStrings #-}

{- | Pure tests over the vendored HTTP type primitives.

These exist primarily to guard the @Network.HTTP.Types.*@ rebrand
against regressions: the same operations the original @hermes@
library shipped should still behave the same after we trimmed the
dep closure.
-}
module Test.VersionTypes (tests) where

import Data.Either (isLeft)
import Network.HTTP.Client.URI qualified as URI
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import Network.HTTP.Types.Version qualified as V
import Test.Syd


tests :: Spec
tests =
  describe "Types" $
    sequence_
      [ versionRoundTrips
      , versionOrder
      , methodConstants
      , uriPorts
      , statusCategories
      , headerLookups
      ]


versionRoundTrips :: Spec
versionRoundTrips = it "Version round-trips through canonical bytes" $ do
  V.versionFromBytes "HTTP/1.0" `shouldBe` Just V.HTTP1_0
  V.versionFromBytes "HTTP/1.1" `shouldBe` Just V.HTTP1_1
  V.versionFromBytes "HTTP/2" `shouldBe` Just V.HTTP2
  V.versionFromBytes "HTTP/2.0" `shouldBe` Just V.HTTP2
  V.versionFromBytes "HTTP/3" `shouldBe` Just V.HTTP3
  V.versionFromBytes "HTTP/9.9" `shouldBe` Nothing
  V.versionToBytes V.HTTP1_0 `shouldBe` "HTTP/1.0"
  V.versionToBytes V.HTTP1_1 `shouldBe` "HTTP/1.1"
  V.versionToBytes V.HTTP2 `shouldBe` "HTTP/2"


versionOrder :: Spec
versionOrder = it "Version Ord is lexicographic on (major, minor)" $ do
  (V.HTTP1_0 < V.HTTP1_1) `shouldBe` True
  (V.HTTP1_1 < V.HTTP2) `shouldBe` True
  (V.HTTP2 < V.HTTP3) `shouldBe` True
  V.versionMajor V.HTTP1_1 `shouldBe` 1
  V.versionMinor V.HTTP1_1 `shouldBe` 1
  V.versionMajor V.HTTP2 `shouldBe` 2
  V.versionMinor V.HTTP2 `shouldBe` 0


methodConstants :: Spec
methodConstants = it "Method constants serialise correctly" $ do
  M.methodToBytes M.mGet `shouldBe` "GET"
  M.methodToBytes M.mPost `shouldBe` "POST"
  M.methodToBytes M.mPropFind `shouldBe` "PROPFIND"
  (M.isSafe M.mGet) `shouldBe` True
  (M.isIdempotent M.mGet) `shouldBe` True
  (not (M.isSafe M.mPost)) `shouldBe` True
  (not (M.isIdempotent M.mPost)) `shouldBe` True
  (M.bodyAllowedInRequest M.mPost) `shouldBe` True
  let safe = [M.mQuery, M.mPri, M.mPropFind, M.mReport, M.mSearch]
  map M.isSafe safe `shouldBe` replicate 5 True
  map M.isIdempotent safe `shouldBe` replicate 5 True
  map M.isSafe ["query", "search", "EXTEND"] `shouldBe` replicate 3 False


uriPorts :: Spec
uriPorts = it "applies bounded HTTP ports and parses bracketed IPv6 hosts" $ do
  (URI.uriPort <$> URI.parseURI "http://example.test:/") `shouldBe` Right 80
  (URI.uriPort <$> URI.parseURI "https://example.test:65535/") `shouldBe` Right 65535
  case URI.parseURI "http://[2001:db8::1]:8080/path" of
    Right uri -> do
      URI.uriHost uri `shouldBe` "2001:db8::1"
      URI.uriPort uri `shouldBe` 8080
      URI.renderURI uri `shouldBe` "http://[2001:db8::1]:8080/path"
    Left err -> expectationFailure err
  map (isLeft . URI.parseURI)
    [ "http://example.test:65536/", "http://example.test:18446744073709551696/"
    , "http://[::1]:65536/", "http://example.test:+80/", "http://example.test:http/"
    ] `shouldBe` replicate 5 True
  map URI.removeDotSegments ["/a/b/.", "/a/b/..", "/a/./b", "/a/../b"]
    `shouldBe` ["/a/b/", "/a/", "/a/b", "/b"]


statusCategories :: Spec
statusCategories = it "Status categorises by 100s digit" $ do
  S.statusCategory S.status200 `shouldBe` S.Successful
  S.statusCategory S.status301 `shouldBe` S.Redirection
  S.statusCategory S.status404 `shouldBe` S.ClientError
  S.statusCategory S.status500 `shouldBe` S.ServerError
  S.statusCategory (S.Status 0) `shouldBe` S.Other
  S.statusReason S.status200 `shouldBe` "OK"
  S.statusReason S.status404 `shouldBe` "Not Found"
  S.statusReason (S.Status 999) `shouldBe` ""


headerLookups :: Spec
headerLookups = it "Header lookups are case-insensitive and order-preserving" $ do
  let hs = [(H.hContentType, "text/plain"), (H.hContentLength, "10"), (H.hContentType, "application/json")]
  H.lookupHeader H.hContentType hs `shouldBe` Just "text/plain"
  H.lookupHeaders H.hContentType hs `shouldBe` ["text/plain", "application/json"]
  H.hasHeader H.hContentLength hs `shouldBe` True
  H.hasHeader H.hAuthorization hs `shouldBe` False
  let hs2 = H.insertHeader H.hContentType "text/html" hs
  H.lookupHeaders H.hContentType hs2 `shouldBe` ["text/html"]
  let hs3 = H.deleteHeader H.hContentType hs
  H.lookupHeader H.hContentType hs3 `shouldBe` Nothing
