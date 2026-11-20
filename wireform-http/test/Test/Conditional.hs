{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "Network.HTTP.Client.Conditional".

ETag and If-Match grammars are parsed and rendered by hermes;
these tests lock down the shape of what wireform users see.
-}
module Test.Conditional (tests) where

import Data.ByteString qualified as BS
import Network.HTTP.Client.Conditional
import Network.HTTP.Client.Request qualified as Request
import Network.HTTP.Client.URI qualified as URI
import Network.HTTP.Types.Header qualified as H
import Test.Syd


tests :: Spec
tests =
  describe "Network.HTTP.Client.Conditional" $
    sequence_
      [ describe "ETag" $
          sequence_
            [ it "strong tag round-trips through render/parse" $ do
                let t = strongETag "abc123"
                renderETag t `shouldBe` "\"abc123\""
                parseETag (renderETag t) `shouldBe` Just t
            , it "weak tag carries the W/ prefix" $
                renderETag (weakETag "v2") `shouldBe` "W/\"v2\""
            , it "parseETag rejects malformed input" $
                parseETag "not an etag" `shouldBe` Nothing
            , it "keeps opaque non-UTF-8 bytes in validators and outgoing preconditions" $ do
                let first = BS.singleton 0x80
                    second = BS.singleton 0xff
                    strong = strongETag first
                    weak = weakETag first
                    wire = "\"" <> first <> "\""
                parseETag wire `shouldBe` Just strong
                parseETag ("W/" <> wire) `shouldBe` Just weak
                renderETag strong `shouldBe` wire
                ifMatchHeader [strong] `shouldBe` wire
                ifNoneMatchHeader [weak] `shouldBe` "W/" <> wire
                ifRangeHeader (IfRangeETag strong) `shouldBe` wire
                strongMatch strong (strongETag first) `shouldBe` True
                strongMatch strong weak `shouldBe` False
                weakMatch strong weak `shouldBe` True
                strongMatch strong (strongETag second) `shouldBe` False
                weakMatch strong (weakETag second) `shouldBe` False
            ]
      , describe "If-Match" $
          sequence_
            [ it "empty list collapses to wildcard" $
                ifMatchHeader [] `shouldBe` "*"
            , it "non-empty list comma-joins entity tags" $
                ifMatchHeader [strongETag "a", weakETag "b"]
                  `shouldBe` "\"a\", W/\"b\""
            , it "If-None-Match wildcard" $
                ifNoneMatchHeader [] `shouldBe` "*"
            ]
      , describe "If-Modified-Since" $
          sequence_
            [ it "renders an IMF-fixdate" $
                let bs = ifModifiedSinceHeader (read "1994-11-06 08:49:37 UTC")
                in bs `shouldBe` "Sun, 06 Nov 1994 08:49:37 GMT"
            ]
      , describe "If-Range" $ do
          it "replaces repeated field lines with a strong validator" $ do
            let opaque = BS.singleton 0x80
                original = rangeRequest [("If-Range", "\"v1\""), ("Range", "items=2-3"), ("if-range", "\"v2\"")]
            Request.headers (ifRange (IfRangeETag (strongETag opaque)) original)
              `shouldBe` [(H.hIfRange, "\"" <> opaque <> "\""), (H.hRange, "items=2-3")]
          it "sets a caller-supplied strong date validator" $ do
            let date = read "1994-11-06 08:49:37 UTC"
                original = rangeRequest [(H.hRange, "bytes=10-")]
            Request.headers (ifRange (IfRangeDate date) original)
              `shouldBe` [(H.hRange, "bytes=10-"), (H.hIfRange, "Sun, 06 Nov 1994 08:49:37 GMT")]
      , describe "Validator comparison (RFC 9110 §8.8.3)" $
          sequence_
            [ it "strongMatch: identical strong tags match" $
                strongMatch (strongETag "abc") (strongETag "abc") `shouldBe` True
            , it "strongMatch: strong vs weak with same bytes does NOT match" $
                strongMatch (strongETag "abc") (weakETag "abc") `shouldBe` False
            , it "strongMatch: weak vs weak does NOT match" $
                strongMatch (weakETag "abc") (weakETag "abc") `shouldBe` False
            , it "strongMatch: different opaque bytes do not match" $
                strongMatch (strongETag "abc") (strongETag "def") `shouldBe` False
            , it "weakMatch: strong-strong matches when bytes agree" $
                weakMatch (strongETag "abc") (strongETag "abc") `shouldBe` True
            , it "weakMatch: strong-weak matches when bytes agree" $
                weakMatch (strongETag "abc") (weakETag "abc") `shouldBe` True
            , it "weakMatch: weak-weak matches when bytes agree" $
                weakMatch (weakETag "abc") (weakETag "abc") `shouldBe` True
            , it "weakMatch: different bytes do not match" $
                weakMatch (strongETag "abc") (weakETag "abd") `shouldBe` False
            ]
      ]

rangeRequest :: H.Headers -> Request.Request ()
rangeRequest fields = (Request.get template) {Request.headers = fields}
  where
    template = either (error . show) id (URI.parseTemplate "https://example.com/download")
