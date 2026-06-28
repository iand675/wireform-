{-# LANGUAGE OverloadedStrings #-}

{- | Pure tests over the 'VersionRange' negotiation surface.

These don't open any sockets — they just verify that the range
constructors produce the right ALPN protocol lists and that
'versionAllowed' / 'preferredVersion' give the answers the
client \/ server dispatch relies on.
-}
module Test.Negotiation (tests) where

import Data.List.NonEmpty qualified as NE
import Network.HTTP.Types.Version qualified as V
import Network.HTTP.VersionRange
import Test.Syd


tests :: Spec
tests =
  describe "VersionRange" $
    sequence_
      [ namedRanges
      , alpnLists
      , membership
      , alpnRoundTrips
      ]


namedRanges :: Spec
namedRanges = it "named ranges have the documented preference order" $ do
  NE.toList (versionRangeList anyVersion) `shouldBe` [V.HTTP2, V.HTTP1_1, V.HTTP1_0]
  NE.toList (versionRangeList http2Only) `shouldBe` [V.HTTP2]
  NE.toList (versionRangeList http1Only) `shouldBe` [V.HTTP1_1, V.HTTP1_0]
  NE.toList (versionRangeList preferHttp2) `shouldBe` [V.HTTP2, V.HTTP1_1, V.HTTP1_0]
  NE.toList (versionRangeList preferHttp1) `shouldBe` [V.HTTP1_1, V.HTTP2, V.HTTP1_0]
  NE.toList (versionRangeList http2OrHttp11) `shouldBe` [V.HTTP2, V.HTTP1_1]
  preferredVersion http2Only `shouldBe` V.HTTP2
  preferredVersion preferHttp1 `shouldBe` V.HTTP1_1


alpnLists :: Spec
alpnLists = it "ALPN protocol lists mirror the range's preference order" $ do
  versionAlpnProtocols http2Only `shouldBe` ["h2"]
  versionAlpnProtocols http1Only `shouldBe` ["http/1.1", "http/1.0"]
  versionAlpnProtocols http2OrHttp11 `shouldBe` ["h2", "http/1.1"]
  versionAlpnProtocols preferHttp1 `shouldBe` ["http/1.1", "h2", "http/1.0"]


membership :: Spec
membership = it "versionAllowed matches the constructor's allowlist" $ do
  versionAllowed V.HTTP2 http2Only `shouldBe` True
  versionAllowed V.HTTP1_1 http2Only `shouldBe` False
  versionAllowed V.HTTP1_1 http1Only `shouldBe` True
  versionAllowed V.HTTP2 http1Only `shouldBe` False
  versionAllowed V.HTTP1_0 http1Only `shouldBe` True
  versionAllowed V.HTTP3 anyVersion `shouldBe` False


alpnRoundTrips :: Spec
alpnRoundTrips = it "alpnForVersion / versionForAlpn are inverses" $ do
  alpnForVersion V.HTTP2 `shouldBe` Just "h2"
  alpnForVersion V.HTTP1_1 `shouldBe` Just "http/1.1"
  alpnForVersion V.HTTP1_0 `shouldBe` Just "http/1.0"
  alpnForVersion V.HTTP3 `shouldBe` Just "h3"
  versionForAlpn "h2" `shouldBe` Just V.HTTP2
  versionForAlpn "http/1.1" `shouldBe` Just V.HTTP1_1
  versionForAlpn "h3" `shouldBe` Just V.HTTP3
  versionForAlpn "h2c" `shouldBe` Nothing
