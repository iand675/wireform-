{-# LANGUAGE OverloadedStrings #-}
{- | Pure tests over the 'VersionRange' negotiation surface.

These don't open any sockets — they just verify that the range
constructors produce the right ALPN protocol lists and that
'versionAllowed' / 'preferredVersion' give the answers the
client \/ server dispatch relies on.
-}
module Test.Negotiation (tests) where

import qualified Data.List.NonEmpty as NE

import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP.VersionRange
import qualified Network.HTTP.Types.Version as V

tests :: TestTree
tests = testGroup "VersionRange"
  [ namedRanges
  , alpnLists
  , membership
  , alpnRoundTrips
  ]

namedRanges :: TestTree
namedRanges = testCase "named ranges have the documented preference order" $ do
  NE.toList (versionRangeList anyVersion) @?= [V.HTTP2, V.HTTP1_1, V.HTTP1_0]
  NE.toList (versionRangeList http2Only)  @?= [V.HTTP2]
  NE.toList (versionRangeList http1Only)  @?= [V.HTTP1_1, V.HTTP1_0]
  NE.toList (versionRangeList preferHttp2) @?= [V.HTTP2, V.HTTP1_1, V.HTTP1_0]
  NE.toList (versionRangeList preferHttp1) @?= [V.HTTP1_1, V.HTTP2, V.HTTP1_0]
  NE.toList (versionRangeList http2OrHttp11) @?= [V.HTTP2, V.HTTP1_1]
  preferredVersion http2Only   @?= V.HTTP2
  preferredVersion preferHttp1 @?= V.HTTP1_1

alpnLists :: TestTree
alpnLists = testCase "ALPN protocol lists mirror the range's preference order" $ do
  versionAlpnProtocols http2Only      @?= ["h2"]
  versionAlpnProtocols http1Only      @?= ["http/1.1", "http/1.0"]
  versionAlpnProtocols http2OrHttp11  @?= ["h2", "http/1.1"]
  versionAlpnProtocols preferHttp1    @?= ["http/1.1", "h2", "http/1.0"]

membership :: TestTree
membership = testCase "versionAllowed matches the constructor's allowlist" $ do
  versionAllowed V.HTTP2 http2Only @?= True
  versionAllowed V.HTTP1_1 http2Only @?= False
  versionAllowed V.HTTP1_1 http1Only @?= True
  versionAllowed V.HTTP2 http1Only @?= False
  versionAllowed V.HTTP1_0 http1Only @?= True
  versionAllowed V.HTTP3 anyVersion @?= False

alpnRoundTrips :: TestTree
alpnRoundTrips = testCase "alpnForVersion / versionForAlpn are inverses" $ do
  alpnForVersion V.HTTP2 @?= Just "h2"
  alpnForVersion V.HTTP1_1 @?= Just "http/1.1"
  alpnForVersion V.HTTP1_0 @?= Just "http/1.0"
  alpnForVersion V.HTTP3 @?= Just "h3"
  versionForAlpn "h2" @?= Just V.HTTP2
  versionForAlpn "http/1.1" @?= Just V.HTTP1_1
  versionForAlpn "h3" @?= Just V.HTTP3
  versionForAlpn "h2c" @?= Nothing
