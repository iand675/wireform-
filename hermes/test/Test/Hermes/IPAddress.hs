{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.IPAddress (tests) where

import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.IPAddress as IP
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Unit examples
-- ---------------------------------------------------------------------------

unit_ipv4 :: Spec
unit_ipv4 =
  it "parses dotted-quad IPv4" $
    IP.parseIPv4 "192.168.0.1" `shouldBe` Right (IP.IPv4 192 168 0 1)


unit_loopback :: Spec
unit_loopback =
  it "parses ::1" $
    IP.parseIPv6 "::1" `shouldBe` Right (IP.IPv6 0 0 0 0 0 0 0 1)


unit_doc :: Spec
unit_doc =
  it "parses 2001:db8::1" $
    IP.parseIPv6 "2001:db8::1" `shouldBe` Right (IP.IPv6 0x2001 0x0db8 0 0 0 0 0 1)


unit_v4mapped :: Spec
unit_v4mapped =
  it "parses ::ffff:192.0.2.1 (IPv4-mapped tail)" $
    IP.parseIPv6 "::ffff:192.0.2.1"
      `shouldBe` Right (IP.IPv6 0 0 0 0 0 0xffff 0xc000 0x0201)


unit_linklocal :: Spec
unit_linklocal =
  it "parses fe80::1" $
    IP.parseIPv6 "fe80::1" `shouldBe` Right (IP.IPv6 0xfe80 0 0 0 0 0 0 1)


unit_canonical :: Spec
unit_canonical = it "canonicalizes fully-expanded form to RFC 5952" $
  case IP.parseIPv6 "2001:0db8:0000:0000:0000:0000:0000:0001" of
    Right ip -> M.toStrictByteString (IP.renderIPv6 ip) `shouldBe` "2001:db8::1"
    Left err -> error err


unit_render_ipv4 :: Spec
unit_render_ipv4 =
  it "renders IPv4 as dotted-quad" $
    M.toStrictByteString (IP.renderIPv4 (IP.IPv4 192 168 0 1)) `shouldBe` "192.168.0.1"


unit_render_ipv6 :: Spec
unit_render_ipv6 =
  it "renders ::1" $
    M.toStrictByteString (IP.renderIPv6 (IP.IPv6 0 0 0 0 0 0 0 1)) `shouldBe` "::1"


unit_ipvfuture :: Spec
unit_ipvfuture =
  it "parses an IPvFuture literal verbatim" $
    IP.parseIPAddress "v1.fe-2" `shouldBe` Right (IP.IPvFuture "v1.fe-2")


unit_dispatch_v4 :: Spec
unit_dispatch_v4 =
  it "ipAddressParser dispatches to IPv4" $
    IP.parseIPAddress "192.168.0.1"
      `shouldBe` Right (IP.IPv4Address (IP.IPv4 192 168 0 1))


unit_dispatch_v6 :: Spec
unit_dispatch_v6 =
  it "ipAddressParser dispatches to IPv6" $
    IP.parseIPAddress "2001:db8::1"
      `shouldBe` Right (IP.IPv6Address (IP.IPv6 0x2001 0x0db8 0 0 0 0 0 1))


-- ---------------------------------------------------------------------------
-- Round-trip properties
-- ---------------------------------------------------------------------------

ipv4Gen :: Gen IP.IPv4
ipv4Gen = IP.IPv4 <$> octet <*> octet <*> octet <*> octet
  where
    octet = Gen.word8 Range.constantBounded


ipv6Gen :: Gen IP.IPv6
ipv6Gen =
  IP.IPv6
    <$> hextet
    <*> hextet
    <*> hextet
    <*> hextet
    <*> hextet
    <*> hextet
    <*> hextet
    <*> hextet
  where
    hextet = Gen.word16 Range.constantBounded


prop_ipv4_roundtrip :: Property
prop_ipv4_roundtrip = property $ do
  ip <- forAll ipv4Gen
  let bs = M.toStrictByteString (IP.renderIPv4 ip)
  case IP.parseIPv4 bs of
    Right ip' -> ip === ip'
    Left err -> error (err <> " on " <> show bs)


prop_ipv6_roundtrip :: Property
prop_ipv6_roundtrip = property $ do
  ip <- forAll ipv6Gen
  let bs = M.toStrictByteString (IP.renderIPv6 ip)
  case IP.parseIPv6 bs of
    Right ip' -> ip === ip'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "IPAddress" $
    sequence_
      [ unit_ipv4
      , unit_loopback
      , unit_doc
      , unit_v4mapped
      , unit_linklocal
      , unit_canonical
      , unit_render_ipv4
      , unit_render_ipv6
      , unit_ipvfuture
      , unit_dispatch_v4
      , unit_dispatch_v6
      , it "IPv4 round-trip" prop_ipv4_roundtrip
      , it "IPv6 round-trip" prop_ipv6_roundtrip
      ]
