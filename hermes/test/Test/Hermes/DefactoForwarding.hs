{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.DefactoForwarding (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import qualified Network.HTTP.Headers.XForwardedFor as XFF
import qualified Network.HTTP.Headers.XForwardedHost as XFH
import qualified Network.HTTP.Headers.XForwardedPort as XFP
import qualified Network.HTTP.Headers.XForwardedProto as XFPr
import qualified Network.HTTP.Headers.XHttpMethodOverride as XMO
import qualified Network.HTTP.Headers.XRealIP as XRI
import Network.HTTP.Methods (mConnect, mDelete, mGet, mHead, mOptions, mPatch, mPost, mPut, mTrace)
import Network.IPAddress (IPAddress (..), IPv4 (..))
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Run a parser to completion, tolerating only trailing OWS.
runFully :: ParserT st String a -> ByteString -> Either String a
runFully p bs = case runParser p bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


-- ---------------------------------------------------------------------------
-- X-Forwarded-For
-- ---------------------------------------------------------------------------

renderXFF :: XFF.XForwardedFor -> ByteString
renderXFF = M.toStrictByteString . XFF.renderXForwardedFor


unit_xff_parse :: Spec
unit_xff_parse = it "X-Forwarded-For parses a node chain" $
  case runFully XFF.xForwardedForParser "203.0.113.195, 70.41.3.18, 150.172.238.178" of
    Right (XFF.XForwardedFor ns) ->
      NE.toList ns
        `shouldBe` map ST.fromString ["203.0.113.195", "70.41.3.18", "150.172.238.178"]
    other -> error (show other)


unit_xff_parse_mixed :: Spec
unit_xff_parse_mixed = it "X-Forwarded-For keeps bracketed/obfuscated nodes verbatim" $
  case runFully XFF.xForwardedForParser "[2001:db8::1]:8080, _hidden, unknown" of
    Right (XFF.XForwardedFor ns) ->
      NE.toList ns
        `shouldBe` map ST.fromString ["[2001:db8::1]:8080", "_hidden", "unknown"]
    other -> error (show other)


unit_xff_render :: Spec
unit_xff_render =
  it "X-Forwarded-For renders comma-separated" $
    renderXFF (XFF.XForwardedFor (ST.fromString "192.0.2.43" :| [ST.fromString "198.51.100.17"]))
      `shouldBe` "192.0.2.43, 198.51.100.17"


nodeGen :: Gen ST.ShortText
nodeGen = ST.fromString <$> Gen.list (Range.linear 1 15) (Gen.element nodeChars)
  where
    nodeChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ".:[]_-"


xffGen :: Gen XFF.XForwardedFor
xffGen = do
  first <- nodeGen
  rest <- Gen.list (Range.linear 0 4) nodeGen
  pure (XFF.XForwardedFor (first :| rest))


prop_xff_roundtrip :: Property
prop_xff_roundtrip = property $ do
  v <- forAll xffGen
  let bs = renderXFF v
  case runFully XFF.xForwardedForParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- X-Forwarded-Host
-- ---------------------------------------------------------------------------

renderXFH :: XFH.XForwardedHost -> ByteString
renderXFH = M.toStrictByteString . XFH.renderXForwardedHost


unit_xfh_parse :: Spec
unit_xfh_parse = it "X-Forwarded-Host preserves host:port verbatim" $
  case runFully XFH.xForwardedHostParser "example.com:8443" of
    Right h -> XFH.xForwardedHostValue h `shouldBe` ST.fromString "example.com:8443"
    other -> error (show other)


unit_xfh_render :: Spec
unit_xfh_render =
  it "X-Forwarded-Host renders the raw value" $
    renderXFH (XFH.XForwardedHost (ST.fromString "origin.internal")) `shouldBe` "origin.internal"


-- ---------------------------------------------------------------------------
-- X-Forwarded-Proto
-- ---------------------------------------------------------------------------

renderXFPr :: XFPr.XForwardedProto -> ByteString
renderXFPr = M.toStrictByteString . XFPr.renderXForwardedProto


unit_xfpr_https :: Spec
unit_xfpr_https = it "X-Forwarded-Proto parses https" $
  case runFully XFPr.xForwardedProtoParser "https" of
    Right XFPr.XForwardedProtoHttps -> pure () :: IO ()
    other -> error (show other)


unit_xfpr_http :: Spec
unit_xfpr_http = it "X-Forwarded-Proto parses http" $
  case runFully XFPr.xForwardedProtoParser "http" of
    Right XFPr.XForwardedProtoHttp -> pure () :: IO ()
    other -> error (show other)


unit_xfpr_other :: Spec
unit_xfpr_other = it "X-Forwarded-Proto keeps an unknown scheme" $
  case runFully XFPr.xForwardedProtoParser "wss" of
    Right (XFPr.XForwardedProtoOther t) -> t `shouldBe` ST.fromString "wss"
    other -> error (show other)


unit_xfpr_render :: Spec
unit_xfpr_render =
  it "X-Forwarded-Proto renders https" $
    renderXFPr XFPr.XForwardedProtoHttps `shouldBe` "https"


protoGen :: Gen XFPr.XForwardedProto
protoGen =
  Gen.choice
    [ pure XFPr.XForwardedProtoHttp
    , pure XFPr.XForwardedProtoHttps
    , XFPr.XForwardedProtoOther <$> otherTok
    ]
  where
    otherTok =
      Gen.filter (\t -> t /= "http" && t /= "https") $
        ST.fromString <$> Gen.list (Range.linear 1 8) (Gen.element schemeChars)
    schemeChars = ['a' .. 'z'] ++ ['0' .. '9'] ++ "+-."


prop_xfpr_roundtrip :: Property
prop_xfpr_roundtrip = property $ do
  v <- forAll protoGen
  let bs = renderXFPr v
  case runFully XFPr.xForwardedProtoParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- X-Forwarded-Port
-- ---------------------------------------------------------------------------

renderXFP :: XFP.XForwardedPort -> ByteString
renderXFP = M.toStrictByteString . XFP.renderXForwardedPort


unit_xfp_parse :: Spec
unit_xfp_parse = it "X-Forwarded-Port parses a port number" $
  case runFully XFP.xForwardedPortParser "443" of
    Right p -> XFP.xForwardedPortNumber p `shouldBe` 443
    other -> error (show other)


unit_xfp_render :: Spec
unit_xfp_render =
  it "X-Forwarded-Port renders a port number" $
    renderXFP (XFP.XForwardedPort 8080) `shouldBe` "8080"


portGen :: Gen XFP.XForwardedPort
portGen = XFP.XForwardedPort <$> Gen.word (Range.linear 0 65535)


prop_xfp_roundtrip :: Property
prop_xfp_roundtrip = property $ do
  v <- forAll portGen
  let bs = renderXFP v
  case runFully XFP.xForwardedPortParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- X-Real-IP
-- ---------------------------------------------------------------------------

renderXRI :: XRI.XRealIP -> ByteString
renderXRI = M.toStrictByteString . XRI.renderXRealIP


unit_xri_parse_v4 :: Spec
unit_xri_parse_v4 = it "X-Real-IP parses an IPv4 literal" $
  case runFully XRI.xRealIPParser "192.0.2.146" of
    Right (XRI.XRealIP (IPv4Address (IPv4 192 0 2 146))) -> pure () :: IO ()
    other -> error (show other)


unit_xri_parse_v6 :: Spec
unit_xri_parse_v6 = it "X-Real-IP parses an IPv6 literal" $
  case runFully XRI.xRealIPParser "2001:db8::1" of
    Right (XRI.XRealIP (IPv6Address _)) -> pure () :: IO ()
    other -> error (show other)


unit_xri_render_v4 :: Spec
unit_xri_render_v4 =
  it "X-Real-IP renders an IPv4 literal" $
    renderXRI (XRI.XRealIP (IPv4Address (IPv4 192 0 2 146))) `shouldBe` "192.0.2.146"


xriGen :: Gen XRI.XRealIP
xriGen = do
  a <- octet
  b <- octet
  c <- octet
  XRI.XRealIP . IPv4Address . IPv4 a b c <$> octet
  where
    octet = Gen.word8 (Range.linear 0 255)


prop_xri_roundtrip :: Property
prop_xri_roundtrip = property $ do
  v <- forAll xriGen
  let bs = renderXRI v
  case runFully XRI.xRealIPParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------
-- X-Http-Method-Override
-- ---------------------------------------------------------------------------

renderXMO :: XMO.XHttpMethodOverride -> ByteString
renderXMO = M.toStrictByteString . XMO.renderXHttpMethodOverride


unit_xmo_parse :: Spec
unit_xmo_parse = it "X-Http-Method-Override parses a method token" $
  case runFully XMO.xHttpMethodOverrideParser "PATCH" of
    Right v -> XMO.xHttpMethodOverrideMethod v `shouldBe` mPatch
    other -> error (show other)


unit_xmo_render :: Spec
unit_xmo_render =
  it "X-Http-Method-Override renders the method" $
    renderXMO (XMO.XHttpMethodOverride mDelete) `shouldBe` "DELETE"


methodGen :: Gen XMO.XHttpMethodOverride
methodGen =
  XMO.XHttpMethodOverride
    <$> Gen.element [mGet, mPost, mPut, mDelete, mPatch, mHead, mOptions, mConnect, mTrace]


prop_xmo_roundtrip :: Property
prop_xmo_roundtrip = property $ do
  v <- forAll methodGen
  let bs = renderXMO v
  case runFully XMO.xHttpMethodOverrideParser bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "DefactoForwarding" $
    sequence_
      [ describe "X-Forwarded-For" $
          sequence_
            [ unit_xff_parse
            , unit_xff_parse_mixed
            , unit_xff_render
            , it "round-trip" prop_xff_roundtrip
            ]
      , describe "X-Forwarded-Host" $
          sequence_
            [ unit_xfh_parse
            , unit_xfh_render
            ]
      , describe "X-Forwarded-Proto" $
          sequence_
            [ unit_xfpr_https
            , unit_xfpr_http
            , unit_xfpr_other
            , unit_xfpr_render
            , it "round-trip" prop_xfpr_roundtrip
            ]
      , describe "X-Forwarded-Port" $
          sequence_
            [ unit_xfp_parse
            , unit_xfp_render
            , it "round-trip" prop_xfp_roundtrip
            ]
      , describe "X-Real-IP" $
          sequence_
            [ unit_xri_parse_v4
            , unit_xri_parse_v6
            , unit_xri_render_v4
            , it "round-trip" prop_xri_roundtrip
            ]
      , describe "X-Http-Method-Override" $
          sequence_
            [ unit_xmo_parse
            , unit_xmo_render
            , it "round-trip" prop_xmo_roundtrip
            ]
      ]
