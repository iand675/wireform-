{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ObsoleteProfile (tests) where

import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.GetProfile as GP
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.P3P as P3
import qualified Network.HTTP.Headers.PICSLabel as PL
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.ProfileObject as PO
import qualified Network.HTTP.Headers.SetProfile as SP
import Test.Syd
import Test.Syd.Hedgehog ()


{- | Shared generator of opaque header values: non-empty printable ASCII.
These headers preserve their value verbatim, so any such string round-trips.
-}
genValue :: Gen ST.ShortText
genValue =
  ST.fromString
    <$> Gen.string
      (Range.linear 1 48)
      (Gen.element ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ()=\";:/.,-_*" :: String))


-- GetProfile -----------------------------------------------------------------

unit_getProfile_parse :: Spec
unit_getProfile_parse = it "parses GetProfile value verbatim" $
  case runParser GP.getProfileParser "\"*\" \"user.name\"" of
    OK v "" -> GP.getProfileValue v `shouldBe` ST.fromString "\"*\" \"user.name\""
    other -> error (show other)


unit_getProfile_render :: Spec
unit_getProfile_render =
  it "renders GetProfile value verbatim" $
    M.toStrictByteString (GP.renderGetProfile (GP.GetProfile (ST.fromString "\"*\" \"user.name\"")))
      `shouldBe` "\"*\" \"user.name\""


prop_getProfile :: Property
prop_getProfile = property $ do
  v <- forAll genValue
  let val = GP.GetProfile v
      bs = M.toStrictByteString (GP.renderGetProfile val)
  case runParser GP.getProfileParser bs of
    OK val' "" -> val' === val
    other -> error (show other <> " on " <> show bs)


-- SetProfile -----------------------------------------------------------------

unit_setProfile_parse :: Spec
unit_setProfile_parse = it "parses SetProfile value verbatim" $
  case runParser SP.setProfileParser "\"user.bdate=1970.01.01\"" of
    OK v "" -> SP.setProfileValue v `shouldBe` ST.fromString "\"user.bdate=1970.01.01\""
    other -> error (show other)


unit_setProfile_render :: Spec
unit_setProfile_render =
  it "renders SetProfile value verbatim" $
    M.toStrictByteString (SP.renderSetProfile (SP.SetProfile (ST.fromString "\"user.bdate=1970.01.01\"")))
      `shouldBe` "\"user.bdate=1970.01.01\""


prop_setProfile :: Property
prop_setProfile = property $ do
  v <- forAll genValue
  let val = SP.SetProfile v
      bs = M.toStrictByteString (SP.renderSetProfile val)
  case runParser SP.setProfileParser bs of
    OK val' "" -> val' === val
    other -> error (show other <> " on " <> show bs)


-- ProfileObject --------------------------------------------------------------

unit_profileObject_parse :: Spec
unit_profileObject_parse = it "parses ProfileObject value verbatim" $
  case runParser PO.profileObjectParser "MIIBkzCCAX2gAwIBAgIBADAN" of
    OK v "" -> PO.profileObjectValue v `shouldBe` ST.fromString "MIIBkzCCAX2gAwIBAgIBADAN"
    other -> error (show other)


unit_profileObject_render :: Spec
unit_profileObject_render =
  it "renders ProfileObject value verbatim" $
    M.toStrictByteString (PO.renderProfileObject (PO.ProfileObject (ST.fromString "MIIBkzCCAX2gAwIBAgIBADAN")))
      `shouldBe` "MIIBkzCCAX2gAwIBAgIBADAN"


prop_profileObject :: Property
prop_profileObject = property $ do
  v <- forAll genValue
  let val = PO.ProfileObject v
      bs = M.toStrictByteString (PO.renderProfileObject val)
  case runParser PO.profileObjectParser bs of
    OK val' "" -> val' === val
    other -> error (show other <> " on " <> show bs)


-- PICS-Label -----------------------------------------------------------------

picsExample :: ST.ShortText
picsExample = ST.fromString "(PICS-1.1 \"http://www.gcf.org/v2.5\" labels ratings (s 0))"


unit_picsLabel_parse :: Spec
unit_picsLabel_parse = it "parses PICS-Label labellist verbatim" $
  case runParser PL.picsLabelParser "(PICS-1.1 \"http://www.gcf.org/v2.5\" labels ratings (s 0))" of
    OK v "" -> PL.picsLabelValue v `shouldBe` picsExample
    other -> error (show other)


unit_picsLabel_render :: Spec
unit_picsLabel_render =
  it "renders PICS-Label labellist verbatim" $
    M.toStrictByteString (PL.renderPICSLabel (PL.PICSLabel picsExample))
      `shouldBe` "(PICS-1.1 \"http://www.gcf.org/v2.5\" labels ratings (s 0))"


prop_picsLabel :: Property
prop_picsLabel = property $ do
  v <- forAll genValue
  let val = PL.PICSLabel v
      bs = M.toStrictByteString (PL.renderPICSLabel val)
  case runParser PL.picsLabelParser bs of
    OK val' "" -> val' === val
    other -> error (show other <> " on " <> show bs)


-- P3P ------------------------------------------------------------------------

p3pExample :: ST.ShortText
p3pExample = ST.fromString "policyref=\"/w3c/p3p.xml\", CP=\"NON DSP COR\""


unit_p3p_parse :: Spec
unit_p3p_parse = it "parses P3P value verbatim" $
  case runParser P3.p3pParser "policyref=\"/w3c/p3p.xml\", CP=\"NON DSP COR\"" of
    OK v "" -> P3.p3pValue v `shouldBe` p3pExample
    other -> error (show other)


unit_p3p_render :: Spec
unit_p3p_render =
  it "renders P3P value verbatim" $
    M.toStrictByteString (P3.renderP3P (P3.P3P p3pExample))
      `shouldBe` "policyref=\"/w3c/p3p.xml\", CP=\"NON DSP COR\""


prop_p3p :: Property
prop_p3p = property $ do
  v <- forAll genValue
  let val = P3.P3P v
      bs = M.toStrictByteString (P3.renderP3P val)
  case runParser P3.p3pParser bs of
    OK val' "" -> val' === val
    other -> error (show other <> " on " <> show bs)


tests :: Spec
tests =
  describe "ObsoleteProfile" $
    sequence_
      [ unit_getProfile_parse
      , unit_getProfile_render
      , it "GetProfile round-trip" prop_getProfile
      , unit_setProfile_parse
      , unit_setProfile_render
      , it "SetProfile round-trip" prop_setProfile
      , unit_profileObject_parse
      , unit_profileObject_render
      , it "ProfileObject round-trip" prop_profileObject
      , unit_picsLabel_parse
      , unit_picsLabel_render
      , it "PICS-Label round-trip" prop_picsLabel
      , unit_p3p_parse
      , unit_p3p_render
      , it "P3P round-trip" prop_p3p
      ]
