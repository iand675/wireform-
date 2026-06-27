{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the obsolete HTTP Extension Framework (RFC 2774) and PEP
header family: @Ext@, @Man@, @Opt@, @PEP@ and @PEP-Info@. Each is a
faithful raw-preserving newtype, so the central invariant is that an
arbitrary (valid) field value survives a render/parse round-trip
byte-for-byte.
-}
module Test.Hermes.ObsoleteMan (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Ext as Ext
import qualified Network.HTTP.Headers.Man as Man
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.Opt as Opt
import qualified Network.HTTP.Headers.PEP as PEP
import qualified Network.HTTP.Headers.PEPInfo as PEPInfo
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Unwrap a fully-consumed parse, failing loudly otherwise.
expectOk :: (Show a) => Result String a -> a
expectOk r = case r of
  OK v "" -> v
  OK _ rest -> error ("unconsumed: " <> show rest)
  Fail -> error "parse failed"
  Err err -> error err


{- | A known-good ASCII value must parse to the constructed newtype and
render back to the exact same bytes.
-}
checkValue
  :: (Eq a, Show a)
  => (ST.ShortText -> a)
  -> (a -> M.Builder)
  -> (ByteString -> Result String a)
  -> String
  -> IO ()
checkValue mk renderA parse raw = do
  let value = mk (ST.fromString raw)
      bytes = BSC.pack raw
  expectOk (parse bytes) `shouldBe` value
  M.toStrictByteString (renderA value) `shouldBe` bytes


unit_ext :: Spec
unit_ext = it "Ext acknowledgement value (normally empty) round-trips verbatim" $ do
  checkValue Ext.Ext Ext.renderExt (runParser Ext.extParser) ""
  checkValue Ext.Ext Ext.renderExt (runParser Ext.extParser) "12, 15"


unit_man :: Spec
unit_man =
  it "Man mandatory extension declaration round-trips verbatim" $
    checkValue
      Man.Man
      Man.renderMan
      (runParser Man.manParser)
      "\"http://www.copyright.org/rights-management\"; ns=16"


unit_opt :: Spec
unit_opt =
  it "Opt optional extension declaration round-trips verbatim" $
    checkValue
      Opt.Opt
      Opt.renderOpt
      (runParser Opt.optParser)
      "\"http://www.digest.org/Digest\"; ns=15"


unit_pep :: Spec
unit_pep =
  it "PEP protocol declaration round-trips verbatim" $
    checkValue
      PEP.PEP
      PEP.renderPEP
      (runParser PEP.pepParser)
      "{{map \"/Pep/Inventory\"}{strength must}}"


unit_pepInfo :: Spec
unit_pepInfo =
  it "PEP-Info protocol information round-trips verbatim" $
    checkValue
      PEPInfo.PEPInfo
      PEPInfo.renderPEPInfo
      (runParser PEPInfo.pepInfoParser)
      "{{map \"/Pep/Inventory\"}}"


-- A character set covering the punctuation that shows up in ext-decl /
-- PEP protocol values, sufficient to exercise verbatim preservation
-- without introducing leading/trailing whitespace.
valueChar :: Gen Char
valueChar =
  Gen.choice
    [ Gen.alphaNum
    , Gen.element ['"', ';', '=', '/', ':', '.', '-', '_', '~', '{', '}', '@']
    ]


rawValueGen :: Gen ST.ShortText
rawValueGen = ST.fromString <$> Gen.string (Range.linear 0 40) valueChar


-- | Any generated value renders to bytes that parse back to itself.
roundTripProp
  :: (Eq a, Show a)
  => (ST.ShortText -> a)
  -> (a -> M.Builder)
  -> (ByteString -> Result String a)
  -> Property
roundTripProp mk renderA parse = property $ do
  raw <- forAll rawValueGen
  let value = mk raw
      bytes = M.toStrictByteString (renderA value)
  case parse bytes of
    OK value' "" -> value === value'
    OK _ rest -> error ("unconsumed " <> show rest <> " on " <> show bytes)
    Fail -> error ("parse failed on " <> show bytes)
    Err err -> error (err <> " on " <> show bytes)


prop_ext :: Property
prop_ext = roundTripProp Ext.Ext Ext.renderExt (runParser Ext.extParser)


prop_man :: Property
prop_man = roundTripProp Man.Man Man.renderMan (runParser Man.manParser)


prop_opt :: Property
prop_opt = roundTripProp Opt.Opt Opt.renderOpt (runParser Opt.optParser)


prop_pep :: Property
prop_pep = roundTripProp PEP.PEP PEP.renderPEP (runParser PEP.pepParser)


prop_pepInfo :: Property
prop_pepInfo = roundTripProp PEPInfo.PEPInfo PEPInfo.renderPEPInfo (runParser PEPInfo.pepInfoParser)


tests :: Spec
tests =
  describe "ObsoleteMan" $
    sequence_
      [ unit_ext
      , unit_man
      , unit_opt
      , unit_pep
      , unit_pepInfo
      , it "Ext round-trip" prop_ext
      , it "Man round-trip" prop_man
      , it "Opt round-trip" prop_opt
      , it "PEP round-trip" prop_pep
      , it "PEP-Info round-trip" prop_pepInfo
      ]
