{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.TokenBinding (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.IncludeReferredTokenBindingID as IR
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.OSCORE as OSC
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.SecTokenBinding as STB
import Test.Syd
import Test.Syd.Hedgehog ()


parseIR :: ByteString -> Either String IR.IncludeReferredTokenBindingID
parseIR bs = case runParser IR.includeReferredTokenBindingIDParser bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseSTB :: ByteString -> Either String STB.SecTokenBinding
parseSTB bs = case runParser STB.secTokenBindingParser bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


parseOSC :: ByteString -> Either String OSC.OSCORE
parseOSC bs = case runParser OSC.oscoreParser bs of
  OK v leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


renderIR :: IR.IncludeReferredTokenBindingID -> ByteString
renderIR = M.toStrictByteString . IR.renderIncludeReferredTokenBindingID


renderSTB :: STB.SecTokenBinding -> ByteString
renderSTB = M.toStrictByteString . STB.renderSecTokenBinding


renderOSC :: OSC.OSCORE -> ByteString
renderOSC = M.toStrictByteString . OSC.renderOSCORE


-- Include-Referred-Token-Binding-ID --------------------------------------

unit_include_parse :: Spec
unit_include_parse = it "parses the literal true" $
  case parseIR "true" of
    Right (IR.IncludeReferredTokenBindingID True) -> pure () :: IO ()
    other -> error (show other)


unit_include_render :: Spec
unit_include_render =
  it "renders the boolean flag" $ do
    renderIR (IR.IncludeReferredTokenBindingID True) `shouldBe` "true"
    renderIR (IR.IncludeReferredTokenBindingID False) `shouldBe` "false"


prop_include_roundtrip :: Property
prop_include_roundtrip = property $ do
  b <- forAll Gen.bool
  let v = IR.IncludeReferredTokenBindingID b
      bs = renderIR v
  case parseIR bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Sec-Token-Binding ------------------------------------------------------

base64urlChar :: Gen Char
base64urlChar =
  Gen.element (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ['-', '_'])


unit_secbinding_parse :: Spec
unit_secbinding_parse = it "preserves the base64url TokenBindingMessage" $
  case parseSTB "AIkAAgBBQA-cf2v3p7BzG" of
    Right v -> STB.secTokenBindingMessage v `shouldBe` ST.fromString "AIkAAgBBQA-cf2v3p7BzG"
    other -> error (show other)


unit_secbinding_render :: Spec
unit_secbinding_render =
  it "renders the raw value verbatim" $
    renderSTB (STB.SecTokenBinding (ST.fromString "AIkAAgBBQA-cf2v3p7BzG"))
      `shouldBe` "AIkAAgBBQA-cf2v3p7BzG"


prop_secbinding_roundtrip :: Property
prop_secbinding_roundtrip = property $ do
  s <- forAll (Gen.string (Range.linear 1 64) base64urlChar)
  let v = STB.SecTokenBinding (ST.fromString s)
      bs = renderSTB v
  case parseSTB bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- OSCORE -----------------------------------------------------------------

base64Char :: Gen Char
base64Char =
  Gen.element (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ['+', '/'])


unit_oscore_parse :: Spec
unit_oscore_parse = it "preserves the opaque OSCORE option value" $
  case parseOSC "CQUAaGVsbG8=" of
    Right v -> OSC.oscoreValue v `shouldBe` ST.fromString "CQUAaGVsbG8="
    other -> error (show other)


unit_oscore_render :: Spec
unit_oscore_render =
  it "renders the raw value verbatim" $
    renderOSC (OSC.OSCORE (ST.fromString "CQUAaGVsbG8="))
      `shouldBe` "CQUAaGVsbG8="


prop_oscore_roundtrip :: Property
prop_oscore_roundtrip = property $ do
  s <- forAll (Gen.string (Range.linear 1 64) base64Char)
  let v = OSC.OSCORE (ST.fromString s)
      bs = renderOSC v
  case parseOSC bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "TokenBinding" $
    sequence_
      [ unit_include_parse
      , unit_include_render
      , it "Include-Referred-Token-Binding-ID round-trips" prop_include_roundtrip
      , unit_secbinding_parse
      , unit_secbinding_render
      , it "Sec-Token-Binding round-trips" prop_secbinding_roundtrip
      , unit_oscore_parse
      , unit_oscore_render
      , it "OSCORE round-trips" prop_oscore_roundtrip
      ]
