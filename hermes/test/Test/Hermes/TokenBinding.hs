{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.TokenBinding (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
import Network.HTTP.Headers.HeaderFieldName (hIncludeReferredTokenBindingID, hOSCORE, hSecTokenBinding)
import qualified Network.HTTP.Headers.IncludeReferredTokenBindingID as IR
import qualified Network.HTTP.Headers.Mason as M
import qualified Network.HTTP.Headers.OSCORE as OSC
import Network.HTTP.Headers.Parsing.Util (parseFieldLine)
import qualified Network.HTTP.Headers.SecTokenBinding as STB
import Test.Syd
import Test.Syd.Hedgehog ()


bindingWire :: ByteString
bindingWire =
  "AIkAAgBBQFzK4_bhAqLDwRQxqJWte33d7hZ0hZWHwk-miKPg4E"
    <> "9fcgs7gBPoz-9RfuDfN9WCw6keHEw1ZPQMGs9CxpuHm-YAQM_j"
    <> "aOwwej6a-cQBGU7CJpUHOvXG4VvjNq8jDsvta9Y8_bPEPj25Gg"
    <> "mKiPjhJEtZA6mJ_9SNifLvVBTi7fR9wSAAAA"


p256Binding :: STB.TokenBinding
p256Binding =
  STB.TokenBinding
    STB.ProvidedTokenBinding
    STB.ECDSAP256
    (BS.cons 64 (BS.replicate 64 0))
    (BS.replicate 64 0)
    []


propBindingRoundTrip :: Property
propBindingRoundTrip = property $ do
  key <- BS.pack <$> forAll (Gen.list (Range.singleton 64) (Gen.word8 Range.constantBounded))
  signature <- BS.pack <$> forAll (Gen.list (Range.singleton 64) (Gen.word8 Range.constantBounded))
  extension <- BS.pack <$> forAll (Gen.list (Range.linear 0 64) (Gen.word8 Range.constantBounded))
  let entries =
        p256Binding
          { STB.bindingPublicKey = BS.cons 64 key
          , STB.bindingSignature = signature
          , STB.bindingExtensions = [STB.TokenBindingExtension 52 extension]
          }
          :| []
  case STB.secTokenBindingFromEntries entries of
    Left err -> error err
    Right header -> do
      let fields = setHeader header (headerMapFromList [])
      (lookupHeader @STB.SecTokenBinding fields >>= traverse STB.secTokenBindingEntries) === Right (Just entries)


propOSCORERoundTrip :: Property
propOSCORERoundTrip = property $ do
  bytes <- BS.pack <$> forAll (Gen.list (Range.linear 0 80) (Gen.word8 Range.constantBounded))
  let value = OSC.oscoreFromOptionBytes bytes
      expected = if bytes == BS.singleton 0 then BS.empty else bytes
      fields = setHeader value (headerMapFromList [])
  (lookupHeader @OSC.OSCORE fields >>= traverse OSC.oscoreOptionBytes) === Right (Just expected)


tests :: Spec
tests = describe "TokenBinding" $ do
  describe "Include-Referred-Token-Binding-ID" $ do
    let name = "Include-Referred-Token-Binding-ID"
        check wire = lookupHeader @IR.IncludeReferredTokenBindingID (headerMapFromList [(name, wire)])
    it "reads the case-insensitive true response signal and renders lowercase" $ do
      check " \tTrUE\t " `shouldBe` Right (Just (IR.IncludeReferredTokenBindingID True))
      parseFieldLine "flag" IR.includeReferredTokenBindingIDParser "TRUE" `shouldBe` Right (IR.IncludeReferredTokenBindingID True)
      lookupRawHeader hIncludeReferredTokenBindingID (setHeader (IR.IncludeReferredTokenBindingID True) (headerMapFromList []))
        `shouldBe` Just ("true" :| [])
    it "rejects false, suffixes and duplicate lines" $ do
      forM_ ["false", "", "true, true", "truE!", "t r u e", "true\r\n", "\255rue"] $ \wire ->
        check wire `shouldSatisfy` isLeft
      lookupHeader @IR.IncludeReferredTokenBindingID (headerMapFromList [(name, "true"), (name, "true")])
        `shouldSatisfy` isLeft
    it "omits a disabled signal" $ do
      M.toStrictByteString (IR.renderIncludeReferredTokenBindingID (IR.IncludeReferredTokenBindingID False)) `shouldBe` ""
      lookupRawHeader hIncludeReferredTokenBindingID (setHeader (IR.IncludeReferredTokenBindingID False) (headerMapFromList [(name, "true")]))
        `shouldBe` Nothing
  describe "Sec-Token-Binding" $ do
    let name = "Sec-Token-Binding"
        check wire = lookupHeader @STB.SecTokenBinding (headerMapFromList [(name, wire)])
    it "reads the RFC 8473 message and retains the exact encoded bytes" $ do
      case check (" \t" <> bindingWire <> "\t ") of
        Right (Just value) -> do
          STB.secTokenBindingMessage value `shouldBe` maybe (error "invalid fixture ASCII") id (ST.fromByteString bindingWire)
          M.toStrictByteString (STB.renderSecTokenBinding value) `shouldBe` bindingWire
          lookupRawHeader hSecTokenBinding (setHeader value (headerMapFromList [])) `shouldBe` Just (bindingWire :| [])
          parseFieldLine "Sec-Token-Binding" STB.secTokenBindingParser bindingWire `shouldBe` Right value
          fmap BS.length (STB.secTokenBindingBytes value) `shouldBe` Right 139
          case STB.secTokenBindingEntries value of
            Right (entry :| []) -> do
              STB.bindingType entry `shouldBe` STB.ProvidedTokenBinding
              STB.bindingKeyParameters entry `shouldBe` STB.ECDSAP256
              BS.take 5 (STB.bindingPublicKey entry) `shouldBe` BS.pack [64, 92, 202, 227, 246]
              BS.length (STB.bindingSignature entry) `shouldBe` 64
              STB.bindingExtensions entry `shouldBe` []
              STB.secTokenBindingFromEntries (entry :| []) `shouldBe` Right value
            other -> expectationFailure (show other)
        other -> expectationFailure (show other)
    it "enforces canonical unpadded base64url and single-line framing" $ do
      forM_ ["", "AA", bindingWire <> "=", bindingWire <> "AA", BS.init bindingWire <> "B", BS.drop 1 bindingWire, "!" <> BS.drop 1 bindingWire] $ \wire ->
        check wire `shouldSatisfy` isLeft
      lookupHeader @STB.SecTokenBinding (headerMapFromList [(name, bindingWire), (name, bindingWire)])
        `shouldSatisfy` isLeft
    it "checks vector boundaries, key sizes and required bindings" $ do
      let provided = p256Binding
          referred = provided {STB.bindingType = STB.ReferredTokenBinding}
          unknown =
            provided
              { STB.bindingType = STB.OtherTokenBindingType 8
              , STB.bindingKeyParameters = STB.OtherTokenBindingKeyParameters 42
              , STB.bindingExtensions = [STB.TokenBindingExtension 187 "\0\255"]
              }
          rsa =
            provided
              { STB.bindingKeyParameters = STB.RSA2048PSS
              , STB.bindingPublicKey = "\1\0\128" <> BS.replicate 255 0 <> "\3\1\0\1"
              , STB.bindingSignature = BS.replicate 256 17
              }
          entries = unknown :| [rsa, referred]
      (STB.secTokenBindingFromEntries entries >>= STB.secTokenBindingEntries) `shouldBe` Right entries
      forM_
        [ referred :| []
        , provided :| [provided]
        , provided :| [referred, referred]
        , provided :| [provided {STB.bindingType = STB.OtherTokenBindingType 1}]
        , (provided {STB.bindingKeyParameters = STB.OtherTokenBindingKeyParameters 2}) :| []
        , (provided {STB.bindingPublicKey = "not an EC point"}) :| []
        , (provided {STB.bindingSignature = BS.replicate 63 0}) :| []
        , (rsa {STB.bindingPublicKey = BS.init (STB.bindingPublicKey rsa)}) :| []
        , (provided {STB.bindingExtensions = [STB.TokenBindingExtension 3 (BS.replicate 65535 0)]}) :| []
        ]
        $ \invalid -> STB.secTokenBindingFromEntries invalid `shouldSatisfy` isLeft
      case STB.secTokenBindingFromEntries (provided :| []) >>= STB.secTokenBindingBytes of
        Right bytes -> do
          forM_ [BS.init bytes, bytes <> "\0", "\0\132" <> BS.drop 2 bytes, BS.take 6 bytes <> "\65" <> BS.drop 7 bytes] $ \invalid ->
            STB.secTokenBindingFromBytes invalid `shouldSatisfy` isLeft
        Left err -> expectationFailure err
    it "round-trips framed entries and opaque extensions through HeaderMap" propBindingRoundTrip
  describe "OSCORE" $ do
    let name = "OSCORE"
        check wire = lookupHeader @OSC.OSCORE (headerMapFromList [(name, wire)]) >>= traverse OSC.oscoreOptionBytes
    it "maps RFC 8613 options, including the special one-octet empty mapping" $ do
      check " \tCSU \t" `shouldBe` Right (Just "\9\37")
      check "AA" `shouldBe` Right (Just "")
      check "AAA" `shouldBe` Right (Just "\0\0")
      check "-_8" `shouldBe` Right (Just "\251\255")
      parseFieldLine "OSCORE" OSC.oscoreParser "CQUl" `shouldBe` Right (OSC.OSCORE "CQUl")
      M.toStrictByteString (OSC.renderOSCORE (OSC.OSCORE "CQUl")) `shouldBe` "CQUl"
      lookupRawHeader hOSCORE (setHeader (OSC.oscoreFromOptionBytes "") (headerMapFromList [])) `shouldBe` Just ("AA" :| [])
    it "rejects wrong alphabets, padding, stray bits, whitespace and duplicate lines" $ do
      forM_ ["", "A", "AAAAA", "AB", "AAF", "+/8", "-_8=", "A\tA", "AA, AA", "AA\r\n"] $ \wire ->
        check wire `shouldSatisfy` isLeft
      lookupHeader @OSC.OSCORE (headerMapFromList [(name, "AA"), (name, "CSU")]) `shouldSatisfy` isLeft
    it "round-trips arbitrary CoAP option octets through HeaderMap" propOSCORERoundTrip
