{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ClientCert (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.CertNotAfter as CNA
import qualified Network.HTTP.Headers.CertNotBefore as CNB
import qualified Network.HTTP.Headers.ClientCert as CC
import qualified Network.HTTP.Headers.ClientCertChain as CCC
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


parseFull :: ParserT () String a -> ByteString -> Either String a
parseFull p bs = case runParser p bs of
  OK v rest
    | BS.null rest -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


renderCC :: CC.ClientCert -> ByteString
renderCC = M.toStrictByteString . CC.renderClientCert


renderCCC :: CCC.ClientCertChain -> ByteString
renderCCC = M.toStrictByteString . CCC.renderClientCertChain


renderCNB :: CNB.CertNotBefore -> ByteString
renderCNB = M.toStrictByteString . CNB.renderCertNotBefore


renderCNA :: CNA.CertNotAfter -> ByteString
renderCNA = M.toStrictByteString . CNA.renderCertNotAfter


bytesGen :: Gen ByteString
bytesGen = BS.pack <$> Gen.list (Range.linear 0 48) (Gen.word8 Range.constantBounded)


-- Client-Cert -----------------------------------------------------------------

unit_clientcert_parse :: Spec
unit_clientcert_parse = it "parses a Client-Cert byte sequence" $
  case parseFull CC.clientCertParser ":aGVsbG8=:" of
    Right (CC.ClientCert bs) -> bs `shouldBe` "hello"
    other -> error (show other)


unit_clientcert_render :: Spec
unit_clientcert_render =
  it "renders a Client-Cert byte sequence" $
    renderCC (CC.ClientCert "hello") `shouldBe` ":aGVsbG8=:"


prop_clientcert_roundtrip :: Property
prop_clientcert_roundtrip = property $ do
  bytes <- forAll bytesGen
  let v = CC.ClientCert bytes
  case parseFull CC.clientCertParser (renderCC v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show (renderCC v))


-- Client-Cert-Chain -----------------------------------------------------------

unit_chain_parse :: Spec
unit_chain_parse = it "parses a Client-Cert-Chain list" $
  case parseFull CCC.clientCertChainParser ":aGVsbG8=:, :d29ybGQ=:" of
    Right (CCC.ClientCertChain certs) -> NE.toList certs `shouldBe` ["hello", "world"]
    other -> error (show other)


unit_chain_render :: Spec
unit_chain_render =
  it "renders a Client-Cert-Chain list" $
    renderCCC (CCC.ClientCertChain ("hello" :| ["world"])) `shouldBe` ":aGVsbG8=:, :d29ybGQ=:"


prop_chain_roundtrip :: Property
prop_chain_roundtrip = property $ do
  certs <- forAll (Gen.nonEmpty (Range.linear 1 5) bytesGen)
  let v = CCC.ClientCertChain certs
  case parseFull CCC.clientCertChainParser (renderCCC v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show (renderCCC v))


-- Cert-Not-Before -------------------------------------------------------------

unit_notbefore_parse :: Spec
unit_notbefore_parse = it "parses a Cert-Not-Before sf-date" $
  case parseFull CNB.certNotBeforeParser "@1659578233" of
    Right (CNB.CertNotBefore n) -> n `shouldBe` 1659578233
    other -> error (show other)


unit_notbefore_render :: Spec
unit_notbefore_render =
  it "renders a Cert-Not-Before sf-date" $
    renderCNB (CNB.CertNotBefore 1659578233) `shouldBe` "@1659578233"


prop_notbefore_roundtrip :: Property
prop_notbefore_roundtrip = property $ do
  secs <- forAll (Gen.int (Range.linearFrom 0 (-1_000_000_000) 4_000_000_000))
  let v = CNB.CertNotBefore secs
  case parseFull CNB.certNotBeforeParser (renderCNB v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show (renderCNB v))


-- Cert-Not-After --------------------------------------------------------------

unit_notafter_parse :: Spec
unit_notafter_parse = it "parses a Cert-Not-After sf-date" $
  case parseFull CNA.certNotAfterParser "@1690354800" of
    Right (CNA.CertNotAfter n) -> n `shouldBe` 1690354800
    other -> error (show other)


unit_notafter_render :: Spec
unit_notafter_render =
  it "renders a Cert-Not-After sf-date" $
    renderCNA (CNA.CertNotAfter 1690354800) `shouldBe` "@1690354800"


prop_notafter_roundtrip :: Property
prop_notafter_roundtrip = property $ do
  secs <- forAll (Gen.int (Range.linearFrom 0 (-1_000_000_000) 4_000_000_000))
  let v = CNA.CertNotAfter secs
  case parseFull CNA.certNotAfterParser (renderCNA v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show (renderCNA v))


tests :: Spec
tests =
  describe "ClientCert" $
    sequence_
      [ unit_clientcert_parse
      , unit_clientcert_render
      , it "Client-Cert round-trip" prop_clientcert_roundtrip
      , unit_chain_parse
      , unit_chain_render
      , it "Client-Cert-Chain round-trip" prop_chain_roundtrip
      , unit_notbefore_parse
      , unit_notbefore_render
      , it "Cert-Not-Before round-trip" prop_notbefore_roundtrip
      , unit_notafter_parse
      , unit_notafter_render
      , it "Cert-Not-After round-trip" prop_notafter_roundtrip
      ]
