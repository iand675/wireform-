{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ClientCert (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Time (UTCTime (..), fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Headers (headerMapFromList, headerMapToList, lookupHeader, setHeader)
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


prop_notbefore_roundtrip :: Property
prop_notbefore_roundtrip = property $ do
  secs <- forAll (Gen.int (Range.linearFrom 0 (-1_000_000_000) 4_000_000_000))
  let v = CNB.CertNotBefore (posixSecondsToUTCTime (fromIntegral secs))
  case parseFull CNB.certNotBeforeParser (renderCNB v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show (renderCNB v))


prop_notafter_roundtrip :: Property
prop_notafter_roundtrip = property $ do
  secs <- forAll (Gen.int (Range.linearFrom 0 (-1_000_000_000) 4_000_000_000))
  let v = CNA.CertNotAfter (posixSecondsToUTCTime (fromIntegral secs))
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
      , it "requires one end-entity field and preserves the chain order across lines" $ do
          let fields =
                headerMapFromList
                  [ ("Client-Cert", "\t:aGVsbG8:\t")
                  , ("Client-Cert-Chain", " :d29ybGQ=: ")
                  , ("Client-Cert-Chain", "\t:YQ==:, :Yg==:")
                  ]
          CC.lookupClientCertificateHeaders fields
            `shouldBe` Right (Just (CC.ClientCertificateHeaders (CC.ClientCert "hello") (Just (CCC.ClientCertChain ("world" :| ["a", "b"])))))
          lookupHeader @CC.ClientCert (headerMapFromList [("Client-Cert", ":YQ==:"), ("Client-Cert", ":Yg==:")]) `shouldSatisfy` isLeft
          lookupHeader @CC.ClientCert (headerMapFromList [("Client-Cert", ":YQ==:, :Yg==:")]) `shouldSatisfy` isLeft
          lookupHeader @CCC.ClientCertChain (headerMapFromList [("Client-Cert-Chain", ":YQ==:"), ("Client-Cert-Chain", "(:Yg==:)")])
            `shouldSatisfy` isLeft
      , it "rejects a chain with no leaf or with the leaf in the chain" $ do
          CC.lookupClientCertificateHeaders (headerMapFromList [("Client-Cert-Chain", ":YQ==:")]) `shouldSatisfy` isLeft
          CC.lookupClientCertificateHeaders (headerMapFromList [("Client-Cert", ":YQ:"), ("Client-Cert-Chain", ":Yg==:, :YQ==:")])
            `shouldSatisfy` isLeft
          CC.lookupClientCertificateHeaders (headerMapFromList [("Client-Cert", ":YQ:")])
            `shouldBe` Right (Just (CC.ClientCertificateHeaders (CC.ClientCert "a") Nothing))
      , it "looks up and renders the ACME STAR validity fields as HTTP dates" $ do
          let beforeWire = "Thu, 03 Oct 2019 00:00:00 GMT"
              afterWire = "Thu, 10 Oct 2019 00:00:00 GMT"
              starts = CNB.CertNotBefore (UTCTime (fromGregorian 2019 10 3) 0)
              expires = CNA.CertNotAfter (UTCTime (fromGregorian 2019 10 10) 0)
              fields = headerMapFromList [("Cert-Not-Before", beforeWire), ("Cert-Not-After", afterWire)]
          lookupHeader @CNB.CertNotBefore fields `shouldBe` Right (Just starts)
          lookupHeader @CNA.CertNotAfter fields `shouldBe` Right (Just expires)
          headerMapToList (setHeader starts (headerMapFromList [])) `shouldBe` [("cert-not-before", beforeWire)]
          headerMapToList (setHeader expires (headerMapFromList [])) `shouldBe` [("cert-not-after", afterWire)]
          lookupHeader @CNB.CertNotBefore (headerMapFromList [("Cert-Not-Before", beforeWire), ("Cert-Not-Before", beforeWire)])
            `shouldSatisfy` isLeft
          lookupHeader @CNA.CertNotAfter (headerMapFromList [("Cert-Not-After", "@1690354800")]) `shouldSatisfy` isLeft
      , it "Cert-Not-Before round-trip" prop_notbefore_roundtrip
      , it "Cert-Not-After round-trip" prop_notafter_roundtrip
      ]
