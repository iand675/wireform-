{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Signature (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.AcceptSignature as AS
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ItemValue (..), RFC8941String (..), Result (..), runParser)
import qualified Network.HTTP.Headers.Signature as Sig
import qualified Network.HTTP.Headers.SignatureInput as SI
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- parse / render helpers
-- ---------------------------------------------------------------------------

dropOws :: ByteString -> ByteString
dropOws = BS.dropWhile (\w -> w == 0x20 || w == 0x09)


parseSig :: ByteString -> Either String Sig.Signature
parseSig bs = case runParser Sig.signatureParser bs of
  OK v rest
    | BS.null (dropOws rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


parseSI :: ByteString -> Either String SI.SignatureInput
parseSI bs = case runParser SI.signatureInputParser bs of
  OK v rest
    | BS.null (dropOws rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


parseAS :: ByteString -> Either String AS.AcceptSignature
parseAS bs = case runParser AS.acceptSignatureParser bs of
  OK v rest
    | BS.null (dropOws rest) -> Right v
    | otherwise -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderSig :: Sig.Signature -> ByteString
renderSig = M.toStrictByteString . Sig.renderSignature


renderSI :: SI.SignatureInput -> ByteString
renderSI = M.toStrictByteString . SI.renderSignatureInput


renderAS :: AS.AcceptSignature -> ByteString
renderAS = M.toStrictByteString . AS.renderAcceptSignature


str :: String -> RFC8941String
str = RFC8941String . ST.fromString


-- ---------------------------------------------------------------------------
-- Signature unit tests
-- ---------------------------------------------------------------------------

unit_signature_parse :: Spec
unit_signature_parse = it "parses a Signature dictionary" $
  case parseSig "sig1=:dGVzdA==:, proof=:YWJj:" of
    Right (Sig.Signature ((l1, b1) :| [(l2, b2)])) -> do
      l1 `shouldBe` ST.fromString "sig1"
      b1 `shouldBe` ("test" :: ByteString)
      l2 `shouldBe` ST.fromString "proof"
      b2 `shouldBe` ("abc" :: ByteString)
    other -> error (show other)


unit_signature_render :: Spec
unit_signature_render =
  it "renders a Signature dictionary" $
    let v = Sig.Signature ((ST.fromString "sig1", "test") :| [(ST.fromString "proof", "abc")])
    in renderSig v `shouldBe` "sig1=:dGVzdA==:, proof=:YWJj:"


-- ---------------------------------------------------------------------------
-- Signature-Input unit tests
-- ---------------------------------------------------------------------------

unit_siginput_parse :: Spec
unit_siginput_parse = it "parses a Signature-Input dictionary with a component flag" $
  case parseSI "sig1=(\"@method\" \"content-digest\";sf);created=1618884475;keyid=\"test-key\"" of
    Right (SI.SignatureInput (e :| [])) -> do
      SI.signatureLabel e `shouldBe` ST.fromString "sig1"
      SI.signatureComponents e
        `shouldBe` [ SI.SignatureComponent (str "@method") []
                   , SI.SignatureComponent (str "content-digest") [(ST.fromString "sf", Nothing)]
                   ]
      SI.signatureParameters e
        `shouldBe` [ (ST.fromString "created", Just (Integer 1618884475))
                   , (ST.fromString "keyid", Just (String (str "test-key")))
                   ]
    other -> error (show other)


unit_siginput_render :: Spec
unit_siginput_render =
  it "renders a Signature-Input dictionary" $
    let v =
          SI.SignatureInput
            ( SI.SignatureInputEntry
                (ST.fromString "sig1")
                [ SI.SignatureComponent (str "@method") []
                , SI.SignatureComponent (str "@target-uri") []
                ]
                [ (ST.fromString "created", Just (Integer 1618884475))
                , (ST.fromString "keyid", Just (String (str "test-key")))
                ]
                :| []
            )
    in renderSI v
        `shouldBe` "sig1=(\"@method\" \"@target-uri\");created=1618884475;keyid=\"test-key\""


-- ---------------------------------------------------------------------------
-- Accept-Signature unit tests
-- ---------------------------------------------------------------------------

unit_acceptsig_parse :: Spec
unit_acceptsig_parse = it "parses an Accept-Signature dictionary" $
  case parseAS "acc=(\"@method\" \"@authority\");keyid=\"test-key\";alg=\"rsa-pss-sha512\"" of
    Right (AS.AcceptSignature (e :| [])) -> do
      AS.signatureLabel e `shouldBe` ST.fromString "acc"
      AS.signatureComponents e
        `shouldBe` [ AS.AcceptSignatureComponent (str "@method") []
                   , AS.AcceptSignatureComponent (str "@authority") []
                   ]
      AS.signatureParameters e
        `shouldBe` [ (ST.fromString "keyid", Just (String (str "test-key")))
                   , (ST.fromString "alg", Just (String (str "rsa-pss-sha512")))
                   ]
    other -> error (show other)


unit_acceptsig_render :: Spec
unit_acceptsig_render =
  it "renders an Accept-Signature dictionary" $
    let v =
          AS.AcceptSignature
            ( AS.AcceptSignatureEntry
                (ST.fromString "acc")
                [AS.AcceptSignatureComponent (str "@method") []]
                [(ST.fromString "keyid", Just (String (str "test-key")))]
                :| []
            )
    in renderAS v `shouldBe` "acc=(\"@method\");keyid=\"test-key\""


-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genLabel :: Gen ST.ShortText
genLabel = do
  c <- Gen.element ['a' .. 'z']
  rest <- Gen.list (Range.linear 0 6) (Gen.element (['a' .. 'z'] <> ['0' .. '9']))
  pure (ST.pack (c : rest))


genComponentName :: Gen RFC8941String
genComponentName = do
  prefix <- Gen.element ["", "@"]
  body <- Gen.list (Range.linear 1 12) (Gen.element (['a' .. 'z'] <> "-"))
  pure (RFC8941String (ST.pack (prefix <> body)))


genItemValue :: Gen ItemValue
genItemValue =
  Gen.choice
    [ Integer <$> Gen.int (Range.linear 0 1_000_000)
    , String . RFC8941String . ST.pack
        <$> Gen.list (Range.linear 0 10) (Gen.element (['a' .. 'z'] <> ['0' .. '9'] <> "-"))
    ]


genParam :: Gen (ST.ShortText, Maybe ItemValue)
genParam = do
  k <- genLabel
  mv <- Gen.maybe genItemValue
  pure (k, mv)


genParams :: Gen [(ST.ShortText, Maybe ItemValue)]
genParams = Gen.list (Range.linear 0 3) genParam


-- Signature

genSigValue :: Gen (ST.ShortText, ByteString)
genSigValue = do
  label <- genLabel
  bytes <- BS.pack <$> Gen.list (Range.linear 0 32) (Gen.word8 Range.constantBounded)
  pure (label, bytes)


genSignature :: Gen Sig.Signature
genSignature = Sig.Signature <$> Gen.nonEmpty (Range.linear 1 3) genSigValue


-- Signature-Input

genSIComponent :: Gen SI.SignatureComponent
genSIComponent = SI.SignatureComponent <$> genComponentName <*> Gen.list (Range.linear 0 2) genParam


genSIEntry :: Gen SI.SignatureInputEntry
genSIEntry =
  SI.SignatureInputEntry
    <$> genLabel
    <*> Gen.list (Range.linear 0 3) genSIComponent
    <*> genParams


genSignatureInput :: Gen SI.SignatureInput
genSignatureInput = SI.SignatureInput <$> Gen.nonEmpty (Range.linear 1 3) genSIEntry


-- Accept-Signature

genASComponent :: Gen AS.AcceptSignatureComponent
genASComponent = AS.AcceptSignatureComponent <$> genComponentName <*> Gen.list (Range.linear 0 2) genParam


genASEntry :: Gen AS.AcceptSignatureEntry
genASEntry =
  AS.AcceptSignatureEntry
    <$> genLabel
    <*> Gen.list (Range.linear 0 3) genASComponent
    <*> genParams


genAcceptSignature :: Gen AS.AcceptSignature
genAcceptSignature = AS.AcceptSignature <$> Gen.nonEmpty (Range.linear 1 3) genASEntry


-- ---------------------------------------------------------------------------
-- Round-trip properties
-- ---------------------------------------------------------------------------

prop_signature_roundtrip :: Property
prop_signature_roundtrip = property $ do
  v <- forAll genSignature
  let bs = renderSig v
  case parseSig bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


prop_siginput_roundtrip :: Property
prop_siginput_roundtrip = property $ do
  v <- forAll genSignatureInput
  let bs = renderSI v
  case parseSI bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


prop_acceptsig_roundtrip :: Property
prop_acceptsig_roundtrip = property $ do
  v <- forAll genAcceptSignature
  let bs = renderAS v
  case parseAS bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Signature" $
    sequence_
      [ unit_signature_parse
      , unit_signature_render
      , unit_siginput_parse
      , unit_siginput_render
      , unit_acceptsig_parse
      , unit_acceptsig_render
      , it "Signature round-trip" prop_signature_roundtrip
      , it "Signature-Input round-trip" prop_siginput_roundtrip
      , it "Accept-Signature round-trip" prop_acceptsig_roundtrip
      ]
