{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.ClientHints (tests) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.AcceptCH as ACH
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (ParserT, RFC8941Token (..), Result (..), runParser)
import qualified Network.HTTP.Headers.SecGPC as GPC
import qualified Network.HTTP.Headers.SecPurpose as SP
import Test.Syd
import Test.Syd.Hedgehog ()


-- Run a standalone header parser, requiring it to consume the whole input.
runP :: ParserT () String a -> ByteString -> Either String a
runP p bs = case runParser p bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err e -> Left e


tok :: String -> RFC8941Token
tok = RFC8941Token . ST.fromString


-- A valid RFC 8941 token: ALPHA leader followed by token characters.
tokenGen :: Gen RFC8941Token
tokenGen = do
  c <- Gen.element (['a' .. 'z'] ++ ['A' .. 'Z'])
  rest <- Gen.list (Range.linear 0 8) (Gen.element tokenChars)
  pure (RFC8941Token (ST.fromString (c : rest)))
  where
    tokenChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_."


renderACH :: ACH.AcceptCH -> ByteString
renderACH = M.toStrictByteString . ACH.renderAcceptCH


renderSP :: SP.SecPurpose -> ByteString
renderSP = M.toStrictByteString . SP.renderSecPurpose


renderGPC :: GPC.SecGPC -> ByteString
renderGPC = M.toStrictByteString . GPC.renderSecGPC


-- Accept-CH ---------------------------------------------------------------

unit_acceptCH_parse :: Spec
unit_acceptCH_parse = it "parses a client-hint token list" $
  case runP ACH.acceptCHParser "Sec-CH-UA, DPR, Width" of
    Right (ACH.AcceptCH hints) ->
      map unsafeToRFC8941Token hints
        `shouldBe` map ST.fromString ["Sec-CH-UA", "DPR", "Width"]
    other -> error (show other)


unit_acceptCH_render :: Spec
unit_acceptCH_render =
  it "renders a client-hint token list" $
    renderACH (ACH.AcceptCH [tok "DPR", tok "Width"]) `shouldBe` "DPR, Width"


prop_acceptCH_roundtrip :: Property
prop_acceptCH_roundtrip = property $ do
  hints <- forAll (Gen.list (Range.linear 1 6) tokenGen)
  let v = ACH.AcceptCH hints
  case runP ACH.acceptCHParser (renderACH v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show (renderACH v))


-- Sec-Purpose -------------------------------------------------------------

unit_secPurpose_parse :: Spec
unit_secPurpose_parse = it "parses the prefetch purpose" $
  case runP SP.secPurposeParser "prefetch" of
    Right (SP.SecPurpose t) -> unsafeToRFC8941Token t `shouldBe` ST.fromString "prefetch"
    other -> error (show other)


unit_secPurpose_render :: Spec
unit_secPurpose_render =
  it "renders the prefetch purpose" $
    renderSP (SP.SecPurpose (tok "prefetch")) `shouldBe` "prefetch"


prop_secPurpose_roundtrip :: Property
prop_secPurpose_roundtrip = property $ do
  t <- forAll tokenGen
  let v = SP.SecPurpose t
  case runP SP.secPurposeParser (renderSP v) of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show (renderSP v))


-- Sec-GPC -----------------------------------------------------------------

unit_secGPC_parse :: Spec
unit_secGPC_parse = it "parses the GPC signal" $
  case runP GPC.secGPCParser "1" of
    Right GPC.SecGPC -> pure () :: IO ()
    other -> error (show other)


unit_secGPC_render :: Spec
unit_secGPC_render =
  it "renders the GPC signal" $
    renderGPC GPC.SecGPC `shouldBe` "1"


prop_secGPC_roundtrip :: Property
prop_secGPC_roundtrip = property $ do
  v <- forAll (Gen.constant GPC.SecGPC)
  case runP GPC.secGPCParser (renderGPC v) of
    Right v' -> v === v'
    Left err -> error err


tests :: Spec
tests =
  describe "ClientHints" $
    sequence_
      [ unit_acceptCH_parse
      , unit_acceptCH_render
      , it "Accept-CH round-trips" prop_acceptCH_roundtrip
      , unit_secPurpose_parse
      , unit_secPurpose_render
      , it "Sec-Purpose round-trips" prop_secPurpose_roundtrip
      , unit_secGPC_parse
      , unit_secGPC_render
      , it "Sec-GPC round-trips" prop_secGPC_roundtrip
      ]
