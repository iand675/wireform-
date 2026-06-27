{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Alpn (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.ALPN as A
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.TLS.Extensions as E
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ByteString -> Either String A.ALPN
parseOk bs = case runParser A.alpnParser bs of
  OK v leftover
    | BS.null leftover -> Right v
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: A.ALPN -> ByteString
render = M.toStrictByteString . A.renderALPN


-- Parse the canonical RFC 7639 example, percent-decoding @http%2F1.1@ back to
-- the @http/1.1@ identification sequence.
unit_parse :: Spec
unit_parse = it "parses the RFC 7639 example (h2, http%2F1.1)" $
  case parseOk "h2, http%2F1.1" of
    Right (A.ALPN (a :| [b])) -> do
      a `shouldBe` E.alpnHttp2
      b `shouldBe` E.alpnHttp11
    other -> error (show other)


-- Octets that are not token characters (here @/@) must be percent-encoded with
-- uppercase hex; plain token octets pass through verbatim.
unit_render :: Spec
unit_render =
  it "renders protocol IDs percent-encoded per RFC 7639" $
    render (A.ALPN (E.alpnHttp2 :| [E.alpnHttp11]))
      `shouldBe` "h2, http%2F1.1"


-- Property: any non-empty list of arbitrary octet sequences renders to a header
-- value that parses back to the original protocol IDs.
genProtocol :: Gen E.ALPNProtocol
genProtocol =
  E.mkALPNProtocol . BS.pack
    <$> Gen.list (Range.linear 1 8) (Gen.word8 Range.constantBounded)


genAlpn :: Gen A.ALPN
genAlpn = A.ALPN <$> Gen.nonEmpty (Range.linear 1 4) genProtocol


prop_roundtrip :: Property
prop_roundtrip = property $ do
  v <- forAll genAlpn
  let bs = render v
  case parseOk bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "Alpn" $
    sequence_
      [ unit_parse
      , unit_render
      , it "round-trips arbitrary protocol-id octet sequences" prop_roundtrip
      ]
