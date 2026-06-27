{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Capsule (tests) where

import Data.ByteString (ByteString)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Network.HTTP.Headers.CapsuleProtocol as CP
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Syd
import Test.Syd.Hedgehog ()


parseOk :: ByteString -> Either String CP.CapsuleProtocol
parseOk bs = case runParser CP.capsuleProtocolParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


render :: CP.CapsuleProtocol -> ByteString
render = M.toStrictByteString . CP.renderCapsuleProtocol


unit_parse_true :: Spec
unit_parse_true = it "parses ?1 as enabled" $
  case parseOk "?1" of
    Right (CP.CapsuleProtocol True) -> pure () :: IO ()
    other -> error (show other)


unit_parse_false :: Spec
unit_parse_false = it "parses ?0 as disabled" $
  case parseOk "?0" of
    Right (CP.CapsuleProtocol False) -> pure () :: IO ()
    other -> error (show other)


unit_render :: Spec
unit_render = it "renders sf-boolean" $ do
  render (CP.CapsuleProtocol True) `shouldBe` "?1"
  render (CP.CapsuleProtocol False) `shouldBe` "?0"


genCapsuleProtocol :: Gen CP.CapsuleProtocol
genCapsuleProtocol = CP.CapsuleProtocol <$> Gen.bool


prop_roundtrip :: Property
prop_roundtrip = property $ do
  v <- forAll genCapsuleProtocol
  let bs = render v
  case parseOk bs of
    Right v' -> v' === v
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "CapsuleProtocol" $
    sequence_
      [ unit_parse_true
      , unit_parse_false
      , unit_render
      , it "round-trips" prop_roundtrip
      ]
