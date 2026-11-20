{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.Capsule (tests) where

import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Network.HTTP.Headers (headerMapFromList, lookupHeader, lookupRawHeader, setHeader)
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


unit_capsule_fields :: Spec
unit_capsule_fields = it "reads one Boolean Item and validates its ignored parameters" $ do
  let empty = headerMapFromList []
      field wire = headerMapFromList [("Capsule-Protocol", wire)]
      enabled = field " \t?1;flag;epoch=@42;label=\"tunnel\"\t "
      repeated = headerMapFromList [("Capsule-Protocol", "?1"), ("Capsule-Protocol", "?1")]
  lookupHeader @CP.CapsuleProtocol enabled `shouldBe` Right (Just (CP.CapsuleProtocol True))
  lookupHeader @CP.CapsuleProtocol (field "?0;flag") `shouldBe` Right (Just (CP.CapsuleProtocol False))
  CP.capsuleProtocolInUse enabled `shouldBe` True
  lookupRawHeader "Capsule-Protocol" (setHeader (CP.CapsuleProtocol False) empty) `shouldBe` Just ("?0" :| [])
  mapM_
    ( \headers -> do
        lookupHeader @CP.CapsuleProtocol headers `shouldSatisfy` isLeft
        CP.capsuleProtocolInUse headers `shouldBe` False
    )
    (repeated : map field ["?1, ?1", "1", "?2", "(?1)", "?1;\tflag", "?1;flag=", "?1;bad=\"\\q\""])
  CP.capsuleProtocolInUse empty `shouldBe` False
  CP.capsuleProtocolInUse (field "?0") `shouldBe` False


tests :: Spec
tests =
  describe "CapsuleProtocol" $
    sequence_
      [ unit_parse_true
      , unit_parse_false
      , unit_render
      , it "round-trips" prop_roundtrip
      , unit_capsule_fields
      ]
