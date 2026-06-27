{-# LANGUAGE OverloadedStrings #-}

module Test.Hermes.WebPush (tests) where

import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.TTL as TTL
import qualified Network.HTTP.Headers.Topic as Topic
import qualified Network.HTTP.Headers.Urgency as Urgency
import Test.Syd
import Test.Syd.Hedgehog ()


-- TTL -----------------------------------------------------------------------

parseTTL :: ByteString -> Either String TTL.TTL
parseTTL bs = case runParser TTL.ttlParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderTTL :: TTL.TTL -> ByteString
renderTTL = M.toStrictByteString . TTL.renderTTL


unit_ttl_parse :: Spec
unit_ttl_parse =
  it "parses a delta-seconds value" $
    parseTTL "3600" `shouldBe` Right (TTL.TTL 3600)


unit_ttl_render :: Spec
unit_ttl_render =
  it "renders a delta-seconds value" $
    renderTTL (TTL.TTL 0) `shouldBe` "0"


ttlGen :: Gen TTL.TTL
ttlGen = TTL.TTL <$> Gen.word (Range.linear 0 4_294_967_295)


prop_ttl_roundtrip :: Property
prop_ttl_roundtrip = property $ do
  v <- forAll ttlGen
  let bs = renderTTL v
  case parseTTL bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Topic ---------------------------------------------------------------------

parseTopic :: ByteString -> Either String Topic.Topic
parseTopic bs = case runParser Topic.topicParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderTopic :: Topic.Topic -> ByteString
renderTopic = M.toStrictByteString . Topic.renderTopic


unit_topic_parse :: Spec
unit_topic_parse =
  it "parses a url-safe base64 topic" $
    parseTopic "upd-42_v1" `shouldBe` Right (Topic.Topic (ST.fromString "upd-42_v1"))


unit_topic_render :: Spec
unit_topic_render =
  it "renders a topic verbatim" $
    renderTopic (Topic.Topic (ST.fromString "abcXYZ09-_")) `shouldBe` "abcXYZ09-_"


topicCharGen :: Gen Char
topicCharGen = Gen.element (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "-_")


topicGen :: Gen Topic.Topic
topicGen = Topic.Topic . ST.fromString <$> Gen.list (Range.linear 1 32) topicCharGen


prop_topic_roundtrip :: Property
prop_topic_roundtrip = property $ do
  v <- forAll topicGen
  let bs = renderTopic v
  case parseTopic bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


-- Urgency -------------------------------------------------------------------

parseUrgency :: ByteString -> Either String Urgency.Urgency
parseUrgency bs = case runParser Urgency.urgencyParser bs of
  OK v "" -> Right v
  OK _ rest -> Left ("unconsumed: " <> show rest)
  Fail -> Left "parse failed"
  Err err -> Left err


renderUrgency :: Urgency.Urgency -> ByteString
renderUrgency = M.toStrictByteString . Urgency.renderUrgency


unit_urgency_parse :: Spec
unit_urgency_parse = it "parses the urgency enum values" $ do
  parseUrgency "very-low" `shouldBe` Right Urgency.VeryLow
  parseUrgency "high" `shouldBe` Right Urgency.High


unit_urgency_render :: Spec
unit_urgency_render = it "renders the urgency enum values" $ do
  renderUrgency Urgency.VeryLow `shouldBe` "very-low"
  renderUrgency Urgency.Normal `shouldBe` "normal"


urgencyGen :: Gen Urgency.Urgency
urgencyGen = Gen.element [minBound .. maxBound]


prop_urgency_roundtrip :: Property
prop_urgency_roundtrip = property $ do
  v <- forAll urgencyGen
  let bs = renderUrgency v
  case parseUrgency bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)


tests :: Spec
tests =
  describe "WebPush" $
    sequence_
      [ unit_ttl_parse
      , unit_ttl_render
      , it "TTL round-trip" prop_ttl_roundtrip
      , unit_topic_parse
      , unit_topic_render
      , it "Topic round-trip" prop_topic_roundtrip
      , unit_urgency_parse
      , unit_urgency_render
      , it "Urgency round-trip" prop_urgency_roundtrip
      ]
